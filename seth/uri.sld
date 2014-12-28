(define-library (seth uri)
  (export uri-reference make-uri update-uri update-authority
          uri-reference? uri-auth uri-authority uri-scheme uri-path uri-query
          uri-fragment uri-host uri-port uri-username uri-password
          authority? authority-host authority-port
          authority-username authority-password

          uri? absolute-uri absolute-uri? uri->string uri->list
          relative-ref? uri-relative-to uri-relative-from
          uri-decode-string uri-encode-string
          uri-normalize-case uri-normalize-path-segments
          uri-path-absolute? uri-path-relative?

          char-set:gen-delims char-set:sub-delims
          char-set:uri-reserved char-set:uri-unreserved

          uri-equal? uri-auth-equal?

          uri->path-string)

  (import (scheme base)
          (scheme char)
          (scheme write))
  (cond-expand
   (chibi
    (import (chibi match)
            (chibi char-set)
            (chibi char-set ascii)
            (chibi optional)
            (only (srfi 1) concatenate every fold append-reverse)))
   (chicken
    ;; (import uri-generic)
    (import (only (chicken) let-optionals)
            (matchable)
            (ports)
            (srfi 1)
            (srfi 14))
    )
   (foment
    (import (srfi 1)
            (srfi 14)
            (chibi match)))
   (gauche
    (import (scheme cxr)
            (util match)
            (srfi 1)
            (srfi 14)))
   (kawa
    (import (srfi 1)
            (srfi 14)
            (chibi match)))
   (sagittarius
    (import (match)
            (sagittarius io)
            (srfi 1)
            (srfi 14))))
  (import (except (srfi 13)
                  string-copy string-map string-for-each
                  string-fill! string-copy! string->list
                  string-upcase string-downcase)
          (seth string-read-write))

  (begin

    (cond-expand
     (chicken
      (define (reverse-list->string char-list)
        (let* ((len (length char-list))
               (s (make-string len)))
          (let loop ((i (- len 1))
                     (char-list char-list))
            (cond ((null? char-list) s)
                  (else
                   (string-set! s i (car char-list))
                   (loop (- i 1)
                         (cdr char-list))))))))
     (else))


      (define (conc . args)
        (apply string-append (map display-to-string args)))


      (define (string-intersperse strs sep)
        (let loop ((strs strs)
                   (parts '()))
          (cond ((null? strs)
                 (string-concatenate (reverse parts)))
                ((null? (cdr strs))
                 (loop (cdr strs)
                       (cons (car strs) parts)))
                (else
                 (loop (cdr strs)
                       (cons sep (cons (car strs) parts)))))))


;; 
;; Definitions and parsing routines for Uniform Resource Identifiers (RFC 3986).
;; 
;; Based on the Haskell URI library by  Graham Klyne <gk@ninebynine.org>.
;;
;; Copyright 2008-2014 Ivan Raikov, Peter Bex, Seth Alves.
;;
;; 
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;  notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above
;;  copyright notice, this list of conditions and the following
;;  disclaimer in the documentation and/or other materials provided
;;  with the distribution.
;; 
;;  - Neither name of the copyright holders nor the names of its
;;  contributors may be used to endorse or promote products derived
;;  from this software without specific prior written permission.
;; 
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;  POSSIBILITY OF SUCH DAMAGE.
;;


(define uri-error error)



;; ---- ;;


;; What to do with these?
;; #;(cond-expand
;;    (utf8-strings (use utf8-srfi-13 utf8-srfi-14))
;;    (else (use srfi-13 srfi-14)))

(define-record-type <URI>
  (make-URI scheme authority path query fragment)
  URI?
  (scheme URI-scheme URI-scheme-set!)
  (authority URI-authority URI-authority-set!)
  (path URI-path URI-path-set!)
  (query URI-query URI-query-set!)
  (fragment URI-fragment URI-fragment-set!))
(define-record-type <URIAuth>
  (make-URIAuth username password host port)
  URIAuth?
  (username URIAuth-username URIAuth-username-set!)
  (password URIAuth-password URIAuth-password-set!)
  (host URIAuth-host URIAuth-host-set!)
  (port URIAuth-port URIAuth-port-set!))


(define (update-URI uri . args)
  (let loop ((args args)
             (new-scheme (URI-scheme uri))
             (new-authority (URI-authority uri))
             (new-path (URI-path uri))
             (new-query (URI-query uri))
             (new-fragment (URI-fragment uri)))
    (cond ((null? args)
           (make-URI new-scheme new-authority new-path new-query new-fragment))
          ((null? (cdr args))
           (uri-error "malformed arguments to update-URI"))
          (else
           (let ((key (car args))
                 (value (cadr args)))
             (loop (cddr args)
                   (if (eq? key 'scheme) value new-scheme)
                   (if (eq? key 'authority) value new-authority)
                   (if (eq? key 'path) value new-path)
                   (if (eq? key 'query) value new-query)
                   (if (eq? key 'fragment) value new-fragment)))))))


(define (update-URIAuth uri-auth . args)
  (let loop ((args args)
             (new-username (URIAuth-username uri-auth))
             (new-password (URIAuth-password uri-auth))
             (new-host (URIAuth-host uri-auth))
             (new-port (URIAuth-port uri-auth)))
    (cond ((null? args)
           (make-URIAuth new-username new-password new-host new-port))
          ((null? (cdr args))
           (uri-error "malformed arguments to update-URIAuth"))
          (else
           (let ((key (car args))
                 (value (cadr args)))
             (loop (cddr args)
                   (if (eq? key 'username) value new-username)
                   (if (eq? key 'password) value new-password)
                   (if (eq? key 'host) value new-host)
                   (if (eq? key 'port) value new-port)))))))


(define uri-reference? URI?)

(define uri-auth       URI-authority )
(define uri-authority  URI-authority )
(define uri-scheme     URI-scheme )
(define uri-path       URI-path )
(define uri-query      URI-query )
(define uri-fragment   URI-fragment )

(define (uri-host x)
  (let ((auth (URI-authority x)))
    (and auth (URIAuth-host auth))))

(define (uri-port x)
  (let ((auth (URI-authority x)))
    (and auth (URIAuth-port auth))))

(define (uri-username x)
  (let ((auth (URI-authority x)))
    (and auth (URIAuth-username auth))))

(define (uri-password x)
  (let ((auth (URI-authority x)))
    (and auth (URIAuth-password auth))))

(define authority? URIAuth?)
(define authority-host URIAuth-host)
(define authority-port URIAuth-port)
(define authority-username URIAuth-username)
(define authority-password URIAuth-password)

(define update-authority update-URIAuth)


(define update-uri*
  (let ((unset (list 'unset)))
    (lambda (uri . args)
      (let loop ((key/values args)
                 (scheme (URI-scheme uri))
                 (path (URI-path uri))
                 (query (URI-query uri))
                 (fragment (URI-fragment uri))
                 (auth unset)
                 (authority unset))
        (cond
         ((null? key/values)
        (let* ((base-auth (or
                           (cond
                            ((not (eq? unset auth)) auth)
                            ((not (eq? unset authority)) authority)
                            (else (URI-authority uri)))
                             (make-URIAuth #f #f #f #f)))
                 (updated-auth (apply update-authority base-auth args))
                 (final-auth (if (uri-auth-equal? (make-URIAuth #f #f #f #f)
                                                  updated-auth)
                               #f
                               updated-auth)))
            (make-URI scheme final-auth path query fragment)))
         ((null? (cdr key/values))
          (uri-error "malformed arguments to update-uri"))
         ((not (memq (car key/values)
                     '(scheme authority path query fragment
                              username password host port)))
          (uri-error "unknown argument to update-uri" (car key/values)))
         (else
          (let ((key (car key/values))
                (value (cadr key/values)))
            (loop (cddr key/values)
                  (if (eq? key 'scheme) value scheme)
                  (if (eq? key 'path) value path)
                  (if (eq? key 'query) value query)
                  (if (eq? key 'fragment) value fragment)
                  (if (eq? key 'auth) value auth)
                  (if (eq? key 'authority) value authority)))))))))


(cond-expand

 ;; (chicken-not-r7rs
 ;;  (define update-uri
 ;;    (let ((unset (list 'unset)))
 ;;      (lambda (uri . key/values)
 ;;        (apply
 ;;         (lambda (#!key
 ;;                  (scheme (URI-scheme uri)) (path (URI-path uri))
 ;;                  (query (URI-query uri)) (fragment (URI-fragment uri))
 ;;                  (auth unset) (authority unset)
 ;;                  (username unset) (password unset)
 ;;                  (host unset) (port unset))
 ;;           (let* ((args (list 'scheme scheme
 ;;                              'path path
 ;;                              'query query
 ;;                              'fragment fragment))
 ;;                  (args (if (not (eq? auth unset))
 ;;                            (append args (list 'auth auth)) args))
 ;;                  (args (if (not (eq? authority unset))
 ;;                            (append args (list 'authority authority)) args))
 ;;                  (args (if (not (eq? username unset))
 ;;                            (append args (list 'username username)) args))
 ;;                  (args (if (not (eq? password unset))
 ;;                            (append args (list 'password password)) args))
 ;;                  (args (if (not (eq? host unset))
 ;;                            (append args (list 'host host)) args))
 ;;                  (args (if (not (eq? port unset))
 ;;                            (append args (list 'port port)) args))
 ;;                  )
 ;;             (apply update-uri* uri args)))
 ;;         key/values)))))

 (else
  (define update-uri update-uri*)))


(define (make-uri* . key/values)
  (apply update-uri* (make-URI #f #f '() #f #f) key/values))

(cond-expand

 (chicken-not-r7rs
(define (make-uri . key/values)
    (apply update-uri (make-URI #f #f '() #f #f) key/values)))

 (else
  (define make-uri make-uri*)))


(define (uri-equal? a b)
  (or (and (not a) (not b))
      (and (equal? (URI-scheme a) (URI-scheme b))
           (uri-auth-equal? (URI-authority a) (URI-authority b))
           (equal? (URI-path a) (URI-path b))
           (equal? (URI-query a) (URI-query b))
           (equal? (URI-fragment a) (URI-fragment b)))))


(define (uri-auth-equal? a b)
  (or (and (not a) (not b))
      (and
       (equal? (URIAuth-username a) (URIAuth-username b))
       (equal? (URIAuth-password a) (URIAuth-password b))
       (equal? (URIAuth-host a) (URIAuth-host b))
       (equal? (URIAuth-port a) (URIAuth-port b)))))


;; Character classes
 
(define (hexdigit-char? c)    (and (char? c) (char-set-contains? char-set:hex-digit c)))

(define (unreserved-char? c)  (and (char? c) (char-set-contains? char-set:uri-unreserved c)))

(define (scheme-char? c)      (and (char? c) (char-set-contains? char-set:scheme c)))

(define (ipv-future-char? c)  (and (char? c) (char-set-contains? char-set:ipv-future c)))

(define (alpha-char? c)       (and (char? c) (char-set-contains? char-set:letter c)))

(define (pct-encoded? c)      (match c ((#\% h1 h2) (and (hexdigit-char? h1) (hexdigit-char? h2)))
				     (else #f)))


;; Helper functions for character parsing
  
(define (uchar extras)
  (let ((extras-set (or (and (char-set? extras) extras) (string->char-set extras))))
    (lambda (c) (or (pct-encoded? c) (unreserved-char? c) 
		    (char-set-contains? char-set:sub-delims c) 
		    (char-set-contains? extras-set c) ))))
  
(define (many pred?)
  (lambda (s)
    (let loop ((lst (list)) (rst s))
      (cond ((null? rst)        (list (reverse lst) rst))
	    ((pred? (car rst))  (loop (cons (car rst) lst) (cdr rst)))
	    (else               (list (reverse lst) rst))))))

(define (many1 pred?)
  (lambda (s)
    (let ((a1 (and (not (null? s)) (pred? (car s)) (car s))))
      (and a1 (match ((many pred?) (cdr s))
		     ((as rst)  (list (cons a1 as) rst))
		     (else #f))))))


(define (count-min-max m n pred?)
  (lambda (s) 
    (let loop ((m m) (n n) (lst (list)) (rst s))
      (cond ((and (pair? rst) (positive? m))
	     (if (pred? (car rst))
		 (loop (- m 1) (- n 1) (cons (car rst) lst) (cdr rst)) #f))
	    ((or (<= n 0) (null? rst))   (list (reverse lst) rst))
	    (else 
	     (if (pred? (car rst))
		 (loop 0 (- n 1) (cons (car rst) lst) (cdr rst))
		 (list (reverse lst) rst)))))))

;; Parser combinators

(define (consume f) 
  (lambda (s)
    (let loop ((lst (list)) (rst s))
      (match (f rst)
	     ((a rst)  (loop (cons a lst) rst))
	     (else  (list (reverse lst) rst))))))


(define (consume-count n f)
  (lambda (s)
    (let loop ((n n) (lst (list)) (rst s))
      (if (positive? n)
	  (match (or (f rst) (list #f s))
		 ((x rst)  (and x (loop (- n 1) (cons x lst) rst))))
	  (list (reverse lst) rst)))))


(define (consume-min-max m n f)
  (lambda (s) 
    (let loop ((m m) (n n) (lst (list)) (rst s))
      (cond ((positive? m)
	     (match (f rst)
		    ((a1 rst) (loop (- m 1) (- n 1) (cons a1 lst) rst))
		    (else #f)))
	    ((<= n 0)   (list (reverse lst) rst))
	    (else 
	     (match (f rst)
		    ((a1 rst) (loop 0 (- n 1) (cons a1 lst) rst))
		    (else #f)))))))

;; Helper function for malformed ip address error messages

(define (try-ip-literal->string s)
  (let loop ((lst (list))  (rst s))
    (match rst ((#\] . rst)  (uri-char-list->string (reverse lst)))
	   (()  (uri-char-list->string (reverse lst)))
	   (else (loop (cons (car rst) lst) (cdr rst))))))

;; RFC 3986, section 2.1
;;
;; Returns a 'pct-encoded' sequence of octets.
;;
(define (pct-encode char-list char-set)
  (define (hex-digit i)
    (and (>= i 0) (< i 16)
         (car (string->list (string-upcase (number->string i 16))))))
  (reverse (fold (lambda (c cl)
                   (if (char-set-contains? char-set c)
                       (let* ((x (char->integer c))
                              (h1 (hex-digit (quotient x 16)))
                              (h2 (hex-digit (remainder x 16))))
                         (cons `(#\% ,h1 ,h2) cl))
                       (cons c cl)))
		 (list) char-list)))

;; Inverse operation: 'pct-decode' a sequence of octets.

(define (pct-decode char-list char-set)
  (define (octet-decode h1 h2)
    (string->number (list->string (list h1 h2)) 16))
  (map (lambda (c)
	 (match c
		((#\% h1 h2) 
		 (let ((dc (integer->char (octet-decode h1 h2))))
		   (if (char-set-contains? char-set dc) dc c)))
		(else c)))
       char-list))


;; RFC3986, section 2.2 
;;
;; Reserved characters.
;;

(define char-set:gen-delims (string->char-set ":/?#[]@"))
(define char-set:sub-delims (string->char-set "!$&'()*+,;="))

(define char-set:uri-reserved (char-set-union char-set:gen-delims char-set:sub-delims))

;;  RFC3986, section 2.3
;;
;;  "Unreserved" characters.
;;

(define char-set:uri-unreserved 
  (char-set-union char-set:letter+digit (string->char-set "-_.~")))



;;  RFC3986, section 3
;;
;;   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
;;
;;   hier-part   = "//" authority path-abempty
;;               / path-abs
;;               / path-rootless
;;               / path-empty

;; TODO: Export a modified version of this one, to match absolute-uri
;;       (modified = throw an error instead of #f)
(define (uri s)
  (let ((s (if (string? s) (uri-string->normalized-char-list s) s)))
    (and s (match (scheme s)
		  ((us rst)
		   (match-let* (((ua up rst)   (hier-part rst))
				((uq rst)      (match rst ((#\? . rst) (query rst))
						      (else (list #f rst))))
				((uf rst)      (match rst ((#\# . rst) (fragment rst))
						      (else (list #f rst)))))
			       (and (null? rst)
                        (make-URI (string->symbol (list->string us))
                                  ua
                                  (uri-path-list->path up)
                                  (and uq (uri-char-list->string uq))
                                  (and uf (uri-char-list->string uf))))))
		  (else #f)))))

(define (uri? u)
  (and (uri-reference? u) (uri-scheme u) #t))

(define (uri-path-list->path pcl)
  (let ((cs (char-set-union char-set:uri-unreserved (char-set #\/))))
    (match pcl
           (('/ . rst) (cons '/ (map (lambda (c)
                                       (uri-char-list->string (pct-decode c cs)))
                                     rst)))
           (else (map (lambda (c)
                        (uri-char-list->string (pct-decode c cs)))
                      pcl)))))

(define (hier-part s)
  (match s ((#\/ #\/ . rst) 
	    (match-let* (((ua rst)  (authority rst))
			 ((up rst)  (path-abempty rst)))
			(list ua up rst)))
	 (else (match-let (((up rst) (or (path-abs s) (path-rootless s) (list '() s))))
			  (list #f up rst)))))

;;  RFC3986, section 3.1

(define scheme0 (many scheme-char?))
(define (scheme s)
  (match s
	 (((and s0 (? alpha-char?)) . rst)
          (match (scheme0 rst)
                 ((ss (#\: . rst))  (list (cons s0 ss) rst))
                 (else #f)))
	 (else #f)))

(define char-set:scheme
  (char-set-union char-set:letter+digit (string->char-set "+-.")))


;;  RFC3986, section 3.2
;;
;;     authority     = [ userinfo "@" ] host [ ":" port ]

(define (authority s)
  (match-let* (((uu uw rst)   (or (userinfo s) (list #f #f s)))
	       ((uh rst)      (host rst))
	       ((up rst)      (or (port rst) (list #f rst))))
              (list
               (make-URIAuth
                (and uu (uri-char-list->string uu))
                (and uw (uri-char-list->string uw))
                (uri-char-list->string uh)
                (and (pair? up) (string->number (list->string up))))
		    rst)))

;;  RFC3986, section 3.2.1
;;
;;     userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
;;
;; We split this up in the leading part without colons ("username") and
;; everything after that ("password"), including extra colons.
;;
;; The RFC is not very clear, but it does mention this:
;;   "The userinfo subcomponent may consist of a user name and,
;;    optionally, scheme-specific information about how to gain
;;    authorization to access the resource."
;;
;; The grammar allows multiple colons, and the RFC then continues:
;;   "Applications should not render as clear text any data after
;;    the first colon (":") character found within a userinfo
;;    subcomponent unless the data after the colon is the empty
;;    string (indicating no password)."

(define userinfo0  (many (uchar ";&=+$,")))
(define userinfo1  (many (uchar ";&=+$,:")))

(define (userinfo s)
  (match (userinfo0 s)
	 ((uu ( #\: . rst))   (match (userinfo1 rst)
				     ((up ( #\@ . rst) ) (list uu up rst))
				     (else #f)))
	 ((uu ( #\@ . rst)) (list uu #f rst))
	 (else #f)))


;;  RFC3986, section 3.2.2
;;
;;     host          = IP-literal / IPv4address / reg-name
;;     IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
;;     IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )

(define (host s)  (or (ip-literal s) (ipv4-address s) (reg-name s)))

(define (ip-literal s)
  (match s ((#\[ . rst) 
	    (match (or (ipv6-address rst) (ipv-future rst))
		   ((ua (#\] . rst))  (list ua rst))
                   (else (uri-error 'ip-literal "malformed ip literal"
                                (try-ip-literal->string rst)))))
	 (else #f)))

(define ipv-future0  (many ipv-future-char?))

(define (ipv-future s)
  (match s ((#\v (? hexdigit-char?) #\. . rst)  (ipv-future0 rst))
	 (else #f)))

(define char-set:ipv-future 
  (char-set-union char-set:uri-unreserved char-set:sub-delims (char-set #\;)))



;; IPv6address =                                  6( h16 ":" ) ls32
;;                   /                       "::" 5( h16 ":" ) ls32
;;                   / [               h16 ] "::" 4( h16 ":" ) ls32
;;                   / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
;;                   / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
;;                   / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
;;                   / [ *4( h16 ":" ) h16 ] "::"              ls32
;;                   / [ *5( h16 ":" ) h16 ] "::"              h16
;;                   / [ *6( h16 ":" ) h16 ] "::"

;;       ls32        = ( h16 ":" h16 ) / IPv4address
;;                   ; least-significant 32 bits of address

;;       h16         = 1*4HEXDIG
;;                   ; 16 bits of address represented in hexadecimal


(define (ipv6-address s)
  (or (match (u6-h4c s) ;; 6( h16 ":" ) ls32

	     ((a2 rst)  (match (ls32 rst)
			       ((a3 rst)  (list (append (concatenate a2) a3) rst))
			       (else #f)))
	     (else #f))
      (match s          ;; "::" 5( h16 ":" ) ls32
	     ((#\: #\: . rst)  
	      (match (u5-h4c rst)
		     ((a2 rst)  (match (ls32 rst)
				       ((a3 rst)  (list (append (list #\: #\:) (concatenate a2) a3) rst))
				       (else #f)))))
	     (else #f))
      (match (u_opt_n_h4c_h4 0 s)
	     ((a1 rst) (match rst
			      ((#\: #\: . rst)  
			       (match (u4-h4c rst)
				      ((a2 rst)  (match (ls32 rst)
							((a3 rst)  
							 (list (append (concatenate a1) (list #\: #\:) 
								       (concatenate a2) a3) rst))
							(else #f)))
				      (else #f)
				      ))
			      (else #f)))
	      (else #f))
      (match (u_opt_n_h4c_h4 1 s)
	     ((a1 rst) 
        	      (match rst       
			      ((#\: #\: . rst)  
			       (match (u3-h4c rst)
				      ((a2 rst)  (match (ls32 rst)
							((a3 rst)  
							 (list (append (concatenate a1) (list #\: #\:) 
								       (concatenate a2) a3) rst))
							(else #f)))
				      (else #f)
				      ))
			      (else #f)))
	      (else #f))
      (match (u_opt_n_h4c_h4 2 s)
	     ((a1 rst) (match rst       
			      ((#\: #\: . rst)  
			       (match (u2-h4c rst)
				      ((a2 rst)  (match (ls32 rst)
							((a3 rst)  (list (append (concatenate a1) (list #\: #\:) 
										 (concatenate a2) a3) rst))
							(else #f)))
				      (else #f)
				      ))
			      (else #f)))
	      (else #f))
      (match (u_opt_n_h4c_h4 3 s)
	     ((a1 rst) (match rst       
			      ((#\: #\: . rst)  
			       (match (h4c rst)
				      ((a2 rst)  (match (ls32 rst)
							((a3 rst)  (list (append (concatenate a1) (list #\: #\:) 
										 (concatenate a2) a3) rst))
							(else #f)))
				      (else #f)
				      ))
			      (else #f)))
	      (else #f))
      (match (u_opt_n_h4c_h4 4 s)
	     ((a1 rst) (match rst       
			      ((#\: #\: . rst)  
			       (match (ls32 rst)
				      ((a3 rst)  (list (append (concatenate a1) (list #\: #\:) a3) rst))
				      (else #f)))
			      (else #f)))
	      (else #f))
      (match (u_opt_n_h4c_h4 5 s)
	     ((a1 rst) (match rst       
			      ((#\: #\: . rst)  
			       (match (h4 rst)
				      ((a3 rst)  (list (append (concatenate a1) (list #\: #\:) a3) rst))
				      (else #f)))
			      (else #f)))
	      (else #f))
      (match (u_opt_n_h4c_h4 6 s)
	     ((a1 rst) (match rst       
			      ((#\: #\: . rst)  
			       (list (append (concatenate a1) (list #\: #\:)) rst))
			      (else #f)))
	      (else #f))
      (uri-error 'ipv6-address "malformed ipv6 address" (try-ip-literal->string s))))



(define (u_opt_n_h4c_h4 n s)
  (match ((consume-min-max 0 n h4c) s)
	 ((a1 rst)  (match (h4 rst)
			   ((a2 rst) (list (append a1 (list a2)) rst))
			   (else #f)))
	 (else #f)))

(define (ls32 s)
  (match (h4c s)
	 ((a1 rst) (match (h4 rst)
			  ((a2 rst)  (list (append a1 a2) rst))
			  (else (ipv4-address s))))
	 (else (ipv4-address s))))

(define (h4c s)
  (match (h4 s)
	 ((a1 (#\: (and r1 (not #\:)) . rst))
	  (list (append a1 (list #\:)) (cons r1 rst)))
	 (else #f)))

(define u6-h4c (consume-count 6 h4c))
(define u5-h4c (consume-count 5 h4c))
(define u4-h4c (consume-count 4 h4c))
(define u3-h4c (consume-count 3 h4c))
(define u2-h4c (consume-count 2 h4c))

(define h4 (count-min-max 1 4 hexdigit-char?))

(define (ipv4-address s)
  (match (dec-octet s)
	 ((a1 (#\. . rst))
	  (match (dec-octet rst)
		 ((a2 (#\. . rst))
		  (match (dec-octet rst)
			 ((a3 (#\. . rst))
			  (match (dec-octet rst)
				 ((a4 rst)  (list (list a1 #\. a2 #\. a3 #\. a4) rst))
				 (else #f)))
			 (else #f)))
		 (else #f)))
	 (else #f)))

(define (ipv4-octet? lst)
  (and (every (lambda (x) (char-set-contains? char-set:digit x)) lst)
       (let ((num (string->number (list->string lst))))
         (and num (>= num 0) (<= num 255)))))

(define (dec-octet s)
  (match ((count-min-max 1 3 (lambda (c) (and (char? c) (char-numeric? c)))) s)
	 (((and a1 (? ipv4-octet?)) rst)  (list a1 rst))
	 (else #f)))

(define reg-name
  (count-min-max 0 255 (lambda (c) (or (pct-encoded? c) 
				       (unreserved-char? c) 
				       (char-set-contains? char-set:sub-delims c) ))))

;;  RFC3986, section 3.2.3
;;
;;     port          = *DIGIT

(define port0 (many char-numeric?))

(define (port s)
  (match s ((#\: . rst)  (port0 rst))
	 (else #f)))


;;
;;  RFC3986, section 3.3
;;
;;   path          = path-abempty    ; begins with "/" or is empty
;;                 / path-abs        ; begins with "/" but not "//"
;;                 / path-noscheme   ; begins with a non-colon segment
;;                 / path-rootless   ; begins with a segment
;;                 / path-empty      ; zero characters
;;
;;  oddly, "path" is never used in the grammar. The following are used
;;  in "hier-part" and "relative-ref", however:
;;
;;   path-abempty  = *( "/" segment )
;;   path-abs      = "/" [ segment-nz *( "/" segment ) ]
;;   path-noscheme = segment-nzc *( "/" segment )
;;   path-rootless = segment-nz *( "/" segment )
;;   path-empty    = 0<pchar>
;;
;;   segment       = *pchar
;;   segment-nz    = 1*pchar
;;   segment-nzc   = 1*( unreserved / pct-encoded / sub-delims / "@" )
;;
;;   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

(define (slash-segment s)
  (match s
	 ((#\/ . rst)
          (match (segment rst)
            ((ss rst)  (list ss rst))
            (else #f)))
	 (else  #f)))

(define pchar (uchar ":@"))

(define segment (many pchar))

(define segment-nz (many1 pchar))

(define segment-nzc (many1 (uchar "@")))

(define (path-abempty s)
  (match ((consume slash-segment) s)
         ((() rst)    (list (list) rst))
         ((path rst)  (list (cons '/ path) rst))))

(define (path-abs s)
  (match s
	 ((#\/)          (list (list '/ (list))  (list)))
	 ((#\/ . rst)    (match (path-rootless rst) ; optional
				((lst rst) (list (cons '/ lst) rst))
				(else (list (list '/ (list)) rst))))
	 (else #f)))

(define (path-noscheme s)
  (match (segment-nzc s)
	 ((s1 rst)  (match ((consume slash-segment) rst)
			   ((ss rst) (list (cons s1 ss) rst))))
	 (else #f)))

(define (path-rootless s)
  (match (segment-nz s)
	 ((s1 rst)  (match ((consume slash-segment) rst)
			   ((ss rst) (list (cons s1 ss) rst))))
	 (else #f)))

;;  RFC3986, section 3.4
;;
;;   query         = *( pchar / "/" / "?" )

(define query0  (many (uchar ":@/?")))
(define (query s)
  (match (query0 s)
	 ((ss rst)  (list ss rst))
	 (else #f)))

;;  RFC3986, section 3.5
;;   fragment         = *( pchar / "/" / "?" )

(define fragment0  (many (uchar ":@/?")))
(define (fragment s)
  (match (fragment0 s)
	 ((ss rst)  (list ss rst))
	 (else #f)))

;;  Reference, Relative and Absolute URI forms
;;
;;  RFC3986, section 4.1

(define (uri-reference s)
  (let ((s (if (string? s) (uri-string->normalized-char-list s) s)))
    (and s (or (uri s) (relative-ref s)))))

;; (define uri-reference? URI) ; Already defined as URI? (struct predicate)

;;  RFC3986, section 4.2
;;
;;   relative-URI  = relative-part [ "?" query ] [ "#" fragment ]
;;
;;   relative-part = "//" authority path-abempty
;;                 / path-abs
;;                 / path-noscheme
;;                 / path-empty

;; TODO: Export a modified version of this  (one that accepts a string
;;       and throws an exception instead of returning #f)
(define (relative-ref s)
  (and (not (scheme s))
       (match-let* (((ua up rst)  (relative-part s))
		    ((uq rst)     (match rst ((#\? . rst) (query rst))
					 (else (list #f rst))))
		    ((uf rst)     (match rst ((#\# . rst) (fragment rst))
					 (else (list #f rst)))))
		   (and (null? rst)
                        (make-URI #f ua
                                  (uri-path-list->path up)
                                  (and uq (uri-char-list->string uq))
                                  (and uf (uri-char-list->string uf)))))))

(define (relative-ref? u)
  (and (uri-reference? u) (not (uri-scheme u))))

(define (relative-part s)
  (match s
	 ((#\/ #\/ . rst)
	  (match-let* (((ua rst)  (authority rst))
		       ((up rst)  (path-abempty rst)))
		      (list ua up rst)))
	 (else (match-let* (((up rst)  (or (path-abs s) (path-noscheme s) (list (list) s))))
			   (list #f up rst))))) 



;;  RFC3986, section 4.3

(define (absolute-uri s)
  (let ((s (if (string? s) (uri-string->normalized-char-list s) s)))
    (and s (match (scheme s)
		  ((us rst)  
		   (match-let* (((ua up rst)  (hier-part rst))
				((uq rst)     (match rst ((#\? . rst)  (query rst))
						     (else (list #f rst)))))
			       (match rst
                      ((#\# . rst) (uri-error 'absolute-uri "fragments are not permitted in absolute URI"))
                      (else (make-URI (string->symbol (list->string us))
                                      ua
                                      (uri-path-list->path up)
                                      (and uq (uri-char-list->string uq))
                                      #f)))))
          (else (uri-error 'absolute-uri "no scheme found in URI string"))))))

(define (absolute-uri? u)
  (and (uri-reference? u) (not (relative-ref? u)) (not (uri-fragment u))))

;; Turns a URI into a string.
;;
;; Uses a supplied function to map the userinfo part of the URI.
;;



(define (uri->string uri . maybe-userinfomap)
  (let ((userinfomap (if (pair? maybe-userinfomap)
                         (car maybe-userinfomap)
                         (lambda (u pw)
                           (string-append u ":******" )))))
    (cond ((URI? uri)
	    (with-output-to-string
	      (lambda ()
               (let ((scheme (URI-scheme uri))
                     (authority (URI-authority uri))
                     (path (URI-path uri))
                     (query (URI-query uri))
                     (fragment (URI-fragment uri)))
		(display-fragments
                  (list
                   (and scheme (list scheme ":"))
                   (and (URIAuth? authority)
                        (string? (URIAuth-host authority))
                        (let ((username (URIAuth-username authority))
                              (password (URIAuth-password authority))
                              (host (URIAuth-host authority))
                              (port (URIAuth-port authority)))
                          (list "//" (and username (list (userinfomap
                                                          username
                                                          password) "@"))
                                host (and port (list ":" port)))))
                   (path->string path)
                   (and query (list "?" query))
                   (and fragment (list  "#" fragment))))))))
	   (else #f))))



(define (display-fragments b)
  (let loop ((fragments b))
    (cond
      ((null? fragments) (begin #t))
      ((not (car fragments)) 
       (loop (cdr fragments) ))
      ((null? (car fragments)) 
       (loop (cdr fragments) ))
      ((pair? (car fragments))
       (begin (loop (car fragments))
	      (loop (cdr fragments) )))
      (else
       (display (car fragments))
       (loop (cdr fragments) )))))

			 
(define (path->string path)
  (match path
         (('/ . segments)     (string-append "/" (join-segments segments)))
         (((? protect?) . _)  (join-segments (cons "." path)))
         (else                (join-segments path))))

(define (join-segments segments)
  (string-intersperse
   (map (lambda (segment)
          (uri-encode-string segment (char-set #\/)))
        segments) "/"))

;; Section 4.2; if the first segment contains a colon, it must be prefixed "./"
(define (protect? sa) (string-index sa #\:))

; specific: ((uri-authority uri) (uri-path uri) (uri-query uri)).

(define (uri->list uri . maybe-userinfomap)
  (let ((userinfomap (if (pair? maybe-userinfomap)
                         (car maybe-userinfomap)
                         (lambda (u pw)
                           (string-append u ":******" )))))
    (cond ((URI? uri)
           `(,(URI-scheme uri)
             (,(uri-auth->list (URI-authority uri) userinfomap)
              ,(URI-path uri) ,(URI-query uri))
             ,(URI-fragment uri)))
	   (else #f))))

(define (uri-auth->list uri-auth userinfomap)
  (cond ((URIAuth? uri-auth)
         `(,(and (URIAuth-username uri-auth) (URIAuth-password uri-auth)
                 (userinfomap (URIAuth-username uri-auth)
                              (URIAuth-password uri-auth)))
           ,(URIAuth-host uri-auth)
           ,(URIAuth-port uri-auth)))
	 (else #f)))
			 

;;  Percent encoding and decoding

(define (uri-encode-string str . maybe-char-set)
  (let ((char-set (if (pair? maybe-char-set)
                      (car maybe-char-set)
                      (char-set-complement char-set:uri-unreserved)))
        (clst (string->list str)))
    (uri-char-list->string
     (pct-encode clst char-set))))

(define (uri-decode-string str . maybe-char-set)
  (let ((char-set (if (pair? maybe-char-set)
                      (car maybe-char-set)
                      char-set:full))
        (str1 (uri-string->char-list str)))
    (and str1 (uri-char-list->string (pct-decode str1 char-set)))))
    
(define (uri-string->normalized-char-list str)
  (let ((str1 (uri-string->char-list str)))
    (and str1 (pct-decode str1 char-set:uri-unreserved))))

;; Convert a URI character list to a string

(define (uri-char-list->string s)
  (reverse-list->string 
   (fold (lambda (x ax)
           (cond ((char? x) (cons x ax))
                 ((list? x) (append-reverse x ax)))) (list) s)))
   
;; Convert a string to a URI character list 

(define (uri-string->char-list s)
  (let loop ((cs (list)) (lst (string->list s)))
    (if (null? lst) (reverse cs)
	(match lst
	       ((#\% h1 h2 . rst)  (and (hexdigit-char? h1) (hexdigit-char? h2)
					(loop (cons (list #\% h1 h2) cs) rst)))
	       (((and c (? char?)) . rst)  (loop (cons c cs) rst))))))
   
;;
;;  Resolving a relative URI relative to a base URI
;;
;;  Returns a new URI which represents the value of the first URI
;;  interpreted as relative to the second URI.
;;
;;  For example:
;;
;;  (uri->string (relative-to (uri-reference "foo") (uri "http://bar.org/")) )
;;         => "http://bar.org/foo"
;;
;;  Algorithm from RFC3986, section 5.2.2
;;

(define (uri-relative-to ref base)
  (and (uri-reference? ref) (uri-reference? base)
       (cond ((uri-scheme ref)
              (update-URI ref 'path (just-segments ref)))
             ((uri-authority ref)
              (update-URI ref
                          'path (just-segments ref)
                          'scheme (uri-scheme base)))
             ((let ((p (uri-path ref))) (and (not (null? p)) p)) =>
              (lambda (ref-path)
                (if (and (pair? ref-path) (eq? '/ (car ref-path)))
                    (update-URI ref
                                'scheme (uri-scheme base)
                                'authority (uri-auth base)
                                'path (just-segments ref))
                    (update-URI ref
                                'scheme (uri-scheme base)
                                'authority (uri-auth base)
                                'path (merge-paths base ref-path)))))
             ((uri-query ref)
              (update-URI ref
                          'scheme (uri-scheme base)
                          'authority (uri-auth base)
                          'path (merge-paths base (list ""))))
             (else (update-URI ref
                               'path (URI-path base)
                               'scheme (URI-scheme base)
                               'authority (URI-authority base)
                               'query (URI-query base))))))

(define (just-segments u)
  (remove-dot-segments (uri-path u)))

(define (merge0 pb pr)
  (let* ((rpb  (reverse pb))
	 (pb1  (reverse (match rpb      ; RFC3986, section 5.2.3, second bullet
                               ((_ . rst) rst)
                               (else rpb)))))
    (append pb1 pr))) ; It is assumed we never get here if pr is empty!

(define (merge-paths b pr)  ; pr is a relative path, *not* a URI object
  (let ((ba (uri-authority b))
	(pb (uri-path b)))
    (let ((mp (if (and ba (null? pb))
                  (cons '/ pr)  ; RFC3986, section 5.2.3, first bullet
                  (merge0 pb pr))))
      (remove-dot-segments mp))))

;;  Remove dot segments, but protect leading '/' symbol
(define (remove-dot-segments ps)
  (match ps
         (('/ . rst)   (cons '/ (elim-dots rst)))
	 (else         (elim-dots ps))))

(define (elim-dots ps)
  (let loop ((ps ps) (trailing-slash? #f) (lst (list)))
    (if (null? ps) (reverse (if trailing-slash? (cons "" lst) lst))
	(match ps
	       (("." . rst)
		(loop rst #t lst))
	       ((".." . rst)
		(loop rst #t (if (pair? lst) (cdr lst) lst)))
	       ((x . rst)
                (loop rst #f (cons x lst)))))))

;;
;; Finding a URI relative to a base URI
;;
;; Returns a new URI which represents the relative location of the
;; first URI with respect to the second URI.  Thus, the values
;; supplied are expected to be absolute URIs, and the result returned
;; may be a relative URI.
;;
;; Example:
;;
;; (uri->string
;;  (uri-relative-from (uri "http://example.com/Root/sub1/name2#frag")
;;                     (uri "http://example.com/Root/sub2/name2#frag")))
;;    ==> "../sub1/name2#frag"
;;


(define (uri-relative-from uabs base)
  (cond ((ucdiff? uri-scheme uabs base)      (update-URI uabs))
        ((ucdiff? uri-authority uabs base) (update-URI uabs 'scheme #f))
        ;; Special case: no relative representation for http://a/ -> http://a
        ;; ....unless that should be a path of ("..")
        ((null? (uri-path uabs))
         (update-URI uabs 'scheme #f))
	((ucdiff? uri-path uabs base)
	 (update-URI uabs
                     'scheme #f
                     'authority #f
                     'path (rel-path-from
			    (remove-dot-segments (uri-path uabs))
			    (remove-dot-segments (uri-path base)))))
	((ucdiff? uri-query uabs base)
	 (update-URI uabs
                     'scheme #f
                     'authority #f
                     'path (list)))
	(else
	 (update-URI uabs
                     'scheme #f
                     'authority #f
                     'query #f
                     'path (list)))))

(define (ucdiff? sel u1 u2)
  (let ((s1 (sel u1))
	(s2 (sel u2)))
    (not (cond ((and (URIAuth? s1) (URIAuth? s2))
		(not (or (ucdiff? uri-username u1 u2) (ucdiff? uri-host u1 u2) (ucdiff? uri-port u1 u2))))
	       ((and (list? s1) (list? s2))       (equal? s1 s2))
	       ((and (string? s1) (string? s2))   (string=? s1 s2))
	       (else                              (eq? s1 s2))))))

(define (rel-path-from pabs base)
  (match (list pabs base)
         ((pabs ()) pabs)
         ((() base) (list))
         ;; Construct a relative path segment if the paths share a
         ;; leading segment other than a leading '/'
         ((('/ . (and sa1 (ra1 . ra2))) ('/ . (and sb1 (rb1 . rb2))))
          (make-rel-path
           (if (string=? ra1 rb1)
               (rel-path-from1 sa1 sb1)
               pabs)))
         (else (uri-error 'rel-path-from "Both URI paths must be absolute" pabs base))))

(define (make-rel-path x)
  (match x
         ((or ('/ . rst) ("." . rst) (".." . rst)) x)
         (else (cons "." x))))

;;  rel-path-from1 strips off trailing names from the supplied paths,

(define (rel-path-from1 pabs base)
  (match-let* (((na . sa)  (reverse pabs))  
	       ((nb . sb)  (reverse base)))
     (let ((rp (rel-segs-from (reverse sa) (reverse sb))))
       (if (null? rp)  (cond ((string=? na nb)  (list))
			     (else              (list na)))
	   (append rp (list na))))))

			  
;;  rel-segs-from discards any common leading segments from both paths,
;;  then invokes dif-segs-from to calculate a relative path from the end
;;  of the base path to the end of the target path.  The final name is
;;  handled separately, so this deals only with "directory" segments.

(define (rel-segs-from sabs base)
  (cond ((and (null? sabs) (null? base))  (list))
	((or (null? sabs) (null? base))   (dif-segs-from sabs base))
	(else (match-let (((sa1 . ra1) sabs)
			  ((sb1 . rb1) base))
			 (if (string=? sa1 sb1)
			     (rel-segs-from ra1 rb1)
			     (dif-segs-from sabs base))))))

;;  dif-segs-from calculates a path difference from base to target,
;;  not including the final name at the end of the path (i.e. results
;;  always ends with '/')
;;
;;  This function operates under the invariant that the supplied value
;;  of sabs is the desired path relative to the beginning of base.
;;  Thus, when base is empty, the desired path has been found.

(define (dif-segs-from sabs base)
  (if (null? base) sabs (dif-segs-from (cons ".." sabs)  (cdr base))))


;; Other normalization functions
;;
;; Case normalization; cf. RFC3986 section 6.2.2.1

(define (uri-normalize-case uri)
  (let* ((normalized-uri (uri-reference 
                          (normalize-pct-encoding (uri->string uri (lambda (user pass) (conc user ":" pass))))))
         (scheme (string->symbol (string-downcase (->string (uri-scheme uri)))))
         (host           (normalize-pct-encoding (string-downcase (uri-host uri)))))
    (update-uri* normalized-uri 'scheme scheme 'host host)))

(define (normalize-pct-encoding str)
  (let ((str1 (uri-string->normalized-char-list str)))
    (and str1 (uri-char-list->string
	       (map (lambda (c) (match c
				       ((#\% h1 h2)  `(#\% ,(char-upcase h1) ,(char-upcase h2)))
				       (else c)))
		    str1)))))

;;  Path segment normalization; cf. RFC3986 section 6.2.2.4

(define (uri-normalize-path-segments uri)
  (update-URI uri 'path (just-segments uri)))

(define (uri-path-absolute? uri)
  (let ((path (uri-path uri)))
   (and (pair? path) (eq? '/ (car path)))))

(define (uri-path-relative? uri)
  (not (uri-path-absolute? uri)))

(define (uri->path-string uri)
  (uri->string
   (update-uri uri 'scheme #f 'authority #f 'fragment #f)))

))
