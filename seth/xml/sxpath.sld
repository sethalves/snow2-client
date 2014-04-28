(define-library (seth xml sxpath)

  (export sxpath nodeset? as-nodeset sxml:element? ntype-names??
          ntype?? ntype-namespace-id?? sxml:complement node-eq? node-equal?
          node-pos sxml:filter take-until take-after map-union
          node-parent
          node-reverse node-trace select-kids node-self node-join
          node-reduce node-or node-closure
          if-sxpath if-car-sxpath car-sxpath
          sxml:id-alist
          sxml:string sxml:boolean sxml:number sxml:string-value
          sxml:node? sxml:attr-list sxml:id sxml:equality-cmp
          sxml:equal? sxml:not-equal? sxml:relational-cmp
          sxml:attribute sxml:child sxml:parent sxml:ancestor
          sxml:ancestor-or-self sxml:descendant sxml:descendant-or-self
          sxml:following sxml:following-sibling sxml:namespace
          sxml:preceding sxml:preceding-sibling
          sxml:child-nodes sxml:child-elements
          sxml:xpath
          sxml:xpath+index
          sxml:xpath+root
          sxml:xpath+root+vars
          sxml:xpointer+index
          sxml:xpointer+root+vars)

  (import (scheme base))

  (cond-expand
   (chibi
    (import
     (scheme read) (scheme write) (scheme char) (scheme cxr)
     (chibi io)
     (only (chibi string) string-null? call-with-input-string)
     (only (scheme r5rs) inexact->exact exact->inexact)
     (srfi 1) (srfi 2)
     (snow snowlib) (snow assert) (snow srfi-13-strings) (snow extio)))
   (chicken (import (sxpath) (txpath) (sxpath-lolevel)))
   (gauche (import (sxml sxpath)))
   (sagittarius (import (text sxml sxpath))))

  (begin


    (cond-expand
     ((or chicken gauche sagittarius) #t)
     (else


(define (write-to-string obj)
  (let ((s (open-output-string)))
    (write obj s)
    (let ((result (get-output-string s)))
      (close-output-port s)
      result)))


(define (display-to-string obj)
  (let ((s (open-output-string)))
    (display obj s)
    (let ((result (get-output-string s)))
      (close-output-port s)
      result)))


(define pp snow-pretty-print)


(define-syntax --
  (syntax-rules () ((-- x) (- x 1))))

(define-syntax inc
  (syntax-rules () ((inc x) (+ 1 x))))

(define (cout . strs)
  (for-each
   (lambda (str)
     (display str))
   strs))


(define (cerr . strs)
  (for-each
   (lambda (str)
     (display str (current-error-port)))
   strs))


; Return the index of the last occurence of a-char in str, or #f
; See SRFI-13
(define string-rindex string-index-right)


(define nl (string #\newline))


; -- procedure+: substring? PATTERN STRING
;     Searches STRING to see if it contains the substring PATTERN.
;     Returns the index of the first substring of STRING that is equal
;     to PATTERN; or `#f' if STRING does not contain PATTERN.
;
;          (substring? "rat" "pirate")             =>  2
;          (substring? "rat" "outrage")            =>  #f
;          (substring? "" any-string)              =>  0
(define (substring? pattern str) (string-contains str pattern))


(define-syntax assert
  (syntax-rules ()
    ((assert should-be-true)
     (snow-assert should-be-true))
    ((assert should-be-true-0 should-be-true-1)
     (snow-assert should-be-true-0)
     (snow-assert should-be-true-1))))


; 
; -- procedure+: string-split STRING
; -- procedure+: string-split STRING '()
; -- procedure+: string-split STRING '() MAXSPLIT
;
; Returns a list of whitespace delimited words in STRING.
; If STRING is empty or contains only whitespace, then the empty list
; is returned. Leading and trailing whitespaces are trimmed.
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; -- procedure+: string-split STRING CHARSET
; -- procedure+: string-split STRING CHARSET MAXSPLIT
;
; Returns a list of words delimited by the characters in CHARSET in
; STRING. CHARSET is a list of characters that are treated as delimiters.
; Leading or trailing delimeters are NOT trimmed. That is, the resulting
; list will have as many initial empty string elements as there are
; leading delimiters in STRING.
;
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; This is based on the split function in Python/Perl
;
; (string-split " abc d e f  ") ==> ("abc" "d" "e" "f")
; (string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
; (string-split " abc d e f  " '() 0) ==> ()
; (string-split ":abc:d:e::f:" '(#\:)) ==> ("" "abc" "d" "e" "" "f" "")
; (string-split ":" '(#\:)) ==> ("" "")
; (string-split "root:x:0:0:Lord" '(#\:) 2) ==> ("root" "x:0:0:Lord")
; (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
; ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
; (string-split "/usr/local/bin" '(#\/)) ==> ("" "usr" "local" "bin")

(define (string-split str . rest)
		; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (inc i) yet-to-split-count))
        (else (scan-beg-word (inc i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

		; maxsplit is a positive number
		; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

			; resolver of overloading...
			; if omitted, maxsplit defaults to
			; (inc (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (inc (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (inc (string-length str)))))
        (cond 
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)




; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return
; a quotation procedure. The returned quotation procedure takes a string
; and returns either a string or a list of strings. The quotation procedure
; check to see if its argument string contains any instance of a character
; that needs to be encoded (quoted). If the argument string is "clean",
; it is returned unchanged. Otherwise, the quotation procedure will
; return a list of string fragments. The input straing will be broken
; at the places where the special characters occur. The special character
; will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters,
; do
;	(make-char-quotator
;	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (map car char-encoding)))

    ; Check to see if str contains one of the characters in charset,
    ; from the position i onward. If so, return that character's index.
    ; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
	(and (< i (string-length str))
	     (if (memv (string-ref str i) charset) i
		 (loop (inc i))))))

    ; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
	(if (not bad-pos) str	; str had all good chars
	    (let loop ((from 0) (to bad-pos))
	      (cond
	       ((>= from (string-length str)) '())
	       ((not to)
		(cons (substring str from (string-length str)) '()))
	       (else
		(let ((quoted-char
		       (cdr (assv (string-ref str to) char-encoding)))
		      (new-to 
		       (index-cset str (inc to) bad-chars)))
		  (if (< from to)
		      (cons
		       (substring str from to)
		       (cons quoted-char (loop (inc to) new-to)))
		      (cons quoted-char (loop (inc to) new-to))))))))))
))







;;                            S X M L   T o o l s               
; $Revision: 3.14 $ from $Date: 2003/12/23 05:39:31 $:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to lisovsky@acm.org
;           Kirill Lisovsky
;
;   SXML normal form used for normalization-dependent functions:
; If attr-list is present it's always the second in SXML element.
; If aux-list is present - then list of attributes is always
; included, and aux-list is always the third.
;   Minimized form is just the same, but all the empty aux-lists are 
; absent, and empty attr-lists are present only in elements with aux-lists
; present. 

;==============================================================================
; Auxiliary functions.

; unlike filter-map from SRFI-1 this function uses separate predicate 
; and mapping functions. 
; Applies proc to  all the elements of source list that satisfy the predicate 
; and return the list of the results.
(define (filter-and-map pred proc lis)			
  (let rpt ((l lis))		
    (if (null? l)
      '()
      (if (pred (car l))
	(cons (proc (car l)) (rpt (cdr l)))
	(rpt (cdr l))))))
      
; Applies pred to every member of lst and yields #t if all the results
; are #t
(define (check-list pred lst) 
  (cond
    ((null? lst) #t)
    ((pred (car lst))
     (check-list pred (cdr lst)))
    (else #f)))

; Returns attr-list node for a given obj 
;   or #f if it is absent
(define (sxml:attr-list-node obj)
  (if (and (not (null? (cdr obj)))
	    (pair? (cadr obj)) 
	    (eq? '@ (caadr obj)))
	 (cadr obj)
	 #f))

; Returns attr-list wrapped in list
;   or '((@)) if it is absent and aux-list is present 
;   or '() if both lists are absent
(define (sxml:attr-as-list obj)
  (cond
    ((sxml:attr-list-node obj)
     => list)
    ((sxml:aux-list-node obj)
     '((@)))
    (else '())))


; Returns aux-list node for a given obj 
;   or #f if it is absent
(define (sxml:aux-list-node obj)
  (if
    (or (null? (cdr obj))
	(null? (cddr obj))
	(not (pair? (caddr obj)))
	(not (eq? (caaddr obj) '@@)))
    #f
    (caddr obj)))

; Returns aux-list wrapped in list 
;   or '() if it is absent
(define (sxml:aux-as-list obj)
  (cond 
    ((sxml:aux-list-node obj)
     => list)
    (else '())))

; optimized (string-rindex name #\:)
; returns position of a separator between namespace-id and LocalName
;; (define-macro (sxml:find-name-separator len)
;;   `(let rpt ((pos (-- ,len))) 
;;      (cond
;;        ((negative? pos) #f) 	
;;        ((char=? #\: (string-ref name pos)) pos)
;;        (else (rpt (-- pos))))))


(define (sxml:find-name-separator name len)
  (let rpt ((pos (-- len)))
    (cond
     ((negative? pos) #f)
     ((char=? #\: (string-ref name pos)) pos)
     (else (rpt (-- pos))))))



; sxml error message
(define (sxml:error . messages)
  (snow-error
   "SXML ERROR"
   (apply string-append (map display-to-string messages)))
  ;; (cerr nl "SXML ERROR: ")
  ;; (apply cerr messages)
  ;; (cerr nl)
  ;; (exit -1)
  )

;==============================================================================
; Predicates

; Predicate which returns #t if given element <obj> is empty. 
; Empty element has no nested elements, text nodes, PIs, Comments or entities
; but it may contain attributes or namespace-id.
; It is a SXML counterpart of XML empty-element.
(define (sxml:empty-element? obj)
  (not 
    ((select-first-kid 
     (lambda(x)
       (or ((ntype-names?? '(*PI* *COMMENT* *ENTITY*)) x)
           ((ntype?? '*) x)
	   (string? x)))) obj)))

; Returns #t if the given <obj> is shallow-normalized SXML element.
; The element itself has to be normalised but its nested elements are not tested.
(define (sxml:shallow-normalized? obj)
  (or 
    (null? (cdr obj))
    (and (or 
	   (and 
	     (pair? (cadr obj)) 
	     (eq? (caadr obj) '@))
	   (not ((select-first-kid (ntype-names?? '(@ @@))) obj)))
	 (or (null? (cddr obj))
	     (and (pair? (caddr obj)) 
		  (eq? (caaddr obj) '@@))
	     (not ((select-first-kid (ntype?? '@@)) obj))))))

; Returns #t if the given <obj> is normalized SXML element.
;  The element itself and all its nested elements have to be normalised.
(define (sxml:normalized? obj)
    (and
      (sxml:shallow-normalized? obj)
    (check-list
      (lambda(x)
	(if
	   (sxml:element? x)
	   (sxml:normalized? x)
	   #t))
       (sxml:content obj))
    ))

; Returns #t if the given <obj> is shallow-minimized SXML element.
; The element itself has to be minimised but its nested elements are not tested.
(define (sxml:shallow-minimized? obj)
  (and
    (sxml:shallow-normalized? obj)
    (not (and (sxml:aux-list-node obj) 
	      (null? (sxml:aux-list obj))))
    (not (and (sxml:attr-list-node obj)
	      (null? (sxml:attr-list obj))
	      (not (sxml:aux-list-node obj))))))

; Returns #t if the given <obj> is minimized SXML element.
;  The element itself and all its nested elements have to be minimised.
(define (sxml:minimized? obj)
    (and
      (sxml:shallow-minimized? obj)
    (check-list
      (lambda(x)
	(if
	   (sxml:element? x)
	   (sxml:minimized? x)
	   #t))
       (sxml:content obj))
    ))

;==============================================================================
; Accessors  

; Returns a name of a given SXML node
; It is introduced for the sake of encapsulation.
(define sxml:name car)

; A version of sxml:name, which returns #f if the given <obj> is 
; not a SXML element.
; Otherwise returns its name.
(define (sxml:element-name obj)
  (and ((ntype?? '*) obj) 
       (car obj)))

; Safe version of sxml:name, which returns #f if the given <obj> is 
; not a SXML node.
; Otherwise returns its name.
(define (sxml:node-name obj)
  (and (pair? obj) 
       (symbol? (car obj))
    (car obj)))

; Returns Local Part of Qualified Name (Namespaces in XML production [6])
; for given obj, which is ":"-separated suffix of its Qualified Name
; If a name of a node given is NCName (Namespaces in XML production [4]), then 
; it is returned as is.
; Please note that while SXML name is a symbol this function returns a string.
(define (sxml:ncname obj)
  (let* ((name (symbol->string (car obj)))
	 (len (string-length name)))
    (cond
      ((sxml:find-name-separator name len)
       => (lambda (pos) 
	    (substring name (+ pos 1) len)))
      (else name))))

; Returns namespace-id part of given name, or #f if it's LocalName
(define (sxml:name->ns-id sxml-name)
  (let* ((name (symbol->string sxml-name)))
    (cond
      ((sxml:find-name-separator name (string-length name))
       => (lambda (pos) 
	    (substring name  0 pos)))
      (else #f))))
    

; Returns the content of given SXML element or nodeset (just text and element
; nodes) representing it as a list of strings and nested elements in document 
; order.  This list is empty if <obj> is empty element or empty list.
(define (sxml:content obj)
  (((if (nodeset? obj) 
      sxml:filter
      select-kids) 
    (lambda(x)
      (or
	(string? x)   ;  ((ntype?? '*text*) x)
       ((ntype?? '*) x)))) 
   obj))

; Returns a string which combines all the character data 
; from text node childrens of the given SXML element
; or "" if there is no text node children
(define (sxml:text obj)
  (let ((tnodes
	 ((select-kids
	   string?) 
	   obj)))
    (cond 
      ((null? tnodes) "")
      ((null? (cdr tnodes))
       (car tnodes))
      (else (apply string-append tnodes)))))

;------------------------------------------------------------------------------
; Normalization-dependent accessors
;
;
; "Universal" accessors are less effective but may be used for non-normalized SXML
; Safe accessors are named with suffix '-u'
;
; "Fast" accessors are optimized for normalized SXML data.
; They are not applicable to arbitrary non-normalized SXML data
; Their names has no specific suffixes

; Returns all the content of normalized SXML element except attr-list and
; aux-list.
; Thus it includes PI, COMMENT and  ENTITY nodes as well as TEXT and ELEMENT nodes
; returned by sxml:content.
; Returns  a list of nodes in document order or empty list if <obj> is empty 
; element or empty list.
; This function is faster than sxml:content
(define (sxml:content-raw obj)
  ((if (and (not (null? (cdr obj))) 
	    (pair? (cadr obj)) (eq? (caadr obj) '@))
     (if (and (not (null? (cddr obj))) 
	      (pair? (caddr obj)) (eq? (caaddr obj) '@@))
       cdddr
       cddr)
     cdr) obj))


; Returns the list of attributes for given element or nodeset.
; Analog of ((sxpath '(@ *)) obj)
; Empty list is returned if there is no list of attributes.
(define (sxml:attr-list-u obj)
  (cond (((select-first-kid (ntype?? '@)) obj)
	 => cdr)
	(else '())))

; Returns the list of auxiliary nodes for given element or nodeset.
; Analog of ((sxpath '(@@ *)) obj)
; Empty list is returned if a list of auxiliary nodes is absent.
(define (sxml:aux-list obj)
  (if
    (or (null? (cdr obj))
	(null? (cddr obj))
	(not (pair? (caddr obj)))
	(not (eq? (caaddr obj) '@@)))
    '()
    (cdaddr obj)))  

; Returns the list of auxiliary nodes for given element or nodeset.
; Analog of ((sxpath '(@@ *)) obj)
; Empty list is returned if a list of auxiliary nodes is absent.
(define (sxml:aux-list-u obj)
  (cond (((select-first-kid (ntype?? '@@)) obj)
	 => cdr)
	(else '())))

; Return the first aux-node with <aux-name> given in SXML element <obj> 
; or #f is such a node is absent.
; NOTE: it returns just the FIRST node found even if multiple nodes are
; present, so it's mostly intended for nodes with unique names 
(define (sxml:aux-node obj aux-name)
  (cond 
    ((assq aux-name (sxml:aux-list obj)))
    (else #f))) 

; Return a list of aux-node with <aux-name> given in SXML element <obj> 
; or '() if such a node is absent.
(define (sxml:aux-nodes obj aux-name)
  (filter 
    (lambda(x) (eq? aux-name (car x)))
    (sxml:aux-list obj)))

; Accessor for an attribute <attr-name> of given SXML element <obj> which 
; It returns: 
;    the value of the attribute if the attribute is present
;    #f if there is no such an attribute in the given element
(define (sxml:attr obj attr-name)
  (cond 
    ((assq attr-name (sxml:attr-list obj))
     => cadr)
    (else #f)))

; Extracts a value of attribute with given name from attr-list
(define (sxml:attr-from-list attr-list name)
	    (cond 
	      ((assq name attr-list) 
	       => cadr)
	      (else #f)))

; Accessor for a numerical attribute <attr-name> of given SXML element <obj> 
; which It returns: 
;    a value of the attribute as the attribute as a number if the attribute 
;    is present and its value may be converted to number using string->number
;    #f if there is no such an attribute in the given element or
;    its value can't be converted to a number
(define (sxml:num-attr obj attr-name)
  (cond 
    ((assq attr-name (sxml:attr-list obj))
     => (lambda(x) (string->number (cadr x))))
    (else #f)))

; Accessor for an attribute <attr-name> of given SXML element <obj> which 
; may also be an attributes-list or nodeset (usually content of SXML element)
;
; It returns: 
;    the value of the attribute if the attribute is present
;    #f if there is no such an attribute in the given element
(define (sxml:attr-u obj attr-name)
  (cond 
    ((assq attr-name
	   ; the list of attributes is computed below
	   (cond
	     ((and (not (null? (cdr obj))) 
		   (pair? (cadr obj))
		   (eq? '@ (caadr obj)))
	      (cdadr obj))   ; fast track for normalized elements 
	     ((eq? '@ (car obj))
	      (cdr obj))     ; if applied to attr-list
	     (else (sxml:attr-list-u obj))))
     => cadr)
    (else #f)))

; Returns the list of namespaces for given element.
; Analog of ((sxpath '(@@ *NAMESPACES* *)) obj)
; Empty list is returned if there is no list of namespaces.
(define (sxml:ns-list obj)
  (cond ((assv '*NAMESPACES* (sxml:aux-list obj))
	 => cdr)
	(else '())))

; Returns the list of namespace-assoc's for given namespace-id in 
; SXML element <obj>.
; Analog of ((sxpath '(@@ *NAMESPACES* namespace-id)) obj)
; Empty list is returned if there is no namespace-assoc with namespace-id
; given.
(define (sxml:ns-id->nodes obj namespace-id)
  (filter 
    (lambda(x)
      (eq? (car x) namespace-id))
    (sxml:ns-list obj)))

; It returns: 
;    A  URI's for namespace-id given 
;    #f if there is no namespace-assoc with namespace-id given
(define (sxml:ns-id->uri obj namespace-id)
  (cond 
    ((assq namespace-id (sxml:ns-list obj))
     => cadr)
    (else #f)))

; Returns a list of namespace-assocs nodes for NS URI given 
(define (sxml:ns-uri->nodes obj URI)
  (filter
    (lambda (ns-assoc) 
      (string=? (cadr ns-assoc) URI))
    (sxml:ns-list obj)))

; Returns a namespace-id for NS URI given 
(define (sxml:ns-uri->id obj URI)
  (let rpt ((ns-assocs (sxml:ns-list obj)))
  (cond
      ((null? ns-assocs) #f)
      ((string=? (cadar ns-assocs) URI)
       (caar ns-assocs))
      (else (rpt (cdr ns-assocs)))
    )))

; Returns namespace-id for given namespace-assoc list
(define sxml:ns-id car)

; Returns URI for given namespace-assoc list
(define sxml:ns-uri cadr)

; It returns namespace prefix for given namespace-assoc list
;  Original (as in XML document) prefix for namespace-id given 
; has to be strored as the third element in namespace-assoc list 
; if it is different from namespace-id.
;    If original prefix is omitted in namespace-assoc then
;      namespace-id is used instead
(define (sxml:ns-prefix ns-assoc)
      (if (> (length ns-assoc) 2)
	(caddr ns-assoc)
	(car ns-assoc))) 

;==============================================================================
; Data modification functions
; Constructors and mutators for normalized SXML data
; 
; This functions are optimized for normalized SXML data.
; They are not applicable to arbitrary non-normalized SXML data
; 
; Most of the functions are provided in two variants: 
; 1. side-effect intended functions for linear update of given elements.
;   Their names are ended with exclamation mark.
;   An example: 
;      sxml:change-content! 
; 2. pure functions without side-effects which return modified elements.
;   An example: 
;      sxml:change-content
 
; Change the content of given SXML element to <new-content>
; If <new-content> is an empty list then the <obj> is transformed 
; The resulting SXML element is normalized
; Former name sxml:content!
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:change-content! obj new-content)
    (set-cdr! obj 
              `(
                ,@(sxml:attr-as-list obj)
                ,@(sxml:aux-as-list obj)
                ,@new-content)))
  ))
  
; Change the content of given SXML element to <new-content>
; If <new-content> is an empty list then the <obj> is transformed 
; to an empty element
; The resulting SXML element is normalized
(define (sxml:change-content obj new-content)
  `(,(sxml:name obj) 
              ,@(sxml:attr-as-list obj)
              ,@(sxml:aux-as-list obj)
	,@new-content))

; The resulting SXML element is normalized, if <new-attrlist> is empty,
; the cadr of <obj> is (@)
(define (sxml:change-attrlist obj new-attrlist)
  `(,(sxml:name obj) 
     ,@(cond 
	 (new-attrlist
	  `((@ ,@new-attrlist)))
	 ((sxml:aux-list-node obj)
	   '((@)))
	 (else `()))
     ,@(sxml:aux-as-list obj)
     ,@(sxml:content obj)))

; The resulting SXML element is normalized, if <new-attrlist> is empty,
; the cadr of <obj> is (@)
; Former name sxml:attrlist!
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:change-attrlist! obj new-attrlist)
    (set-cdr! obj 
              `(
                ,@(cond 
                    (new-attrlist
                     `((@ ,@new-attrlist)))
                    ((sxml:aux-list-node obj)
                     '((@)))
                    (else `()))
                ,@(sxml:aux-as-list obj)
                ,@(sxml:content obj))))
  ))
      
; Change a name of SXML element destructively
; Former name was 'sxml:name!'
(cond-expand
 (plt
  #f  ; set-car removed from plt
  )
 (else
  (define (sxml:change-name! obj new-name)
    (set-car! obj new-name))
  ))
  
; Returns SXML element with its name changed 
(define (sxml:change-name obj new-name)
  (cons new-name (cdr obj)))

; Returns SXML element <obj> with attribute <attr> added or #f
; if the attribute with given name already exists, 
; <attr> is (<attr-name> <attr-value>)
; Pure functional counterpart to sxml:add-attr!
(define (sxml:add-attr obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (if (assq (car attr) attr-list) 
      #f
      `(,(sxml:name obj)
	(@ ,@(cons attr attr-list))
	,@(sxml:aux-as-list obj)
	,@(sxml:content obj)))))

; Add an attribute <attr> for an element <obj>
; Returns #f if the attribute with given name already exists. 
; The resulting SXML node is normalized.
; Linear update counterpart to sxml:add-attr
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:add-attr! obj attr)
    (let ((attr-list (sxml:attr-list obj)))
      (if (assq (car attr) attr-list) 
          #f
          (begin
            (set-cdr! obj 
                      `(
                        (@ ,@(cons attr attr-list))
                        ,@(sxml:aux-as-list obj)
                        ,@(sxml:content obj)))
            obj))))
  ))


; Returns SXML element <obj> with changed value of attribute <attr> or #f
; if where is no attribute with given name. 
; <attr> is (<attr-name> <attr-value>)
(define (sxml:change-attr obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (if (null? attr-list)
      #f
      (cond 
	((assv (car attr) attr-list) 
	 => (lambda (y)
	      `(,(sxml:name obj)
		 (@ ,@(map
			(lambda(at)
			  (if
			    (eq? at y)
			    attr
			    at))
			attr-list))
		 ,@(sxml:aux-as-list obj)
		 ,@(sxml:content obj)
		 )))
	(else #f)))))
    
; Change value of the attribute for element <obj> 
; <attr> is (<attr-name> <attr-value>)
; Returns #f if where is no such attribute
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:change-attr! obj attr)
    (let ((x (sxml:attr-list obj)))
      (if (null? x)
          #f
          (cond 
            ((assv (car attr) x) => (lambda (y)
                                      (set-cdr! y (cdr attr)) obj))
            (else #f)))))
  ))

; Set attribute <attr> of element <obj> 
; If there is no such attribute the new one is added
(define (sxml:set-attr obj attr)
  (let ((attr-list (sxml:attr-list obj)))
    (cond 
      ((assv (car attr) attr-list) 
       => (lambda (y)
	    `(,(sxml:name obj)
	       (@ ,@(map
		      (lambda(at)
			(if
			  (eq? at y)
			  attr
			  at))
		      attr-list))
	       ,@(sxml:aux-as-list obj)
	       ,@(sxml:content obj)
	       )))
      (else 
	`(,(sxml:name obj)
	   (@ ,@(cons attr attr-list)) 
	   ,@(sxml:aux-as-list obj)
	   ,@(sxml:content obj))))
    ))

; Set attribute <attr> of element <obj> 
; If there is no such attribute the new one is added
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:set-attr! obj attr)
    (let ((attr-list (sxml:attr-list obj)))
      (cond 
        ((assv (car attr) attr-list) 
         => (lambda (x) (set-cdr! x (cdr attr))))
        (else (set-cdr! obj
                        `((@ ,@(cons attr attr-list)) 
                          ,@(sxml:aux-as-list obj)
                          ,@(sxml:content obj))))
        )))
  ))

; Returns SXML element <obj> with an auxiliary node <aux-node> added 
(define (sxml:add-aux obj aux-node)
      `(,(sxml:name obj)
	(@ ,@(sxml:attr-list obj))
	(@@ ,@(cons aux-node (sxml:aux-list obj)))
	,@(sxml:content obj)))

; Add an auxiliary node <aux-node> for an element <obj>
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:add-aux! obj aux-node)
    (set-cdr! obj 
              `(
                (@ ,@(sxml:attr-list obj))
                (@@ ,@(cons aux-node (sxml:aux-list obj)))
                ,@(sxml:content obj)))
    obj)
  ))

; Eliminates empty lists of attributes and aux-lists for given SXML element 
; <obj> and its descendants ("minimize" it)
; Returns: minimized and normalized SXML element
(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:squeeze! obj)
    (set-cdr! obj 
              `(,@(cond 
                    ((sxml:attr-list-node obj)
                     => (lambda (atl) 
                          (if (and (null? (cdr atl)) 
                                   (null? (sxml:aux-list obj)))
                              '()
                              (list atl))))	
                    (else '()))
                ,@(cond ((sxml:aux-list-node obj)
                         => (lambda (axl) 
                              (if (null? (cdr axl))
                                  '()
                                  (list axl))))
                        (else '()))
                ,@(map
                   (lambda(x)
                     (cond 
                       (((ntype?? '*) x)
                        (sxml:squeeze! x)
                        x)
                       (else x)))
                   (sxml:content obj))
                ))
    )
  ))

	     
; Eliminates empty lists of attributes and aux-lists for given SXML element 
; <obj> and its descendants ("minimize" it)
; Returns: minimized and normalized SXML element
(define (sxml:squeeze obj)
  `(,(sxml:name obj)
   ,@(cond 
	((sxml:attr-list-node obj)
	 => (lambda (atl) 
	      (if (and (null? (cdr atl)) 
		       (null? (sxml:aux-list obj)))
		 '()
	         (list atl))))	
	(else '()))
    ,@(cond ((sxml:aux-list-node obj)
	     => (lambda (axl) 
	      (if (null? (cdr axl))
		'()
	         (list axl))))
	(else '()))
    ,@(map
	(lambda(x)
	  (cond 
	    (((ntype?? '*) x)
	     (sxml:squeeze x))
	    (else x)))
       (sxml:content obj))))

; Eliminates empty lists of attributes and ALL aux-lists for given SXML element 
; <obj> and its descendants
; Returns: minimized and normalized SXML element
(define (sxml:clean obj)
  `(,(sxml:name obj)
   ,@(cond 
	((sxml:attr-list-node obj)
	 => (lambda (atl) 
	      (if (null? (cdr atl)) 
		 '()
	         (list atl))))	
	(else '()))
    ,@(map
	(lambda(x)
	  (cond 
	    (((ntype?? '*) x)
	     (sxml:clean x))
	    (else x)))
       (sxml:content obj))))
;==============================================================================
; SXPath-related 

;------------------------------------------------------------------------------
; Extensions

; select-first-kid:: Pred -> Node -> Node
; Given a Node, return its first child that satisfy
; the test-pred?
; Returns #f if there is no such a child
; select-first-kid:: Pred -> Nodeset -> Node
; The same as above, but select among children of all the nodes in
; the Nodeset
(define (select-first-kid test-pred?)
 (lambda(obj)
  (let rpt ((lst (if (symbol? (car obj)) 
		  (cdr obj)
		  obj)))
    (cond 
      ((null? lst) #f)
      ((and (pair? (car lst))
	    (test-pred? (car lst)))
	(car lst))
      (else (rpt (cdr lst)))) 
    )))

;------------------------------------------------------------------------------
; Fast node-parent 

; Returns a function of one argument - SXML element - which returns its parent
; node using *PARENT* pointer in aux-list
; '*TOP-PTR* may be used as a pointer to root node
; It return an empty list when applyed to root node
(define (sxml:node-parent rootnode)
  (lambda(obj)
  (cond 
    ((sxml:aux-node obj '*PARENT*)
     => (lambda(x)
 	  (if 
 	    (eq? '*TOP-PTR* (cadr x))
	  rootnode
	  ((cadr x)))))
    ((and (pair? obj)
          (eq? (car obj) '*TOP* ))
     '())           
     (else (sxml:error nl "PARENT pointer is absent in: " obj nl)
	   ))))

(cond-expand
 (plt
  #f  ; set-cdr removed from plt
  )
 (else
  (define (sxml:add-parents obj . top-ptr)
    (let rpt 
      ((elt obj)
       (p '*TOP*)
       (at-aux (if (eq? (sxml:name obj) '*TOP*)
                   (list (cons '@@ (sxml:aux-list-u obj)))
                   (list
                    (cons '@ (sxml:attr-list obj))
                    (cons '@@ (cons `(*PARENT* ,(lambda() (car top-ptr))) 
                                    (sxml:aux-list obj))))))
       ) ; *TOP* is a parent for top-level element
      (let* ((h (list (sxml:name elt)))
             (b  (append 
                  at-aux
                  (map
                   (lambda(x)
                     (cond 
                       (((ntype?? '*) x)
                        (rpt x h
                             (list
                              (cons '@ (sxml:attr-list x))
                              (cons '@@ (cons `(*PARENT* ,(lambda() h)) 
                                              (sxml:aux-list x))))
                             ))
                       (else x)))
                   (sxml:content elt)))))
        (set-cdr! h b)
        h)))
  ))

; Lookup an element using its ID 
(define (sxml:lookup id index)
    (cond
      ((assoc id index) 
       => cdr)
      (else #f)))

;==============================================================================
; Markup generation

;------------------------------------------------------------------------------
; XML

; Creates the XML markup for attributes.
(define (sxml:attr->xml attr)
   (list " " (sxml:ncname attr)
	 "='" (cadr attr) "'"))

; Return a string or a list of strings where all the occurences of 
; characters < > & " ' in a given string are replaced by corresponding 
; character entity references. See also:  sxml:string->html
(define sxml:string->xml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") 
		    (#\" . "&quot;") (#\' . "&apos;"))))

; A version of dispatch-node specialized and optimized for SXML->XML
; transformation.
(define (sxml:sxml->xml tree)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (sxml:sxml->xml a-tree)) 
	  tree))
    ((pair? tree)
     (let* ((name (sxml:name tree))   ; NS (URI-prefixed) not supported
	    (nm (symbol->string name))
	    (content (sxml:content-raw tree)))
	 `("<" ,nm ,@(map sxml:attr->xml (sxml:attr-list tree))
	   ,@(if (null? content) '("/>")
	       `(">" ,@(sxml:sxml->xml content) "</" ,nm ">")))))
    ((string? tree) (sxml:string->xml tree)) ; *text*
    (else (sxml:error "sxml->html - unexpected type of node: " tree))))


;------------------------------------------------------------------------------
; HTML

; Creates the HTML markup for attributes.
(define (sxml:attr->html attr)
	 (if (equal? "" (cadr attr))
             (list " " (sxml:ncname attr))
             (list " " (sxml:ncname attr) "='" (cadr attr) "'")))



; Given a string, check to make sure it does not contain characters
; < > & " that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.
; Borrowed from Oleg Kiselyov's XML-to-HTML.scm (where its name is
; string->goodHTML)
(define sxml:string->html
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))


; This predicate yields #t for "unterminated" HTML 4.0 tags
(define (sxml:non-terminated-html-tag? tag) 
  (memq tag 
     '(area base basefont br col frame hr img input isindex link meta param)))


; A version of dispatch-node specialized and optimized for SXML->HTML
; transformation.
(define (sxml:sxml->html tree)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (sxml:sxml->html a-tree)) 
	  tree))
    ((pair? tree)
     (let* ((name (sxml:name tree))
	    (nm (symbol->string name))
	    (content (sxml:content-raw tree)))
	 `("<" ,nm ,@(map sxml:attr->html (sxml:attr-list tree))
	   ,@(if (null? content)
	       (if (sxml:non-terminated-html-tag? name) '(">") '("/>"))
	       `(">" ,@(sxml:sxml->html content) "</" ,nm ">")))))
    ((string? tree) (sxml:string->html tree)) ; *text*
    (else (sxml:error "sxml->html - unexpected type of node: " tree))))











;; XPath/XPointer grammar parser.
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; Parser parameterization
; For building a specific XPath/XPointer implementation, grammar parser is to
; be parameterized
;  txp-params ::= (listof  txp-param )
;  txp-param ::= (list  param-name  param-value  [parameterized-func] )
;  parameterized-func is optional
; Each parser-param generally specifies the parser result for the single
; XPath/XPointer grammar rule

; Given param-name, returns the corresponding lambda
(define (txp:param-value param-name txp-params)
  (cond
    ((assq param-name txp-params)
     => cadr)
    (else
     (display "Parameter unspecified: ")
     (display param-name)
     0  ; this would cause program termination
     )))


;=========================================================================
; Errors handling
; There are 2 kinds of errors: syntactic errors and semantic errors
; - Syntactic error is raised when the location path (fragment identifier)
;   doesn't satisfy XPath (XPointer) grammar. Syntactic error is discovered
;   and raised by the parser.
; - Semantic error can be raised by the specific parser parametrization

; Whether a parser returns an error
(define (txp:error? obj)
  (or (eq? obj 'txp:parser-error)
      (eq? obj 'txp:semantic-error)))

;-------------------------------------------------
; Syntactic error (also called a parser error)

(define (sxml:xpointer-parse-error . text)
  (apply cerr
         (append (list "XPath/XPointer parser error: ") text (list nl)))
  #f)

; A warning message for grammar features which are not supported by this
; implementation
(define (sxml:xpointer-parse-warning . text)
  (apply cerr
         (append (list "XPath/XPointer parser warning: ") text (list nl))))


;-------------------------------------------------
; Semantic error
; To signal the parser about the semantic error, the specific parametrization
; is to return the symbol  'txp:semantic-error

(define (txp:semantic-errs-detected? . res-list)
  (not (null?
        (filter
         (lambda (res) (eq? res 'txp:semantic-error))
         res-list))))

; Constructed specific parsers may wish to use this function
(define (txp:signal-semantic-error . text)
  (apply cerr
         (append (list "XPath/XPointer semantic error: ") text (list nl)))
  'txp:semantic-error)


;=========================================================================
; Low level parsing functions
; XPath location path (XPointer fragment identifier) is represented as a list
; of chars

; A list of whitespace characters
(define sxml:whitespace '(#\space #\return #\newline #\tab))

; A sxml:whitespace or () <> [] : / + * , = | ! " ' @ $
(define sxml:delimiter (append sxml:whitespace
                              '(#\( #\) #\< #\> #\[ #\] #\: #\/ #\+ 
                                #\* #\, #\= #\| #\! #\" #\' #\@ #\$)))

; A list of characters a NCName cannot start with
(define (sxml:non-first? ch)
  (or (char-numeric? ch)
      (memv ch sxml:delimiter) 
      (memv ch '(#\. #\-))))

; The function reads a whitespace , production [3] (S) in XML Rec.
;  path - xpointer path string as a list of chars 
; It returns a new path
(define (sxml:skip-ws path)
  (if (or (null? path)
	  (not (memv (car path) sxml:whitespace)))
    path
    (sxml:skip-ws (cdr path))))

; Asserts that the path is over, possibly with trailing whitespace symbols at
; the end. Returns the boolean value - whether assertion passes. If assertion
; fails, signals an error message
(define (sxml:assert-end-of-path path)
  (let ((path (sxml:skip-ws path)))
    (or 
     (null? path)
     (begin
       (sxml:xpointer-parse-error "unexpected - \"" (list->string path) "\"")
       #f))))


;------------------------------------------------
; These two functions read expected information from the path

; Whether the path begins with a 'str' (starting whitespaces are ignored)
;  str - a string to match
;  path - an xpointer path represented as a list of chars
;  char-list - an optional argument. If this argument is supplied, a 'str'
; pattern must be followed by a character from a 'char-list'
; If 'str' is really in the beginning of path, a new path is returned
; Otherwise, function returns #f (path remains unchanged)
(define (sxml:parse-check str path . char-list)
  (let loop ((lst (string->list str)) 
             (p (sxml:skip-ws path)))
    (cond
      ((null? lst)
       (if
        (or (null? p) (null? char-list) (memv (car p) (car char-list)))
        p
        #f))
      ((null? p) #f)
      ((char=? (car lst) (car p))
       (loop (cdr lst) (cdr p)))
      (else #f))))

; Checks whether the PATH starts with a sequence of strings (possibly
; separated by a whitespace) from STR-SEQ
; Returns a new PATH (match successful) or #f (otherwise)
(define (sxml:parse-check-sequence str-seq path . char-list)
  (let ((char-list (if (null? char-list) #f (car char-list))))
    (let loop ((str-seq str-seq)
               (path path))
      (cond
        ((null? str-seq) path)  ; successful match
        ((if char-list
             (sxml:parse-check (car str-seq) path char-list)
             (sxml:parse-check (car str-seq) path))
         => (lambda (new-path)
              (loop (cdr str-seq) new-path)))
        (else #f)))))  ; unsuccessful match

; Similar to the 'parse-check' function. But this function also has a side
; effect. It displays an error message if the 'str' doesn't match the beginning
; of 'path'.
(define (sxml:parse-assert str path)
  (let loop ((lst (string->list str)) 
	     (p (sxml:skip-ws path)))
    (cond
      ((null? lst) p)
      ((null? p) 
       (sxml:xpointer-parse-error 
        "unexpected end of XPath expression. "
        "Expected - \"" str "\", given - \"" (list->string path) "\""))
      ((char=? (car lst) (car p)) (loop (cdr lst) (cdr p)))
      (else
       (sxml:xpointer-parse-error
        "expected - \"" str "\", given - \"" (list->string path) "\"")))))

             
;------------------------------------------------
; NCName readers

; Reads a NCName, taking into account that whitespaces and characters:
; ( ) < > [ ] : / + * , = | ! " ' @ $
; may not be used in it.
; Moreover, its first character can't be: . - or a digit
; The result:  (list  ncname  new-path)
;          or  #f
;  ncname - NCName represented as a string
; If there is no NCName in the current position of the path, then an error 
; message is displayed and #f is returned
(define (sxml:parse-ncname path)
  (let((path (sxml:skip-ws path)))
    (cond
      ((null? path) 
       (sxml:xpointer-parse-error
        "unexpected end of XPath expression. Expected - NCName"))
      ((sxml:non-first? (car path))
       (sxml:xpointer-parse-error
        "expected - NCName instead of " (car path)))
      (else
       (let loop ((ncname (list (car path)))
                  (path (cdr path)))
         (cond
           ((null? path) (list (list->string (reverse ncname)) path))
           ((memv (car path) sxml:delimiter)           
            (list (list->string (reverse ncname)) path))
           (else (loop (cons (car path) ncname) (cdr path)))))))))

; Reads a Name production. It is similar to a 'parse-ncname' function.
; The only difference is that #\: is allowed within a Name
(define (sxml:parse-name path)
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error
	 "unexpected end of XPath expression. Expected - Name"))
      ((and (sxml:non-first? (car path))
	    (not (char=? (car path) #\:)))
       (sxml:xpointer-parse-error "expected - Name instead of " (car path)))
      (else (let loop ((ncname (list (car path)))
		       (path (cdr path)))
	      (cond
		((null? path) 
		 (list (list->string (reverse ncname)) path))
		((and (memv (car path) sxml:delimiter)
		      (not (char=? (car path) #\:)))
		 (list (list->string (reverse ncname)) path))
		(else (loop (cons (car path) ncname) (cdr path)))))))))

; The function reads a qualified name (QName)
; Returns: ( (prefix . local-part) new-path )
;      or  ( local-part new-path )    if there is no prefix
;       if there is not QName in the beginning of the 'path' it calls 
;          sxml:xpointer-parse-error
;  prefix, local-part - strings
;  new-path - a list of characters
(define (sxml:parse-qname path)
  (and-let* ((r1 (sxml:parse-ncname path)))
	    (let ((first (car r1))
		  (path2 (cadr r1)))
	      (cond
		((null? path2) (list first path2))
		((not (char=? (car path2) #\:)) (list first path2))
		((null? (cdr path2))
		 (sxml:xpointer-parse-error "no local part of a qualified name"))
		((char=? (cadr path2) #\:) (list first path2))
		(else (and-let* ((r2 (sxml:parse-ncname (cdr path2))))
				(list (cons first (car r2)) (cadr r2)))
		      )))))
                   
;------------------------------------------------
; Parsers for data of basic types

; Reads a natural number:
; [1-9] [0-9]*
; The result:  (list  number  new-path)  or  #f
(define (sxml:parse-natural path)
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error
        "unexpected end of XPath expression. Expected - number"))
      ((or (char<? (car path) #\1) (char>? (car path) #\9))
       (sxml:xpointer-parse-error "expected - number instead of " (car path)))
      (else (let loop ((res (- (char->integer (car path))
			  48)) ; (char->integer #\0)
                  (path (cdr path)))
         (cond
           ((null? path) (list res path))
           ((char-numeric? (car path))
            (loop (+ (* res 10) (- (char->integer (car path)) 
				   48)) ; (char->integer #\0)
                  (cdr path)))
           (else (list res path))))))))

; Reads a Literal ([29] in XPath specification)
; [29]    Literal    ::=    '"' [^"]* '"'  
;                           | "'" [^']* "'"
; The result:  (string new-path)  or  #f
(define (sxml:parse-literal path)
  (let ((ch (if (sxml:parse-check "\"" path) #\" #\')))
    (let loop ((res '())
	       (path (sxml:parse-assert (if (char=? ch #\") "\"" "'") 
				       path)))
      (cond
	((not path) #f)
	((null? path)
	 (sxml:parse-assert (if (char=? ch #\") "\"" "'") 
			   path)
	 #f)
	((char=? (car path) ch)
	 (list (list->string (reverse res))
	       (cdr path)))
	(else (loop (cons (car path) res) (cdr path)))))))

; Reads a Number ([30]-[31] in XPath specification)
; [30]    Number    ::=    Digits ('.' Digits?)?  
;                          | '.' Digits  
; [31]    Digits    ::=    [0-9]+
; The result:  (number new-path)  or  #f
(define (sxml:parse-number path) 
  (define (digits path)
    (let loop ((n-lst '())
               (path path))
      (cond
        ((and (null? path) (null? n-lst))
         (sxml:xpointer-parse-error 
          "unexpected end of XPath expression. Expected - number"))
        ((null? path) (list n-lst path))
        ((and (or (char<? (car path) #\0) (char>? (car path) #\9))
              (null? n-lst))
         (sxml:xpointer-parse-error "expected - number instead of " (car path)))
        ((or (char<? (car path) #\0) (char>? (car path) #\9))
         (list n-lst path))
        (else
         (loop (cons (- (char->integer (car path)) (char->integer #\0)) n-lst)
               (cdr path))))))
    
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error 
        "unexpected end of XPath expression. Expected - number"))
      ((char=? (car path) #\.)
       (and-let*
        ((lst (digits (cdr path))))
        (let rpt ((res 0)
                  (n-lst (car lst))
                  (path (cadr lst)))
          (if (null? n-lst)
              (list (/ res 10) path)
              (rpt (+ (/ res 10) (car n-lst))
                   (cdr n-lst) 
                   path)))))
      (else
       (and-let*
        ((lst (digits path)))
        (let loop ((num1 0)
                   (n-lst (reverse (car lst)))
                   (path (cadr lst)))
          (if (null? n-lst)
              (cond
                ((or (null? path) (not (char=? (car path) #\.)))
                 (list num1 path))
                ((or (null? (cdr path))
                     (char<? (cadr path) #\0) (char>? (cadr path) #\9))
                 (list (exact->inexact num1) (cdr path)))
                (else
                 (and-let* ((lst2 (digits (cdr path))))
                           (let rpt ((num2 0)
                                     (n-lst (car lst2))
                                     (path (cadr lst2)))
                             (if (null? n-lst)
                                 (list (+ num1 (/ num2 10)) path)
                                 (rpt (+ (/ num2 10) (car n-lst))
                                      (cdr n-lst) 
                                      path))))))
              (loop (+ (* num1 10) (car n-lst))
                    (cdr n-lst)
                    path))))))))


;=========================================================================
; XPath/XPointer grammar parsing

; Returns the corresponding namespace URI or #f
; prefix - a symbol
(define (txp:resolve-ns-prefix prefix ns-binding)
  (cond
    ((assq prefix ns-binding)
     => cdr)
    (else
     (and (eq? prefix 'xml) "xml"))))

; Produces a parameterized parser
; txp-params - a long associative list of parameters which specify handlers
;  for different grammar rules. Precise content for 'txp-params' is discussed
;  iteratively in comments within function's body. However, 'txp-params' are
;  currently intended for TXPath developers only and are thus documented very
;  briefly
;
; The function returns an associative list:
; (list  (list  'xpath     xpath-implementation-res)
;        (list  'xpointer  xpointer-implementation-res)
;        (list  'expr      xpath-expression-implementation-res))
; xpath-implementation-res - XPath implementation produced, as was conducted
;  by 'txp-params'
; xpointer-implementation-res - XPointer implementation produced (for XPointer
;  grammar from W3C Candidate Recommendation 11 September 2001), as was
;  conducted by 'txp-params'
; xpath-expression-implementation-res - implementation for XPath Expr grammar
;  production
;
; NOTE: Future versions of this function may include additional members to the
; associative list which is returned as the result
(define (txp:parameterize-parser txp-params)
  (letrec
      (
       ; All these functions have similar arguments:
       ;  path - an xpath location path represented as a list of chars
       ;  ns-binding - declared namespace prefixes (not for all functions)
       ; ns-binding = (listof (prefix . uri))
       ; prefix - symbol, uri - string
       
       ;-------------------------------------------------
       ; Functions which parse XPath grammar
       
       ; Parses an AxisSpecifier production ([5],[6],[13] in XPath specification)
       ; [5]    AxisSpecifier    ::=    AxisName '::'  
       ;                                | AbbreviatedAxisSpecifier
       ; [6]    AxisName    ::=    'ancestor'  
       ;                           | 'ancestor-or-self'  
       ;                           | 'attribute'  
       ;                           | 'child'  
       ;                           | 'descendant'  
       ;                           | 'descendant-or-self'  
       ;                           | 'following'  
       ;                           | 'following-sibling'  
       ;                           | 'namespace'  
       ;                           | 'parent'  
       ;                           | 'preceding'  
       ;                           | 'preceding-sibling'  
       ;                           | 'self' 
       ; [13]    AbbreviatedAxisSpecifier    ::=    '@'? 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name = 'axis
       ;  param-value =
       ;   (list (list  'ancestor  (lambda (add-on) ...) )
       ;         (list  'ancestor-or-self  (lambda (add-on) ...) )
       ;         (list  'attribute  (lambda (add-on) ...) )
       ;         ...)  ; the remaining axes in the same manner
       (txp:parse-axis-specifier
        (let* ((axis-param-value (txp:param-value 'axis txp-params))
               (child-impl (txp:param-value 'child axis-param-value))
               (parser-pairs
                (cons
                 `(("@") ,(txp:param-value 'attribute axis-param-value))
                 (map
                  (lambda (single-pair)
                    (list
                     (list (symbol->string (car single-pair)) "::")
                     (cadr single-pair)))
                  axis-param-value))))
          (lambda (path ns-binding add-on)   ; ns-binding is dummy here
            (let loop ((pairs parser-pairs))
              (cond
                ((null? pairs)  ; a default (child) axis
                 (list (child-impl add-on) path))
                ((sxml:parse-check-sequence (caar pairs) path)
                 => (lambda (path)
                      (list ((cadar pairs) add-on) path)))
                (else  ; continue loop
                 (loop (cdr pairs))))))))
       
       ; Parses a NodeTest production 
       ; ([7],[37] in XPath specification, [11] in XPointer specification)
       ; [7]    NodeTest    ::=    NameTest  
       ;                           | NodeType '(' ')'  
       ;                           | 'processing-instruction' '(' Literal ')' 
       ; [37]    NameTest    ::=    '*'  
       ;                            | NCName ':' '*'  
       ;                            | QName  
       ; [11]   NodeType   ::=   'comment'  
       ;                         | 'text'  
       ;                         | 'processing-instruction'  
       ;                         | 'node'
       ;                         | 'point'
       ;                         | 'range'
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'node-test
       ;  param-value ::=
       ;   (list (list  'star  (lambda (add-on) ...) )
       ;         (list  'uri+star  (lambda (uri add-on) ...) )
       ;         (list  'qname  (lambda (uri local-name add-on) ...) )
       ;         (list  'comment  (lambda (add-on) ...) )
       ;         (list  'text  (lambda (add-on) ...) )
       ;         (list  'processing-instruction
       ;                (lambda (literal-string add-on) ...) )
       ;         (list  'node  (lambda (add-on) ...) )
       ;         (list  'point  (lambda (add-on) ...) )
       ;         (list  'range  (lambda (add-on) ...) ))
       ; uri - a string or #f (the latter is possible for 'qname only)
       ; local-name - a string
       ; literal - a string
       (txp:parse-node-test
        (let* ((ntest-param-value (txp:param-value 'node-test txp-params))
               (star-impl (txp:param-value 'star ntest-param-value))
               (uri+star-impl (txp:param-value 'uri+star ntest-param-value))
               (qname-impl (txp:param-value 'qname ntest-param-value))
               (comment-impl (txp:param-value 'comment ntest-param-value))
               (text-impl (txp:param-value 'text ntest-param-value))
               (pi-impl
                (txp:param-value 'processing-instruction ntest-param-value))
               (node-impl (txp:param-value 'node ntest-param-value))
               (point-impl (txp:param-value 'point ntest-param-value))
               (range-impl (txp:param-value 'range ntest-param-value))
               (brackets
                (lambda (path)
                  (and-let* ((path (sxml:parse-assert "(" path)))
                            (sxml:parse-assert ")" path)))))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check-sequence '("comment" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (comment-impl add-on) path))))
              ((sxml:parse-check-sequence '("text" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (text-impl add-on) path))))
              ((sxml:parse-check-sequence '("node" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                          (list (node-impl add-on) path))))
              ((sxml:parse-check-sequence '("processing-instruction" "(") path)
               => (lambda (path)
                    (cond
                      ((sxml:parse-check ")" path)
                       => (lambda (path)
                            (list (pi-impl #f add-on) path)))
                      (else
                       (and-let*
                        ((lst (sxml:parse-literal path))
                         (name (car lst))
                         (path (sxml:parse-assert ")" (cadr lst))))
                        (list (pi-impl name add-on) path))))))
              ((sxml:parse-check-sequence '("point" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (point-impl add-on) path))))
              ((sxml:parse-check-sequence '("range" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (range-impl add-on) path))))
              ((sxml:parse-check "*" path)
               => (lambda (path)
                    (list (star-impl add-on) path)))
              (else  ; NCName ':' '*'  |  QName
               (and-let*
                ((lst (sxml:parse-ncname path)))
                (let ((path (cadr lst)))
                  (if
                   (or (null? path) (not (char=? (car path) #\:))) ; local name
                   (list (qname-impl #f (car lst) add-on) path)
                   (let* ((name (string->symbol (car lst)))
                          (path (sxml:parse-assert ":" path))
                          (uri  (txp:resolve-ns-prefix name ns-binding)))
                     (cond
                       ((not uri)
                        (sxml:xpointer-parse-error
                         "unknown namespace prefix - " name))
                       ((and (not (null? path)) (char=? (car path) #\*))
                        (list
                         (uri+star-impl uri add-on)
                         (sxml:parse-assert "*" path)))
                       (else
                        (and-let*
                         ((lst (sxml:parse-ncname path)))
                         (list
                          (qname-impl uri (car lst) add-on)
                          (cadr lst))))))))))))))
                
       ; Parses a Step production 
       ; ([4xptr] in XPointer specification, [12] in XPath specification)
       ; [4xptr] Step ::= AxisSpecifier NodeTest Predicate*
       ;                  | AbbreviatedStep
       ;                  | 'range-to' '(' Expr ')' Predicate*
       ; [12]    AbbreviatedStep    ::=    '.'  
       ;                                   | '..' 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'step
       ;  param-value ::=
       ;   (list
       ;    (list  'common
       ;      (lambda (axis-res node-test-res predicate-res-lst add-on) ...) )
       ;    (list  'range-to
       ;      (lambda (expr-res predicate-res-lst add-on) ...) ))
       (txp:parse-step
        (let* ((step-param-value (txp:param-value 'step txp-params))
               (common-value (txp:param-value 'common step-param-value))
               (range-to-value (txp:param-value 'range-to step-param-value))
               (axis-param-value (txp:param-value 'axis txp-params))
               (self-value (txp:param-value 'self axis-param-value))
               (parent-value (txp:param-value 'parent axis-param-value))
               (ntest-param-value (txp:param-value 'node-test txp-params))
               (node-value (txp:param-value 'node ntest-param-value)))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check ".." path)
               (list
                (common-value (parent-value add-on)
                              (node-value add-on) '() add-on)
                (sxml:parse-assert ".." path)))
              ((sxml:parse-check "." path)
               (list
                (common-value (self-value add-on)
                              (node-value add-on) '() add-on)
                (sxml:parse-assert "." path)))
              ((sxml:parse-check "range-to" path)
               (and-let*
                ((path0
                  (sxml:parse-assert "(" (sxml:parse-assert "range-to" path)))
                 (lst (txp:parse-expr path0 ns-binding add-on))
                 (path (sxml:parse-assert ")" (cadr lst))))
                (let ((expr-res (car lst)))
                  (let loop ((path path)
                             (pred-lst '()))
                    (if
                     (sxml:parse-check "[" path)
                     (and-let*
                      ((lst (txp:parse-predicate path ns-binding add-on)))
                      (loop (cadr lst)
                            (cons (car lst) pred-lst)))
                     ; Predicates are over
                     (list
                      (if
                       (apply txp:semantic-errs-detected?
                              (cons expr-res pred-lst))
                       'txp:semantic-error
                       (range-to-value expr-res (reverse pred-lst) add-on))
                      path))))))
              (else  ; common implementation
               (and-let*
                ((lst (txp:parse-axis-specifier path ns-binding add-on)))
                (let ((axis (car lst)))
                  (and-let*
                   ((lst (txp:parse-node-test (cadr lst) ns-binding add-on)))
                   (let ((test (car lst)))
                     (let loop ((preds '())
                                (path (cadr lst)))
                       (if
                        (sxml:parse-check "[" path)
                        (and-let*
                         ((lst (txp:parse-predicate path ns-binding add-on)))
                         (loop (cons (car lst) preds)
                               (cadr lst)))
                        ; No more predicates                   
                        (list
                         (if (or (txp:semantic-errs-detected? axis test)
                                 (apply txp:semantic-errs-detected? preds))
                             'txp:semantic-error
                             (common-value axis test (reverse preds) add-on))
                         path))))))))))))

       ; Parses a RelativeLocationPath production ([3],[11] in
       ; XPath specification)
       ; [3]  RelativeLocationPath  ::=  Step  
       ;                                 | RelativeLocationPath '/' Step  
       ;                                 | AbbreviatedRelativeLocationPath 
       ; [11]  AbbreviatedRelativeLocationPath  ::=
       ;                                    RelativeLocationPath '//' Step
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'relative-lpath
       ;  param-value ::= (lambda (step-res-lst add-on) ...)
       (txp:parse-relative-location-path
        (let* ((relative-lpath-value
                (txp:param-value 'relative-lpath txp-params))
               (step-param-value (txp:param-value 'step txp-params))
               (common-value (txp:param-value 'common step-param-value))
               (axis-param-value (txp:param-value 'axis txp-params))
               (descendant-or-self-value
                (txp:param-value 'descendant-or-self axis-param-value))
               (ntest-param-value (txp:param-value 'node-test txp-params))
               (node-value (txp:param-value 'node ntest-param-value)))
          (lambda (path ns-binding add-on)
            (let loop ((step-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-step path ns-binding add-on)))
               (let ((step-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "//" path)
                    (loop
                     (cons
                      ; // = /descendant-or-self::node()/
                      (common-value
                       (descendant-or-self-value add-on)
                       (node-value add-on) '() add-on)
                      (cons step-res step-res-lst))
                     (sxml:parse-assert "//" path)))
                   ((sxml:parse-check "/" path)
                    (loop (cons step-res step-res-lst)
                          (sxml:parse-assert "/" path)))                          
                   (else  ; no more steps
                    (list
                     (if
                      (apply txp:semantic-errs-detected? step-res-lst)
                      'txp:semantic-error
                      (relative-lpath-value
                       (reverse (cons step-res step-res-lst)) add-on))
                     path)))))))))

       ; Parses a LocationPath production ([1],[2],[10] in XPath specification)
       ; [1]    LocationPath    ::=    RelativeLocationPath  
       ;                               | AbsoluteLocationPath  
       ; [2]    AbsoluteLocationPath    ::=   '/' RelativeLocationPath?  
       ;                                      | AbbreviatedAbsoluteLocationPath
       ; [10]    AbbreviatedAbsoluteLocationPath    ::=
       ;                                              '//' RelativeLocationPath
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'location-path
       ;  param-value ::=
       ;   (list
       ;    (list  'bare-slash  (lambda (add-on) ...) )
       ;    (list  'slash  (lambda (relative-lpath-res add-on) ...) )
       ;    (list  'double-slash  (lambda (relative-lpath-res add-on) ...) ))
       (txp:parse-location-path
        (let* ((location-path-value
                (txp:param-value 'location-path txp-params))
               (bare-slash-value
                (txp:param-value 'bare-slash location-path-value))
               (slash-value
                (txp:param-value 'slash location-path-value))
               (double-slash-value
                (txp:param-value 'double-slash location-path-value))               
               (nothing?  ; whether no relative location path follows '/'
                (lambda (path)
                  (let ((path (sxml:skip-ws path)))
                    (cond
                      ((null? path) #t)
                      ((memv (car path)
                             '(#\| #\+ #\- #\< #\> #\= #\) #\] #\,)) #t)
                      ((or (sxml:parse-check "mod" path sxml:delimiter)
                           (sxml:parse-check "div" path sxml:delimiter)
                           (sxml:parse-check "!=" path)
                           (sxml:parse-check "and" path sxml:delimiter)
                           (sxml:parse-check "or" path sxml:delimiter)) #t)
                      (else #f))))))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check "//" path)
               (and-let*
                ((lst (txp:parse-relative-location-path
                       (sxml:parse-assert "//" path) ns-binding add-on)))
                (let ((relative-res (car lst))
                      (path (cadr lst)))
                  (list
                   (if (txp:semantic-errs-detected? relative-res)
                       'txp:semantic-error
                       (double-slash-value relative-res add-on))
                   path))))
              ((sxml:parse-check "/" path)
               => (lambda (path)
                    (if (nothing? path)
                        (list (bare-slash-value add-on) path)
                        (and-let*
                         ((lst (txp:parse-relative-location-path
                                path ns-binding add-on)))
                         (let ((relative-res (car lst))
                               (path (cadr lst)))
                           (list
                            (if (txp:semantic-errs-detected? relative-res)
                                'txp:semantic-error
                                (slash-value relative-res add-on))
                            path))))))
              (else  ; Location path is a Relative location path
               (txp:parse-relative-location-path path ns-binding add-on))))))

       ; Parses a Predicate production ([8]-[9] in XPath specification)
       ; [8]    Predicate    ::=    '[' PredicateExpr ']'  
       ; [9]    PredicateExpr    ::=    Expr 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'predicate
       ;  param-value ::= (lambda (expr-res add-on) ...)
       (txp:parse-predicate
        (let ((predicate-value (txp:param-value 'predicate txp-params)))
          (lambda (path ns-binding add-on)
            (and-let*
             ((path0 (sxml:parse-assert "[" path))
              (lst (txp:parse-expr path0 ns-binding add-on))
              (path (sxml:parse-assert "]" (cadr lst))))
             (list
              (if (txp:semantic-errs-detected? (car lst))
                  'txp:semantic-error
                  (predicate-value (car lst) add-on))
              path)))))

       ; Parses a VariableReference production ([36] in XPath specification)
       ; [36]    VariableReference    ::=    '$' QName 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'variable-ref
       ;  param-value ::= (lambda (var-name-string add-on) ...)
       (txp:parse-variable-reference  
        (let ((var-ref-value (txp:param-value 'variable-ref txp-params)))
          (lambda (path ns-binding add-on)
            (and-let*
             ((path (sxml:parse-assert "$" path))
              (lst (sxml:parse-qname path)))
             (let ((name              
                    (if (pair? (car lst))  ; contains a prefix-part
                        (string-append (caar lst) ":" (cdar lst))
                        (car lst))))
               (list (var-ref-value name add-on) (cadr lst)))))))

       ; Parses a FunctionCall production ([16],[17],[35] in
       ; XPath specification)
       ; [16]    FunctionCall    ::=    FunctionName 
       ;                                '(' ( Argument ( ',' Argument )* )? ')'
       ; [17]    Argument    ::=    Expr 
       ; [35]    FunctionName    ::=    QName - NodeType
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'function-call
       ;  param-value ::= (lambda (fun-name-string arg-res-lst add-on) ...)
       ;
       ; NOTE: prefix resolution for qualified function names not implemented
       (txp:parse-function-call
        (let ((fun-call-value (txp:param-value 'function-call txp-params))
              (parse-arguments
               ; Returns (list (listof arg-res) new-path)
               (lambda (path ns-binding add-on)
                 (and-let*
                  ((path (sxml:parse-assert "(" path)))
                  (cond	
                    ((sxml:parse-check ")" path)
                      => (lambda (path) (list '() path)))
                    (else
                     (let single-arg ((arg-res-lst '())
                                      (path path))
                       (and-let*
                        ((lst (txp:parse-expr path ns-binding add-on)))
                        (let ((arg-res (car lst))
                              (path (cadr lst)))
                          (cond
                            ((sxml:parse-check ")" path)
                             => (lambda (path)
                                  (list (reverse (cons arg-res arg-res-lst))
                                        path)))
                            (else
                             (and-let*
                              ((path (sxml:parse-assert "," path)))
                              (single-arg
                               (cons arg-res arg-res-lst) path)))))))))))))
          (lambda (path ns-binding add-on)
            (and-let*
             ((lst (sxml:parse-qname path)))
             (let ((fun-name (car lst)))  ; can be a pair
               (and-let*
                ((lst (parse-arguments (cadr lst) ns-binding add-on)))
                (let ((arg-res-lst (car lst))
                      (path (cadr lst)))
                  (list
                   (if (apply txp:semantic-errs-detected? arg-res-lst)
                       'txp:semantic-error
                       (fun-call-value
                        (if (pair? fun-name)  ; a prefix and a local part
                            (string-append (car fun-name) ":" (cdr fun-name))
                            fun-name)
                        arg-res-lst add-on))
                   path))))))))
                     
       ; Parses a PrimaryExpr production ([15] in XPath specification)
       ; [15]    PrimaryExpr    ::=    VariableReference  
       ;                               | '(' Expr ')'  
       ;                               | Literal  
       ;                               | Number  
       ;                               | FunctionCall 
       ; [29]    Literal    ::=    '"' [^"]* '"'  
       ;                           | "'" [^']* "'"  
       ; [30]    Number    ::=    Digits ('.' Digits?)?  
       ;                          | '.' Digits  
       ; [31]    Digits    ::=    [0-9]+ 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'primary-expr
       ;  param-value ::= 
       ;   (list  (list  'literal  (lambda (literal add-on) ...) )
       ;          (list  'number   (lambda (number add-on) ...)  ))
       (txp:parse-primary-expr
        (let* ((primary-expr-value (txp:param-value 'primary-expr txp-params))
               (literal-value (txp:param-value 'literal primary-expr-value))
               (number-value (txp:param-value 'number primary-expr-value)))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check "$" path)  ; a VariableReference
               (txp:parse-variable-reference path ns-binding add-on))
              ((sxml:parse-check "(" path)  ; an '(' Expr ')'
               (and-let*
                ((lst (txp:parse-expr
                       (sxml:parse-assert "(" path) ns-binding add-on))
                 (path (sxml:parse-assert ")" (cadr lst))))
                (let ((expr-res (car lst)))
                  (list expr-res path))))
              ((or (sxml:parse-check "\"" path)
                   (sxml:parse-check "'" path))  ; a Literal
               (and-let*
                ((lst (sxml:parse-literal path)))
                (list
                 (literal-value (car lst) add-on)
                 (cadr lst))))
              ((let ((p (sxml:skip-ws path)))  ; a Number?
                 (cond ((null? p) #f)
                       ((char=? (car p) #\.) #t)
                       ((and (char>=? (car p) #\0) (char<=? (car p) #\9)) #t)
                       (else #f)))
               (and-let*
                ((lst (sxml:parse-number path)))                               
                (list
                 (number-value (car lst) add-on)	   
                 (cadr lst))))
              (else   ; a Function call
               (txp:parse-function-call path ns-binding add-on))))))

       ; Parses a FilterExpr production ([20] in XPath specification)
       ; [20]    FilterExpr    ::=    PrimaryExpr  
       ;                              | FilterExpr Predicate 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'filter-expr
       ;  param-value ::=
       ;            (lambda (primary-expr-res predicate-res-lst add-on) ...) )
       (txp:parse-filter-expr
        (let ((filter-expr-value (txp:param-value 'filter-expr txp-params)))
          (lambda (path ns-binding add-on)
            (and-let*
             ((lst (txp:parse-primary-expr path ns-binding add-on)))
             (let ((prim-res (car lst)))
               (let loop ((pred-res-lst '())
                          (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "[" path)
                    (and-let*
                     ((lst (txp:parse-predicate path ns-binding add-on)))
                     (loop (cons (car lst) pred-res-lst)
                           (cadr lst))))
                   ; No more predicates
                   ((null? pred-res-lst) (list prim-res path))
                   (else              
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons prim-res pred-res-lst))
                      'txp:semantic-error
                      (filter-expr-value prim-res (reverse pred-res-lst) add-on))
                     path)))))))))

       ; Parses a PathExpr production ([19] in XPath specification)
       ; [19]    PathExpr    ::=    LocationPath  
       ;                            | FilterExpr  
       ;                            | FilterExpr '/' RelativeLocationPath  
       ;                            | FilterExpr '//' RelativeLocationPath
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'path-expr
       ;  param-value ::=
       ;   (list
       ;    (list  'slash
       ;           (lambda (filter-expr-res relative-lpath-res add-on) ...) )
       ;    (list  'double-slash
       ;           (lambda (filter-expr-res relative-lpath-res add-on) ...) ))
       (txp:parse-path-expr
         (let ((filter-expr?
                (lambda (path)
                  (let ((path (sxml:skip-ws path)))
                    (cond
                      ((null? path) #f)
                      ((member 
                        (car path) 
                        '(#\$ #\( #\" #\' #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                       #t)
                      ((char=? (car path) #\.)
                       (cond
                         ((null? (cdr path)) #f)
                         ((member
                           (cadr path)
                           '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                          #t)
                         (else #f)))
                      ((member
                        (car path)
                        '(#\) #\< #\> #\[ #\] #\/ #\+ #\* #\, #\= #\| #\! #\@ #\-))
                       #f)
                      (else
                       (let ((lst (sxml:parse-ncname path)))
                         (cond
                           ((not lst) #f)
                           ((sxml:parse-check "::" (cadr lst)) #f)
                           (else
                            (and-let*
                             ((lst (sxml:parse-name path)))
                             (let ((name (car lst))
                                   (new-path (sxml:skip-ws (cadr lst))))
                               (cond
                                 ((string=? name "range-to") #f)
                                 ((string=? name "comment") #f)
                                 ((string=? name "text") #f)
                                 ((string=? name "processing-instruction") #f)
                                 ((string=? name "node") #f)
                                 ((string=? name "point") #f)
                                 ((string=? name "range") #f)
                                 ((null? new-path) #f)
                                 ((char=? (car new-path) #\() #t)
                                 (else #f)))))))))))))
           (let* ((path-expr-value (txp:param-value 'path-expr txp-params))
                  (slash-value (txp:param-value 'slash path-expr-value))
                  (double-slash-value
                   (txp:param-value 'double-slash path-expr-value)))
             (lambda (path ns-binding add-on)
               (if
                (not (filter-expr? path))
                (txp:parse-location-path path ns-binding add-on)
                (and-let*
                 ((lst (txp:parse-filter-expr path ns-binding add-on)))
                 (let ((filter-ex-res (car lst))
                       (path (cadr lst)))
                   (cond
                     ((sxml:parse-check "//" path)
                      (and-let*
                       ((lst2
                         (txp:parse-relative-location-path
                          (sxml:parse-assert "//" path) ns-binding add-on)))
                       (let ((rel-lpath-res (car lst2))
                             (path (cadr lst2)))
                         (list
                          (if
                           (txp:semantic-errs-detected?
                            filter-ex-res rel-lpath-res)
                           'txp:semantic-error
                           (double-slash-value
                            filter-ex-res rel-lpath-res add-on))
                          path))))
                     ((sxml:parse-check "/" path)
                      (and-let*
                       ((lst2
                         (txp:parse-relative-location-path
                          (sxml:parse-assert "/" path) ns-binding add-on)))
                       (let ((rel-lpath-res (car lst2))
                             (path (cadr lst2)))
                         (list
                          (if
                           (txp:semantic-errs-detected?
                            filter-ex-res rel-lpath-res)
                           'txp:semantic-error
                           (slash-value filter-ex-res rel-lpath-res add-on))
                          path))))
                     (else  ; A single filter expression, not followed by lpath
                      lst)))))))))

       ; Parses a UnionExpr production ([18] in XPath specification)
       ; [18]    UnionExpr    ::=    PathExpr  
       ;                             | UnionExpr '|' PathExpr
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'union-expr
       ;  param-value ::= (lambda (path-expr-res-lst add-on) ...)
       (txp:parse-union-expr
        (let ((union-expr-value (txp:param-value 'union-expr txp-params)))              
          (lambda (path ns-binding add-on)
            (let loop ((p-e-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-path-expr path ns-binding add-on)))
               (let ((p-e-res (car lst))
                     (path (cadr lst)))
                 (let ((new-path (sxml:parse-check "|" path)))
                   (cond
                     (new-path  ; more PathExprs
                      (loop (cons p-e-res p-e-res-lst) new-path))
                     ; no more PathExprs
                     ((null? p-e-res-lst)  ; only one PathExpr                                
                      (list p-e-res path))
                     (else  ; several Path-exprs
                      (list
                       (if
                        (apply txp:semantic-errs-detected?
                               (cons p-e-res p-e-res-lst))
                        'txp:semantic-error
                        (union-expr-value
                         (reverse (cons p-e-res p-e-res-lst)) add-on))
                       path))))))))))
 
       ; Parses a UnaryExpr production ([27] in XPath specification)
       ; [27]    UnaryExpr    ::=    UnionExpr  
       ;                             | '-' UnaryExpr 
       ; Note that the grammar allows multiple unary minuses
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'unary-expr
       ;  param-value ::= (lambda (union-expr-res num-minuses add-on) ...)
       (txp:parse-unary-expr
        (let ((unary-expr-value (txp:param-value 'unary-expr txp-params)))              
          (lambda (path ns-binding add-on)
            (if (not (sxml:parse-check "-" path))
                (txp:parse-union-expr path ns-binding add-on)
                (let loop ((num-minuses 0) (path path))
                  (let ((new-path (sxml:parse-check "-" path)))
                    (if new-path   ; more minuses
                        (loop (+ num-minuses 1) new-path)               
                        (and-let*
                         ((lst (txp:parse-union-expr path ns-binding add-on)))
                         (let ((union-expr-res (car lst))
                               (path (cadr lst)))
                           (list
                            (if
                             (txp:semantic-errs-detected? union-expr-res)
                             'txp:semantic-error
                             (unary-expr-value
                              union-expr-res num-minuses add-on))
                            path))))))))))
                     			
       ; Parses a MultiplicativeExpr production ([26],[34] in
       ; XPath specification)
       ; [26] MultiplicativeExpr  ::=
       ;                 UnaryExpr  
       ;                 | MultiplicativeExpr MultiplyOperator UnaryExpr
       ;                 | MultiplicativeExpr 'div' UnaryExpr  
       ;                 | MultiplicativeExpr 'mod' UnaryExpr 
       ; [34] MultiplyOperator  ::=  '*'
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'mul-expr
       ;  param-value ::= (lambda (unary-expr-res-lst op-lst add-on) ...)
       (txp:parse-multiplicative-expr
        (let* ((mul-expr-value (txp:param-value 'mul-expr txp-params))
               (operations-value (txp:param-value 'operations txp-params))
               (multiply-value (txp:param-value '* operations-value))
               (div-value (txp:param-value 'div operations-value))
               (mod-value (txp:param-value 'mod operations-value)))
          (lambda (path ns-binding add-on)
            (let loop ((unary-expr-res-lst '())
                       (op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-unary-expr path ns-binding add-on)))
               (let ((unary-expr-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "*" path)
                    (loop (cons unary-expr-res unary-expr-res-lst)
                          (cons (multiply-value add-on) op-lst)
                          (sxml:parse-assert "*" path)))
                   ((sxml:parse-check "div" path sxml:delimiter)
                    (loop (cons unary-expr-res unary-expr-res-lst)
                          (cons (div-value add-on) op-lst)
                          (sxml:parse-assert "div" path)))
                   ((sxml:parse-check "mod" path sxml:delimiter)
                    (loop (cons unary-expr-res unary-expr-res-lst)
                          (cons (mod-value add-on) op-lst)
                          (sxml:parse-assert "mod" path)))
                   ; no more UnaryExprs
                   ((null? unary-expr-res-lst)  ; single UnaryExpr
                    lst)
                   (else   ; several UnaryExprs
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons unary-expr-res unary-expr-res-lst))
                      'txp:semantic-error
                      (mul-expr-value
                       (reverse (cons unary-expr-res unary-expr-res-lst))
                       (reverse op-lst) add-on))
                     path)))))))))
              
       ; Parses a AdditiveExpr production ([25] in XPath specification)
       ; [25]    AdditiveExpr    ::=    MultiplicativeExpr  
       ;                                | AdditiveExpr '+' MultiplicativeExpr  
       ;                                | AdditiveExpr '-' MultiplicativeExpr 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'add-expr
       ;  param-value ::= (lambda (mul-expr-res-lst op-lst add-on) ...)
       (txp:parse-additive-expr
        (let* ((add-expr-value (txp:param-value 'add-expr txp-params))
               (operations-value (txp:param-value 'operations txp-params))
               (plus-value (txp:param-value '+ operations-value))
               (minus-value (txp:param-value '- operations-value)))
          (lambda (path ns-binding add-on)
            (let loop ((mul-expr-res-lst '())
                       (op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-multiplicative-expr path ns-binding add-on)))
               (let ((mul-expr-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "+" path)
                    (loop (cons mul-expr-res mul-expr-res-lst)
                          (cons (plus-value add-on) op-lst)
                          (sxml:parse-assert "+" path)))
                   ((sxml:parse-check "-" path)
                    (loop (cons mul-expr-res mul-expr-res-lst)
                          (cons (minus-value add-on) op-lst)
                          (sxml:parse-assert "-" path)))
                   ; no more MultiplicativeExprs
                   ((null? mul-expr-res-lst)  ; single MultiplicativeExpr
                    lst)
                   (else   ; several MultiplicativeExprs
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons mul-expr-res mul-expr-res-lst))
                      'txp:semantic-error
                      (add-expr-value
                       (reverse (cons mul-expr-res mul-expr-res-lst))
                       (reverse op-lst) add-on))
                     path)))))))))
       
       ; Parses a RelationalExpr production ([24] in XPath specification)
       ; [24]    RelationalExpr    ::=    AdditiveExpr  
       ;                                  | RelationalExpr '<' AdditiveExpr
       ;                                  | RelationalExpr '>' AdditiveExpr
       ;                                  | RelationalExpr '<=' AdditiveExpr
       ;                                  | RelationalExpr '>=' AdditiveExpr
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'relational-expr
       ;  param-value ::=
       ;           (lambda (additive-expr-res-lst cmp-op-lst add-on) ...)
       (txp:parse-relational-expr
        (let* ((rel-expr-value (txp:param-value 'relational-expr txp-params))
               (operations-value (txp:param-value 'operations txp-params))
               (ls-value (txp:param-value '< operations-value))
               (gt-value (txp:param-value '> operations-value))
               (le-value (txp:param-value '<= operations-value))
               (ge-value (txp:param-value '>= operations-value)))                              
          (lambda (path ns-binding add-on)
            (let loop ((add-res-lst '())
                       (cmp-op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-additive-expr path ns-binding add-on)))
               (let ((add-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "<=" path)
                    (loop (cons add-res add-res-lst)
                          (cons (le-value add-on) cmp-op-lst)
                          (sxml:parse-assert "<=" path)))
                   ((sxml:parse-check ">=" path)
                    (loop (cons add-res add-res-lst)
                          (cons (ge-value add-on) cmp-op-lst)
                          (sxml:parse-assert ">=" path)))
                   ((sxml:parse-check "<" path)
                    (loop (cons add-res add-res-lst)
                          (cons (ls-value add-on) cmp-op-lst)
                          (sxml:parse-assert "<" path)))
                   ((sxml:parse-check ">" path)
                    (loop (cons add-res add-res-lst)
                          (cons (gt-value add-on) cmp-op-lst)
                          (sxml:parse-assert ">" path)))
                   ; no more AdditiveExprs                   
                   ((null? add-res-lst) ; single AdditiveExpr
                    lst)
                   (else   ; several AdditiveExprs
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons add-res add-res-lst))
                      'txp:semantic-error
                      (rel-expr-value
                       (reverse (cons add-res add-res-lst))
                       (reverse cmp-op-lst) add-on))
                     path)))))))))
       
       ; Parses an EqualityExpr production ([23] in XPath specification)
       ; [23]    EqualityExpr    ::=    RelationalExpr
       ;                                | EqualityExpr '=' RelationalExpr
       ;                                | EqualityExpr '!=' RelationalExpr
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'equality-expr
       ;  param-value ::=
       ;           (lambda (relational-expr-res-lst cmp-op-lst add-on) ...)
       (txp:parse-equality-expr
        (let* ((equality-expr-value
                (txp:param-value 'equality-expr txp-params))
               (operations-value
                (txp:param-value 'operations txp-params))
               (equal-value (txp:param-value '= operations-value))
               (not-equal-value (txp:param-value '!= operations-value)))
          (lambda (path ns-binding add-on)
            (let loop ((rel-res-lst '())
                       (cmp-op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-relational-expr path ns-binding add-on)))
               (let ((rel-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "=" path)
                    (loop (cons rel-res rel-res-lst)
                          (cons (equal-value add-on) cmp-op-lst)
                          (sxml:parse-assert "=" path)))
		  ((sxml:parse-check "!=" path)
		   (loop (cons rel-res rel-res-lst)
                         (cons (not-equal-value add-on) cmp-op-lst)
			 (sxml:parse-assert "!=" path)))
		  ; no more RelationalExprs
                  ((null? rel-res-lst) ; only one RelationalExpr
                   lst)
                  (else  ; several RelationalExprs
                   (list
                    (if
                     (apply txp:semantic-errs-detected?
                            (cons rel-res rel-res-lst))
                      'txp:semantic-error
                      (equality-expr-value
                       (reverse (cons rel-res rel-res-lst))
                       (reverse cmp-op-lst) add-on))
                     path)))))))))
                   
       ; Parses an AndExpr production ([22] in XPath specification)
       ; [22]    AndExpr    ::=    EqualityExpr
       ;                           | AndExpr 'and' EqualityExpr
       ; Note that according to 3.4 in XPath specification, the right operand
       ; is not evaluated if the left operand evaluates to false
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'and-expr
       ;  param-value ::= (lambda (equality-expr-res-lst add-on) ...)
       (txp:parse-and-expr
        (let ((and-expr-value (txp:param-value 'and-expr txp-params)))
          (lambda (path ns-binding add-on)
            (let loop ((equality-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-equality-expr path ns-binding add-on)))
               (let ((equality-res (car lst))
                     (path (cadr lst)))
                 (let ((new-path (sxml:parse-check "and" path sxml:delimiter)))
                   (cond
                     (new-path
                      (loop (cons equality-res equality-res-lst) new-path))
                     ; no more EqualityExprs
                     ((null? equality-res-lst)  ; only one EqualityExpr
                      lst)
                     (else  ; several EqualityExprs
                      (list
                       (if
                        (apply txp:semantic-errs-detected?
                               (cons equality-res equality-res-lst))
                        'txp:semantic-error
                        (and-expr-value
                         (reverse (cons equality-res equality-res-lst))
                         add-on))
                     path))))))))))
                  
       ; Parses an Expr production ([14],[21] in XPath specification)
       ; [14]    Expr    ::=    OrExpr 
       ; [21]    OrExpr    ::=    AndExpr  
       ;                          | OrExpr 'or' AndExpr
       ; Note that according to 3.4 in XPath specification, the right operand
       ; is not evaluated if the left operand evaluates to true
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'or-expr
       ;  param-value ::= (lambda (and-expr-res-lst add-on) ...)
       (txp:parse-expr
        (let ((or-expr-value (txp:param-value 'or-expr txp-params)))
          (lambda (path ns-binding add-on)
            (let loop ((and-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-and-expr path ns-binding add-on)))
               (let ((and-res (car lst))
                     (path (cadr lst)))
                 (let ((new-path (sxml:parse-check "or" path sxml:delimiter)))
                   (cond
                     (new-path
                      (loop (cons and-res and-res-lst) new-path))
                     ; no more AndExprs
                     ((null? and-res-lst)  ; only one AndExpr
                      lst)
                     (else  ; several AndExprs
                      (list
                       (if
                        (apply txp:semantic-errs-detected?
                               (cons and-res and-res-lst))
                        'txp:semantic-error
                        (or-expr-value
                         (reverse (cons and-res and-res-lst)) add-on))
                      path))))))))))
       
       ;------------------------------------------------
       ; Functions which parse XPointer grammar
       
       ; Parses an FullXPtr production ([3]-[10] in XPointer specification)
       ; [3]    FullXPtr    ::=    XPtrPart (S? XPtrPart)* 
       ; [4]    XPtrPart    ::=    'xpointer' '(' XPtrExpr ')'
       ;                           | 'xmlns' '(' XPtrNsDecl? ')' 
       ;                           | Scheme '(' SchemeSpecificExpr ')' 
       ; [5]    Scheme    ::=    NCName 
       ; [6]    SchemeSpecificExpr    ::=    StringWithBalancedParens 
       ; [7]    StringWithBalancedParens    ::=
       ;                    [^()]* ('(' StringWithBalancedParens ')' [^()]*)*
       ; [8]    XPtrExpr    ::=    Expr
       ; [9]    XPtrNsDecl    ::=    NCName S? '=' S? XPtrNsURI 
       ; [10]    XPtrNsURI    ::=    Char*
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'full-xptr
       ;  param-value ::= (lambda (expr-res-lst add-on) ...)
       (txp:parse-full-xptr
        (let ((full-xptr-value (txp:param-value 'full-xptr txp-params)))
          (lambda (path ns-binding add-on)
            (let loop ((expr-res-lst '())
                       (ns-binding ns-binding)
                       (path path))
              (if
               (null? (sxml:skip-ws path))  ; the string is over               
               (cond
                 ((= (length expr-res-lst) 1)  ; a single XPointer part
                  (car expr-res-lst))
                 ((apply txp:semantic-errs-detected? expr-res-lst)
                  'txp:semantic-error)
                 (else
                  (full-xptr-value (reverse expr-res-lst) add-on)))
               (and-let*
                ((lst (sxml:parse-name path))
                 (name (car lst))
                 (path (cadr lst)))
                (cond
                  ((string=? name "xpointer")  ; xpointer part
                   (and-let*
                    ((path (sxml:parse-assert "(" path))
                     (lst2 (txp:parse-expr path ns-binding add-on)))
                    (let ((expr-res (car lst2))
                          (path (cadr lst2)))
                      (and-let*
                       ((path (sxml:parse-assert ")" path)))
                       (loop (cons expr-res expr-res-lst) ns-binding path)))))
                  ((string=? name "xmlns")  ; xmlns part
                   (and-let*
                    ((path0 (sxml:parse-assert "(" path))
                     (lst2 (sxml:parse-ncname path0))
                     (prefix (string->symbol (car lst2)))
                     (path (sxml:parse-assert "=" (cadr lst2))))
                    (let rpt2 ((path (sxml:skip-ws path)) (uri '()))
                      (cond
                        ((null? path)
                         (sxml:parse-assert ")" path)
                         #f)
                        ((and (char=? (car path) #\)) (null? uri))
                         (sxml:xpointer-parse-error
                          "namespace URI cannot be empty"))
                        ((char=? (car path) #\))
                         (loop expr-res-lst
                               (cons
                                (cons prefix (list->string (reverse uri)))
                                ns-binding)
                               (cdr path)))
                        (else
                         (rpt2 (cdr path) (cons (car path) uri)))))))
                  (else  ; any other XPointer scheme
                   (and-let*
                    ((path (sxml:parse-assert "(" path)))
                    (let rpt3 ((n 1) (path path))
                      (cond
                        ((= n 0)
                         (sxml:xpointer-parse-warning
                          "unknown xpointer schema - " name ". Ignoring")
                         (loop expr-res-lst ns-binding path))
                        ((null? path)
                         (sxml:parse-assert ")" path)
                         #f)
                        ((char=? (car path) #\() (rpt3 (+ n 1) (cdr path)))
                        ((char=? (car path) #\)) (rpt3 (- n 1) (cdr path)))
                        (else (rpt3 n (cdr path))))))))))))))
       
       ; Parses an ChildSeq production ([2] in XPointer specification)
       ; [2]    ChildSeq    ::=    Name? ('/' [1-9] [0-9]* )+
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'child-seq
       ;  param-value ::=
       ;   (list
       ;    (list  'with-name
       ;           (lambda (name-string number-lst add-on) ...) )
       ;    (list  'without-name
       ;           (lambda (number-lst add-on) ...) ))
       (txp:parse-child-seq
        (let ((helper
               (lambda (path)
                 (let loop ((num-lst '())
                            (path path))
                   (let ((path2 (sxml:parse-check "/" path)))
                     (cond
                       (path2  ; #\/ found
                        (and-let* ((lst (sxml:parse-natural path2)))
                                  (loop (cons (car lst) num-lst)
                                        (cadr lst))))
                       ((null? (sxml:skip-ws path))  ; end of path
                        (reverse num-lst))
                       (else    ; this will cause an error message
                        (sxml:parse-assert "/" path))))))))                         
          (let* ((child-seq-value (txp:param-value 'child-seq txp-params))
                 (with-name-value (txp:param-value 'with-name child-seq-value))
                  (without-name-value
                   (txp:param-value 'without-name child-seq-value)))
            (lambda (path ns-binding add-on)
              (let ((path2 (sxml:parse-check "/" path)))
                (if
                 path2  ; "/" found => no Name supported
                 (and-let*
                  ((number-lst (helper path)))                  
                  (without-name-value number-lst add-on))
                 (and-let*
                  ((lst (sxml:parse-name path))
                   (name (car lst))
                   (number-lst (helper (cadr lst))))
                  (with-name-value name number-lst add-on))))))))
                   
       ;-------------------------------------------------
       ; Higher level functions
       ;  ns-binding - declared namespace prefixes (an optional argument)
       ;  add-on - whatever; may be useful for specific parser
       ; implementations, since this parameter is passed throughout all
       ; grammar rules
       ;
       ;  ns-binding = (listof  (prefix . uri))
       ;  prefix - a symbol
       ;  uri - a string
       
       ; Parses XPath grammar
       ;  path is a string here
       (txp:parse-xpath
        (lambda (path-string ns-binding add-on)
          (let ((res (txp:parse-location-path
                      (string->list path-string) ns-binding add-on)))
            (if (and res  ; no parser errors
                     (sxml:assert-end-of-path (cadr res)))
                (car res)
                'txp:parser-error))))
       
       ; Parses an XPointer production ([1] in XPointer specification)
       ; [1]    XPointer    ::=    Name | ChildSeq | FullXPtr 
       (txp:parse-xpointer
        (lambda (path-string ns-binding add-on)
          (let ((path (string->list path-string)))
            (if (sxml:parse-check "/" path)   ; => ChildSeq
                (txp:parse-child-seq path ns-binding add-on)
                (and-let*
                 ((lst (sxml:parse-name path))
                  (new-path (cadr lst)))
                 (if (sxml:parse-check "(" new-path)  ; FullXPtr production
                     (txp:parse-full-xptr path ns-binding add-on)
                     (txp:parse-child-seq path ns-binding add-on)))))))
       
       ; Parses XPath Expression
       ; [14]    Expr    ::=    OrExpr
       (txp:parse-xpath-expression
        (lambda (path-string ns-binding add-on)
          (let ((res (txp:parse-expr
                      (string->list path-string) ns-binding add-on)))
            (if (and res  ; no parser errors
                     (sxml:assert-end-of-path (cadr res)))
                (car res)
                'txp:parser-error))))
       
       )
        
    `((xpath ,txp:parse-xpath)
      (xpointer ,txp:parse-xpointer)
      (expr ,txp:parse-xpath-expression))
    ))




;;			XML processing in Scheme
;		     SXPath -- SXML Query Language
;
; $Id: sxpathlib.scm,v 3.918 2004/02/05 22:52:33 kl Exp kl $
;
; This code is in Public Domain
; It's based on SXPath by Oleg Kiselyov, and multiple improvements 
; implemented by Dmitry Lizorkin.
;
; The list of differences from original SXPath.scm my be found in changelog.txt
; 
;  Kirill Lisovsky    lisovsky@acm.org
;
;                                 *  *  *
;
; SXPath is a query language for SXML, an instance of XML Information
; set (Infoset) in the form of s-expressions. See SSAX.scm for the
; definition of SXML and more details. SXPath is also a translation into
; Scheme of an XML Path Language, XPath:
;	http://www.w3.org/TR/xpath
; XPath and SXPath describe means of selecting a set of Infoset's items
; or their properties.
;
; To facilitate queries, XPath maps the XML Infoset into an explicit
; tree, and introduces important notions of a location path and a
; current, context node. A location path denotes a selection of a set of
; nodes relative to a context node. Any XPath tree has a distinguished,
; root node -- which serves as the context node for absolute location
; paths. Location path is recursively defined as a location step joined
; with a location path. A location step is a simple query of the
; database relative to a context node. A step may include expressions
; that further filter the selected set. Each node in the resulting set
; is used as a context node for the adjoining location path. The result
; of the step is a union of the sets returned by the latter location
; paths.
;
; The SXML representation of the XML Infoset (see SSAX.scm) is rather
; suitable for querying as it is. Bowing to the XPath specification,
; we will refer to SXML information items as 'Nodes':
; 	<Node> ::= <Element> | <attributes-coll> | <attrib>
; 		   | "text string" | <PI>
; This production can also be described as
;	<Node> ::= (name . <Nodelist>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodelist> ::= (<Node> ...)
; Nodelists, and Nodes other than text strings are both lists. A
; <Nodelist> however is either an empty list, or a list whose head is not
; a symbol.  A symbol at the head of a node is either an XML name (in
; which case it's a tag of an XML element), or an administrative name
; such as '@'.  This uniform list representation makes processing rather
; simple and elegant, while avoiding confusion. The multi-branch tree
; structure formed by the mutually-recursive datatypes <Node> and
; <Nodelist> lends itself well to processing by functional languages.
;
; A location path is in fact a composite query over an XPath tree or
; its branch. A singe step is a combination of a projection, selection
; or a transitive closure. Multiple steps are combined via join and
; union operations. This insight allows us to _elegantly_ implement
; XPath as a sequence of projection and filtering primitives --
; converters -- joined by _combinators_. Each converter takes a node
; and returns a nodelist which is the result of the corresponding query
; relative to that node. A converter can also be called on a set of
; nodes. In that case it returns a union of the corresponding queries over
; each node in the set. The union is easily implemented as a list
; append operation as all nodes in a SXML tree are considered
; distinct, by XPath conventions. We also preserve the order of the
; members in the union. Query combinators are high-order functions:
; they take converter(s) (which is a Node|Nodelist -> Nodelist function)
; and compose or otherwise combine them. We will be concerned with
; only relative location paths [XPath]: an absolute location path is a
; relative path applied to the root node.
;
; Similarly to XPath, SXPath defines full and abbreviated notations
; for location paths. In both cases, the abbreviated notation can be
; mechanically expanded into the full form by simple rewriting
; rules. In case of SXPath the corresponding rules are given as
; comments to a sxpath function, below. The regression test suite at
; the end of this file shows a representative sample of SXPaths in
; both notations, juxtaposed with the corresponding XPath
; expressions. Most of the samples are borrowed literally from the
; XPath specification, while the others are adjusted for our running
; example, tree1.
;


;=============================================================================
; Basic converters and applicators
; A converter is a function
;	type Converter = Node|Nodelist -> Nodelist
; A converter can also play a role of a predicate: in that case, if a
; converter, applied to a node or a nodelist, yields a non-empty
; nodelist, the converter-predicate is deemed satisfied. Throughout
; this file a nil nodelist is equivalent to #f in denoting a failure.

; Returns #t if given object is a nodelist
(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

; If x is a nodelist - returns it as is, otherwise wrap it in a list.
(define (as-nodeset x)
  (if (nodeset? x) x (list x)))

;-----------------------------------------------------------------------------
; Node test
; The following functions implement 'Node test's as defined in
; Sec. 2.3 of XPath document. A node test is one of the components of a
; location step. It is also a converter-predicate in SXPath.

; Predicate which returns #t if <obj> is SXML element, otherwise returns #f. 
(define (sxml:element? obj)	
   (and (pair? obj)
	(symbol? (car obj))
	(not (memq (car obj) 
			   ; '(@ @@ *PI* *COMMENT* *ENTITY* *NAMESPACES*)
			   ; the line above is a workaround for old SXML
	'(@ @@ *PI* *COMMENT* *ENTITY*)))))

; The function ntype-names?? takes a list of acceptable node names as a
; criterion and returns a function, which, when applied to a node, 
; will return #t if the node name is present in criterion list and #f
; othervise.
;	ntype-names?? :: ListOfNames -> Node -> Boolean
(define (ntype-names?? crit)
  (lambda(node)
    (and (pair? node)
	 (memq (car node) crit))))

; The function ntype?? takes a type criterion and returns
; a function, which, when applied to a node, will tell if the node satisfies
; the test.
;	ntype?? :: Crit -> Node -> Boolean
;
; The criterion 'crit' is 
;  one of the following symbols:
;	id		- tests if the Node has the right name (id)
;	@		- tests if the Node is an <attributes-list>
;	*		- tests if the Node is an <Element>
;	*text*		- tests if the Node is a text node
;	*data*		- tests if the Node is a data node 
;                         (text, number, boolean, etc., but not pair)
;	*PI*		- tests if the Node is a PI node
;	*COMMENT*	- tests if the Node is a COMMENT node
;	*ENTITY*        - tests if the Node is a ENTITY node
;	*any*		- #t for any type of Node
(define (ntype?? crit)
  (case crit
    ((*) sxml:element?)
    ((*any*) (lambda (node) #t))
    ((*text*) (lambda (node) (string? node)))
    ((*data*) (lambda (node) (not (pair? node))))
    (else (lambda (node) (and (pair? node) (eq? crit (car node)))))
    ))

; This function takes a namespace-id, and returns a predicate
; Node -> Boolean, which is #t for nodes with this very namespace-id.
; ns-id is a string
; (ntype-namespace-id?? #f) will be #t for nodes with non-qualified names.
(define (ntype-namespace-id?? ns-id)
  (lambda (node)
    (and (pair? node)
	 (not (memq (car node) 
			 '(@ @@ *PI* *COMMENT* *ENTITY*)))
	 (let ((nm (symbol->string (car node))))
	   (cond 
	     ((string-rindex nm #\:)	   
	      => (lambda (pos) 
	      (and 
		(= pos (string-length ns-id))
		(string-prefix? ns-id nm))))
	     (else (not ns-id)))))))
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

; This function takes a predicate and returns it complemented 
; That is if the given predicate yelds #f or '() the complemented one  
; yields the given node (#t) and vice versa.
(define (sxml:complement pred)
  (lambda(node)
    (case (pred node)
      ((#f '()) node)
      (else #f))))

; Curried equivalence converter-predicates
(define (node-eq? other)
  (lambda (node)
    (eq? other node)))

(define (node-equal? other)
  (lambda (node)
    (equal? other node)))

; node-pos:: N -> Nodelist -> Nodelist, or
; node-pos:: N -> Converter
; Select the N'th element of a Nodelist and return as a singular Nodelist;
; Return an empty nodelist if the Nth element does not exist.
; ((node-pos 1) Nodelist) selects the node at the head of the Nodelist,
; if exists; ((node-pos 2) Nodelist) selects the Node after that, if
; exists.
; N can also be a negative number: in that case the node is picked from
; the tail of the list.
; ((node-pos -1) Nodelist) selects the last node of a non-empty nodelist;
; ((node-pos -2) Nodelist) selects the last but one node, if exists.
(define (node-pos n)
  (lambda (nodelist)
    (cond
     ((not (nodeset? nodelist)) '())
     ((null? nodelist) nodelist)
     ((eqv? n 1) (list (car nodelist)))
     ((negative? n) ((node-pos (- n)) (reverse nodelist)))
     (else
      (assert (positive? n))
      ((node-pos (-- n)) (cdr nodelist))))))

; filter:: Converter -> Converter
; A filter applicator, which introduces a filtering context. The argument
; converter is considered a predicate, with either #f or nil result meaning
; failure.
(define (sxml:filter pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)) 
	       (res '()))
      (if (null? lst)
	  (reverse res)
	  (let ((pred-result (pred? (car lst))))
	    (loop (cdr lst)
		  (if (and pred-result (not (null? pred-result)))
		      (cons (car lst) res)
		      res)))))))

; take-until:: Converter -> Converter, or
; take-until:: Pred -> Node|Nodelist -> Nodelist
; Given a converter-predicate and a nodelist, apply the predicate to
; each element of the nodelist, until the predicate yields anything but #f or
; nil. Return the elements of the input nodelist that have been processed
; till that moment (that is, which fail the predicate).
; take-until is a variation of the filter above: take-until passes
; elements of an ordered input set till (but not including) the first
; element that satisfies the predicate.
; The nodelist returned by ((take-until (not pred)) nset) is a subset -- 
; to be more precise, a prefix -- of the nodelist returned by
; ((filter pred) nset)
(define (take-until pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		'()
		(cons (car lst) (loop (cdr lst)))))
	  ))))

; take-after:: Converter -> Converter, or
; take-after:: Pred -> Node|Nodelist -> Nodelist
; Given a converter-predicate and a nodelist, apply the predicate to
; each element of the nodelist, until the predicate yields anything but #f or
; nil. Return the elements of the input nodelist that have not been processed:
; that is, return the elements of the input nodelist that follow the first
; element that satisfied the predicate.
; take-after along with take-until partition an input nodelist into three
; parts: the first element that satisfies a predicate, all preceding
; elements and all following elements.
(define (take-after pred?)
  (lambda (lst)	; a nodelist or a node (will be converted to a singleton nset)
    (let loop ((lst (as-nodeset lst)))
      (if (null? lst) lst
	  (let ((pred-result (pred? (car lst))))
	    (if (and pred-result (not (null? pred-result)))
		(cdr lst)
		(loop (cdr lst))))
	  ))))

; Apply proc to each element of lst and return the list of results.
; if proc returns a nodelist, splice it into the result
;
; From another point of view, map-union is a function Converter->Converter,
; which places an argument-converter in a joining context.
(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; node-reverse :: Converter, or
; node-reverse:: Node|Nodelist -> Nodelist
; Reverses the order of nodes in the nodelist
; This basic converter is needed to implement a reverse document order
; (see the XPath Recommendation).
(define node-reverse 
  (lambda (node-or-nodelist)
    (if (not (nodeset? node-or-nodelist)) (list node-or-nodelist)
	(reverse node-or-nodelist))))

; node-trace:: String -> Converter
; (node-trace title) is an identity converter. In addition it prints out
; a node or nodelist it is applied to, prefixed with the 'title'.
; This converter is very useful for debugging.
(define (node-trace title)
  (lambda (node-or-nodelist)
    (cout nl "-->" title " :")
    (pp node-or-nodelist)
    node-or-nodelist))


;------------------------------------------------------------------------------
; Converter combinators
;
; Combinators are higher-order functions that transmogrify a converter
; or glue a sequence of converters into a single, non-trivial
; converter. The goal is to arrive at converters that correspond to
; XPath location paths.
;
; From a different point of view, a combinator is a fixed, named
; _pattern_ of applying converters. Given below is a complete set of
; such patterns that together implement XPath location path
; specification. As it turns out, all these combinators can be built
; from a small number of basic blocks: regular functional composition,
; map-union and filter applicators, and the nodelist union.



; select-kids:: Pred -> Node -> Nodelist
; Given a Node, return an (ordered) subset its children that satisfy
; the Pred (a converter, actually)
; select-kids:: Pred -> Nodelist -> Nodelist
; The same as above, but select among children of all the nodes in
; the Nodelist
;
; More succinctly, the signature of this function is
; select-kids:: Converter -> Converter
(define (select-kids test-pred?)
  (lambda (node)		; node or node-set
    (cond 
     ((null? node) node)
     ((not (pair? node)) '())   ; No children
     ((symbol? (car node))
      ((sxml:filter test-pred?) (cdr node)))	; it's a single node
     (else (map-union (select-kids test-pred?) node)))))


; node-self:: Pred -> Node -> Nodelist, or
; node-self:: Converter -> Converter
; Similar to select-kids but apply to the Node itself rather
; than to its children. The resulting Nodelist will contain either one
; component, or will be empty (if the Node failed the Pred).
(define node-self sxml:filter)


; node-join:: [LocPath] -> Node|Nodelist -> Nodelist, or
; node-join:: [Converter] -> Converter
; join the sequence of location steps or paths as described
; in the title comments above.
(define (node-join . selectors)
  (lambda (nodelist)		; Nodelist or node
    (let loop ((nodelist nodelist) (selectors selectors))
      (if (null? selectors) nodelist
	  (loop 
	   (if (nodeset? nodelist)
	       (map-union (car selectors) nodelist)
	       ((car selectors) nodelist))
	   (cdr selectors))))))


; node-reduce:: [LocPath] -> Node|Nodelist -> Nodelist, or
; node-reduce:: [Converter] -> Converter
; A regular functional composition of converters.
; From a different point of view,
;    ((apply node-reduce converters) nodelist)
; is equivalent to
;    (foldl apply nodelist converters)
; i.e., folding, or reducing, a list of converters with the nodelist
; as a seed.
(define (node-reduce . converters)
  (lambda (nodelist)		; Nodelist or node
    (let loop ((nodelist nodelist) (converters converters))
      (if (null? converters) nodelist
	  (loop ((car converters) nodelist) (cdr converters))))))


; node-or:: [Converter] -> Converter
; This combinator applies all converters to a given node and
; produces the union of their results.
; This combinator corresponds to a union, '|' operation for XPath
; location paths.
(define (node-or . converters)
  (lambda (node-or-nodelist)
    (let loop ((result '()) (converters converters))
      (if (null? converters) result
	  (loop (append result (or ((car converters) node-or-nodelist) '()))
		(cdr converters))))))


; node-closure:: Converter -> Converter
; Select all _descendants_ of a node that satisfy a converter-predicate.
; This combinator is similar to select-kids but applies to
; grand... children as well.
; This combinator implements the "descendant::" XPath axis
; Conceptually, this combinator can be expressed as
; (define (node-closure f)
;      (node-or
;        (select-kids f)
;	 (node-reduce (select-kids (ntype?? '*)) (node-closure f))))
; This definition, as written, looks somewhat like a fixpoint, and it
; will run forever. It is obvious however that sooner or later
; (select-kids (ntype?? '*)) will return an empty nodelist. At
; this point further iterations will no longer affect the result and
; can be stopped.
(define (node-closure test-pred?)	    
(let ((kid-selector (select-kids test-pred?)))
  (lambda (node) ; Nodelist or node
    (let loop ((parent node) (result '()))
      (if (null? parent) result
	(loop (sxml:child-elements parent)
	  (append result
	    (kid-selector parent)))
	)))))

;=============================================================================
; Unified with sxpath-ext and sxml-tools

; According to XPath specification 2.3, this test is true for any
; XPath node.
; For SXML auxiliary lists and lists of attributes has to be excluded.
(define (sxml:node? node)
  (not (and 
	 (pair? node)
	 (memq (car node) '(@ @@)))))

; Returns the list of attributes for a given SXML node
; Empty list is returned if the given node os not an element,
; or if it has no list of attributes
(define (sxml:attr-list obj)
  (if (and  (sxml:element? obj) 
	    (not (null? (cdr obj)))
	    (pair? (cadr obj)) 
	    (eq? '@ (caadr obj)))
	 (cdadr obj)
	 '()))

; Attribute axis
(define (sxml:attribute test-pred?)
  (let ((fltr (sxml:filter test-pred?)))
    (lambda (node)
      (map-union
       (lambda (node) (fltr (sxml:attr-list node)))
       (as-nodeset node)))))

; Child axis
;  This function is similar to 'select-kids', but it returns an empty
;  child-list for PI, Comment and Entity nodes
(define (sxml:child test-pred?)
  (lambda (node)		; node or node-set
    (cond 
      ((null? node) node)
      ((not (pair? node)) '())   ; No children
      ((memq (car node) '(*PI* *COMMENT* *ENTITY*))   ; PI, Comment or Entity
       '())   ; No children
      ((symbol? (car node))    ; it's a single node       
       ((sxml:filter test-pred?) (cdr node)))
      (else (map-union (sxml:child test-pred?) node)))))

; Parent axis
; Given a predicate, it returns a function 
;  RootNode -> Converter
; which which yields a 
;  node -> parent 
; converter then applied to a rootnode.
; Thus, such a converter may be constructed using
;  ((sxml:parent test-pred) rootnode)
; and returns a parent of a node it is applied to.
; If applied to a nodelist, it returns the 
; list of parents of nodes in the nodelist. The rootnode does not have
; to be the root node of the whole SXML tree -- it may be a root node
; of a branch of interest.
; The parent:: axis can be used with any SXML node.
(define (sxml:parent test-pred?)
  (lambda (root-node)   ; node or nodelist
    (lambda (node)   ; node or nodelist
      (if (nodeset? node)
	(map-union ((sxml:parent test-pred?) root-node) node)
	(let rpt ((pairs
		    (apply append
		     (map 
			      (lambda (root-n)
				(map
				  (lambda (arg) (cons arg root-n))
				  (append 
				    (sxml:attr-list root-n)
				    (sxml:child-nodes root-n))))
                              (as-nodeset root-node)))
		     ))
	  (if (null? pairs)
	    '()
	    (let ((pair (car pairs)))
	      (if (eq? (car pair) node)
		((sxml:filter test-pred?) (list (cdr pair)))
		(rpt (append
			(map
			  (lambda (arg) (cons arg (car pair)))
			  (append 
			    (sxml:attr-list (car pair))
			    (sxml:child-nodes (car pair))))
			(cdr pairs)
			))))))))))


;=============================================================================
; Popular short cuts 

; node-parent:: RootNode -> Converter
; (node-parent rootnode) yields a converter that returns a parent of a
; node it is applied to. If applied to a nodelist, it returns the list
; of parents of nodes in the nodelist.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;  parent(x) = { y | y=subnode*(root), x=subnode(y) }
; Therefore, node-parent is not the fundamental converter: it can be
; expressed through the existing ones.  Yet node-parent is a rather
; convenient converter. It corresponds to a parent:: axis of SXPath.
;
; Please note: this function is provided for backward compatibility 
; with SXPath/SXPathlib ver. 3.5.x.x and earlier.
; Now it's a particular case of 'sxml:parent' application: 
(define node-parent (sxml:parent (ntype?? '*any*)))

(define sxml:child-nodes (sxml:child sxml:node?))

(define sxml:child-elements (select-kids sxml:element?))








;; Classic TXPath implementation based on sxpathlib, sxpath-ext and txp-parser
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; XPointer's points and ranges are NOT implemented
;
; Full XPath Core Function Library is supported. That is:
; 4.1 Node Set Functions
;    number last()
;    number position()
;    number count(node-set)
;    node-set id(object)
;    string local-name(node-set?)
;    string namespace-uri(node-set?)
;    string name(node-set?)
; 4.2 String Functions
;    string string(object?)
;    string concat(string, string, string*)
;    boolean starts-with(string, string)
;    boolean contains(string, string)
;    string substring-before(string, string)
;    string substring-after(string, string)
;    string substring(string, number, number?)
;    number string-length(string?)
;    string normalize-space(string?)
;    string translate(string, string, string)
; 4.3 Boolean Functions
;    boolean boolean(object)
;    boolean not(boolean)
;    boolean true()
;    boolean false()
;    boolean lang(string)
; 4.4 Number Functions
;    number number(object?)
;    number sum(node-set)
;    number floor(number)
;    number ceiling(number)
;    number round(number)


;==========================================================================
; Auxilliary

; Runtime errors handler (unbound variable, bad argument, etc).
; It may be re-defined (say, like a warning) without 'exit',  and evaluation will 
; be continued.
; In this case, a default value (usually empty nodeset or 0) is returned by 
; a sub-expression which caused an XPath/XPointer runtime error.
(define (sxml:xpointer-runtime-error . text)
  (snow-error
   "XPath/XPointer runtime error"
   (apply string-append (map display-to-string text)))
  ;; (apply cerr (append
  ;;              (list "XPath/XPointer runtime error: ")
  ;;              text (list nl)))
  ;; (exit -1)
  )


;--------------------------------------------------------------------------
; Helper functions

; Filter nodeset using preds-list as described in XPath rec. 2.4
; A helper for sxml:parse-step and sxml:parse-filter-expr
(define (sxml:xpath-nodeset-filter preds-list nodeset root-node var-binding)
  (let rpt ((nodeset nodeset)
	    (ps preds-list))
    (if (null? ps) 
      nodeset
      (let lab ((nset nodeset)
		(res '())
		(pos 1)) 
	(if (null? nset)
	  (rpt (reverse res) (cdr ps))
	  (let* ((size (length nodeset))
		 (val ((car ps) 
		       (list (car nset)) 
		       root-node 
		       (cons pos size) 
		       var-binding)))
	    (lab (cdr nset)
		 (if (if (number? val)
		       (= val pos)
		       (sxml:boolean val))
		   (cons (car nset) res)
		   res)
		 (+ pos 1))))))))


; A helper for arithmetic expressions
;   sxml:parse-additive-expr and sxml:parse-multiplicative-expr 
(define (sxml:arithmetic-eval unary-expr-res-lst op-lst add-on)
  (lambda (nodeset root-node context var-binding)
    (let rpt
      ((res (sxml:number
             ((car unary-expr-res-lst) nodeset root-node context var-binding)))
       (fs (cdr unary-expr-res-lst))
       (ops op-lst))
    (if (null? fs)
        res
        (rpt ((car ops)
              res
              (sxml:number ((car fs) nodeset root-node context var-binding)))
             (cdr fs)
             (cdr ops))))))


;==========================================================================
; XPath Core Function Library

;-------------------------------------------------
; 4.1 Node Set Functions

; last()
(define (sxml:core-last)
  (lambda (nodeset root-node context var-binding)
    (cdr context)))

; position()
(define (sxml:core-position)
  (lambda (nodeset root-node context var-binding)
    (car context)))

; count(node-set)
(define (sxml:core-count arg-func)
  (lambda (nodeset root-node context var-binding)
    (let ((res (arg-func nodeset root-node context var-binding)))
      (cond
        ((nodeset? res) (length res))
        (else
         (sxml:xpointer-runtime-error
          "count() function - an argument is not a nodeset")
         0)))))

; id(object)
(define (sxml:core-id arg-func)
  (lambda (nodeset root-node context var-binding)
    (let* ((id-nset ((sxml:child (ntype?? 'id-index))
                     ((sxml:child (ntype?? '@@)) root-node))))
      (if
       (null? id-nset)  ; no id-index
       '()  ; ID function returns an empty nodeset
       ((sxml:id (cdar id-nset))  ; implemented in "sxpath-ext.scm"
        (arg-func nodeset root-node context var-binding))))))

; local-name(node-set?)
(define (sxml:core-local-name . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (let ((name (symbol->string (caar nodeset))))
             (cond
               ((string-rindex name #\:)
                => (lambda (pos)
                     (substring name (+ pos 1) (string-length name))))
               (else  ; a NCName
                name))))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")              
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring
                          name (+ pos 1) (string-length name))))
                   (else  ; a NCName
                    name))))))))))

; namespace-uri(node-set?)
(define (sxml:core-namespace-uri . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (let ((name (symbol->string (caar nodeset))))
             (cond
               ((string-rindex name #\:)
                => (lambda (pos)
                     (substring name 0 pos)))
               (else ""))))))  ; a NCName
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring name 0 pos)))
                   (else ""))))))))))

; name(node-set?)
(define (sxml:core-name . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (symbol->string (caar nodeset)))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (symbol->string (caar obj)))))))))


;-------------------------------------------------
; 4.2 String Functions

; string(object?)
(define (sxml:core-string . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (sxml:string nodeset))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (sxml:string 
           (func nodeset root-node context var-binding))))))

; concat(string, string, string*)
(define (sxml:core-concat . arg-func-lst)
  (lambda (nodeset root-node context var-binding)
    (apply
     string-append
     (map
      (lambda (f)
        (sxml:string (f nodeset root-node context var-binding)))
      arg-func-lst))))

; starts-with(string, string)
(define (sxml:core-starts-with arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding))))
      (string-prefix? str2 str1))))

; contains(string, string)
(define (sxml:core-contains arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding))))
      (if (substring? str2 str1) #t #f)  ; must return a boolean
      )))
  
; substring-before(string, string)
(define (sxml:core-substring-before arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let* ((str1 (sxml:string
                  (arg-func1 nodeset root-node context var-binding)))
           (str2 (sxml:string
                  (arg-func2 nodeset root-node context var-binding)))
           (pos (substring? str2 str1)))
      (if (not pos)  ; STR1 doesn't contain STR2
          ""
          (substring str1 0 pos)))))

; substring-after(string, string)
(define (sxml:core-substring-after arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let* ((str1 (sxml:string
                  (arg-func1 nodeset root-node context var-binding)))
           (str2 (sxml:string
                  (arg-func2 nodeset root-node context var-binding)))
           (pos (substring? str2 str1)))
      (if
       (not pos)  ; STR1 doesn't contain STR2
       ""
       (substring
        str1 (+ pos (string-length str2)) (string-length str1))))))

; substring(string, number, number?)
(define (sxml:core-substring arg-func1 arg-func2 . arg-func3)
  (if (null? arg-func3)  ; no third argument supplied
      (lambda (nodeset root-node context var-binding)
        (let ((str (sxml:string
                    (arg-func1 nodeset root-node context var-binding)))
              (num1 (sxml:number
                     (arg-func2 nodeset root-node context var-binding))))
          (let ((len (string-length str))
                (start (- (inexact->exact (round num1)) 1)))
            (if (> start len)
                ""
                (substring str (if (< start 0) 0 start) len)))))
      (let ((arg-func3 (car arg-func3)))
        (lambda (nodeset root-node context var-binding)
          (let ((str (sxml:string
                      (arg-func1 nodeset root-node context var-binding)))
                (num1 (sxml:number
                       (arg-func2 nodeset root-node context var-binding)))
                (num2 (sxml:number
                       (arg-func3 nodeset root-node context var-binding))))
            (let* ((len (string-length str))
                   (start (- (inexact->exact (round num1)) 1))
                   (fin (+ start (inexact->exact (round num2)))))
              (if (or (> start len) (< fin 0) (< fin start))
                  ""
                  (substring str
                             (if (< start 0) 0 start)
                             (if (> fin len) len fin)))))))))

; string-length(string?)
(define (sxml:core-string-length . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (string-length (sxml:string nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (string-length
           (sxml:string
            (func nodeset root-node context var-binding)))))))

; normalize-space(string?)
(define (sxml:core-normalize-space . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (let rpt ((src (string-split (sxml:string nodeset) sxml:whitespace))
                  (res '()))
          (cond
            ((null? src)
             (apply string-append (reverse res)))
            ((= (string-length (car src)) 0)  ; empty string
             (rpt (cdr src) res))
            ((null? res)
             (rpt (cdr src) (cons (car src) res)))
            (else
             (rpt (cdr src) (cons (car src) (cons " " res)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let rpt ((src (string-split
                          (sxml:string
                           (func nodeset root-node context var-binding))
                          sxml:whitespace))
                    (res '()))
            (cond
              ((null? src)
               (apply string-append (reverse res)))
              ((= (string-length (car src)) 0)  ; empty string
               (rpt (cdr src) res))
              ((null? res)
               (rpt (cdr src) (cons (car src) res)))
              (else
               (rpt (cdr src) (cons (car src) (cons " " res))))))))))

; translate(string, string, string)
(define (sxml:core-translate arg-func1 arg-func2 arg-func3)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding)))
          (str3 (sxml:string
                 (arg-func3 nodeset root-node context var-binding))))
      (let ((alist
             (let while ((lst2 (string->list str2))
                         (lst3 (string->list str3))
                         (alist '()))
               (cond
                 ((null? lst2) (reverse alist))
                 ((null? lst3)
                  (append
                   (reverse alist)
                   (map
                    (lambda (ch) (cons ch #f))
                    lst2)))
                 (else
                  (while
                   (cdr lst2)
                   (cdr lst3)
                   (cons (cons (car lst2) (car lst3)) alist)))))))
        (let rpt ((lst1 (string->list str1))
                  (res '()))
          (cond
            ((null? lst1) (list->string (reverse res)))
            ((assoc (car lst1) alist)
             => (lambda (pair)
                  (if (cdr pair)
                      (rpt (cdr lst1) (cons (cdr pair) res))
                      (rpt (cdr lst1) res))))
            (else
             (rpt (cdr lst1) (cons (car lst1) res)))))))))
  

;-------------------------------------------------
; 4.3 Boolean Functions

; boolean(object)
(define (sxml:core-boolean arg-func)
  (lambda (nodeset root-node context var-binding)
    (sxml:boolean 
     (arg-func nodeset root-node context var-binding))))

; not(boolean)
(define (sxml:core-not arg-func)
  (lambda (nodeset root-node context var-binding)
    (not (sxml:boolean 
          (arg-func nodeset root-node context var-binding)))))

; true()
(define (sxml:core-true)
  (lambda (nodeset root-node context var-binding) #t))

; false()
(define (sxml:core-false)
  (lambda (nodeset root-node context var-binding) #f))

; lang(string)
(define (sxml:core-lang arg-func)
  (lambda (nodeset root-node context var-binding)
    (if
     (null? nodeset)
     #f
     (let ((arg (sxml:string
                 (arg-func nodeset root-node context var-binding)))
           (context-node (car nodeset)))
       (let rpt ((pairs
                  (map
                   (lambda (node) (cons node #f))
                   root-node)))
         (if
          (null? pairs)  ; context node not found
          #f
          (let* ((lng
                  ((sxml:child (ntype?? '*text*))
                   ((sxml:attribute (ntype?? 'xml:lang))
                    (caar pairs))))
                 (lng (if (null? lng) (cdar pairs) (car lng))))
            (if
             (eq? context-node (caar pairs)) ; context node found
             (and
              lng
              (or (string-ci=? arg lng)
                  (string-prefix-ci? (string-append arg "-") lng)))
             (rpt
              (append
               (map
                (lambda (node) (cons node lng))
                ((sxml:attribute (ntype?? '*)) (caar pairs)))
               (map
                (lambda (node) (cons node lng))
                ((sxml:child sxml:node?) (caar pairs)))
               (cdr pairs)))))))))))
  

;-------------------------------------------------
; 4.4 Number Functions

; number(object?)
(define (sxml:core-number . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (sxml:number nodeset))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (sxml:number 
           (func nodeset root-node context var-binding))))))

; sum(node-set)
(define (sxml:core-sum arg-func)
  (lambda (nodeset root-node context var-binding)
    (let ((res (arg-func nodeset root-node context var-binding)))
      (cond
        ((nodeset? res)
         (apply +
                (map
                 (lambda (node)
                   (sxml:number (sxml:string-value node)))
                 res)))
        (else
         (sxml:xpointer-runtime-error
          "SUM function - an argument is not a nodeset")
         0)))))

; floor(number)
(define (sxml:core-floor arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (floor (sxml:number 
             (arg-func nodeset root-node context var-binding))))))

; ceiling(number)
(define (sxml:core-ceiling arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (ceiling (sxml:number
               (arg-func nodeset root-node context var-binding))))))

; round(number)
(define (sxml:core-round arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (round (sxml:number
             (arg-func nodeset root-node context var-binding))))))



;==========================================================================
; Parameters for classic TXPath implementation

(define sxml:classic-params
  `(
    ; For XPath axes, the result is returned in the form of the pair
    ; (cons  (lambda ...)  root-node-required)  
    ;  (lambda ...) - one of the axis functions
    ;  root-node-required - a boolean value
    ; If root-node-required = #t, lambda's signature is
    ;  (lambda (test-pred?)
    ;   (lambda (root-node)
    ;    (lambda (nodeset) ... )))
    ; otherwise
    ;  (lambda (test-pred?)
    ;   (lambda (nodeset) ... ))
    (axis
     ((ancestor
       ,(lambda (add-on) (cons sxml:ancestor #t)))
      (ancestor-or-self
       ,(lambda (add-on) (cons sxml:ancestor-or-self #t)))
      (attribute
       ,(lambda (add-on) (cons sxml:attribute #f)))
      (child
       ,(lambda (add-on) (cons sxml:child #f)))
      (descendant
       ,(lambda (add-on) (cons sxml:descendant #f)))
      (descendant-or-self
       ,(lambda (add-on) (cons sxml:descendant-or-self #f)))
      (following
       ,(lambda (add-on) (cons sxml:following #t)))
      (following-sibling
       ,(lambda (add-on) (cons sxml:following-sibling #t)))
      (namespace
       ,(lambda (add-on) (cons sxml:namespace #f)))
      (parent
       ,(lambda (add-on) (cons sxml:parent #t)))
      (preceding
       ,(lambda (add-on) (cons sxml:preceding #t)))
      (preceding-sibling
       ,(lambda (add-on) (cons sxml:preceding-sibling #t)))
      (self
       ,(lambda (add-on) (cons sxml:filter #f)))))
    
    ; For NodeTests, the result is
    ;  (lambda (node) ...)  - a node test function
    ; or 'txp:semantic-error (namely, for point and range)
    (node-test
     ((star
       ,(lambda (add-on) (ntype?? '*)))
      (uri+star
       ,(lambda (uri add-on) (ntype-namespace-id?? uri)))
      (qname
       ,(lambda (uri local-name add-on)
          (if (not uri)
              (ntype?? (string->symbol local-name))
              (ntype?? (string->symbol (string-append uri ":" local-name))))))
      (comment
       ,(lambda (add-on) (ntype?? '*COMMENT*)))
      (text
       ,(lambda (add-on) (ntype?? '*text*)))
      (processing-instruction
       ,(lambda (literal-string add-on)
          (if (not literal-string)  ; no literal provided
              (lambda (node)
                (and (pair? node) (eq? (car node) '*PI*)))
              (let ((literal (string->symbol literal-string)))
                (lambda (node)
                  (and (pair? node)
                       (eq? (car node) '*PI*)
                       (equal? (cadr node) literal)))))))
      (node
       ,(lambda (add-on) sxml:node?))
      (point
       ,(lambda (add-on)
          (txp:signal-semantic-error
           "point() NodeTest is not supported by this implementation")))
      (range
       ,(lambda (add-on)
          (txp:signal-semantic-error
           "range() NodeTest is not supported by this implementation")))))
    
    ;-------------
    ; The remaining parameter values return the following
    ; (lambda (nodeset root-node context var-binding) - an SXPath-like
    ; function (it transforms a nodeset into a new nodeset)
    ;  nodeset - a current set of nodes
    ;  root-node - the root of a document (a singleton nodeset)
    ;  context - the context of the node; list of two elements - (position size)
    ;  position - context position (a number)
    ;  size - context size (a number)
    
    ; Parse step implementation
    (step
     ((common
       ,(lambda (axis-res node-test-res predicate-res-lst add-on)
          (let ((axis (car axis-res))
                (root-node-required (cdr axis-res)))
            (if
             (null? predicate-res-lst)
             (lambda (nodeset root-node context var-binding)
               (if root-node-required
                   (((axis node-test-res) root-node) nodeset)
                   ((axis node-test-res) nodeset)))
             (lambda (nodeset root-node context var-binding)
               (map-union
                (lambda (node)
                  (sxml:xpath-nodeset-filter 
                   predicate-res-lst
                   ((if root-node-required
                        ((axis node-test-res) root-node)
                        (axis node-test-res))
                    node)
                   root-node var-binding))
                nodeset))))))
      (range-to
       ,(lambda (expr-res predicate-res-lst add-on)
          (txp:signal-semantic-error "range-to function not implemented")))))
    
    ; Relative location path implementation
    (relative-lpath
     ,(lambda (step-res-lst add-on)
        (if
         (null? (cdr step-res-lst))  ; the only step
         (car step-res-lst)
         (lambda (nodeset root-node context var-binding)
           (let rpt ((nset nodeset)
                     (fs step-res-lst))
             (if (null? fs)
                 nset
                 (rpt ((car fs) nset root-node context var-binding)
                      (cdr fs))))))))
    
    ; Location path implementation
    (location-path
     ((bare-slash
       ,(lambda (add-on)
          (lambda (nodeset root-node context var-binding) root-node)))       
      (slash
       ,(lambda (relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (relative-lpath-res root-node root-node context var-binding))))
      (double-slash
       ,(lambda (relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (relative-lpath-res
             ((sxml:descendant-or-self sxml:node?) root-node)
             root-node context var-binding))))))
    
    ; Predicate implementation
    ; Note that (according to specification) a Predicate must return a number
    ; or a boolean value. However, the return value type is not checked in this
    ; function. This is performed in functions that use 'parse-predicate'
    (predicate
     ,(lambda (expr-res add-on) expr-res))  ; similar to identity function
    
    ; Variable reference implementation
    (variable-ref
     ,(lambda (var-name-string add-on)
        (let ((name (string->symbol var-name-string)))
          (lambda (nodeset root-node context var-binding)
            (cond
              ((assoc name var-binding)
               => cdr)
              (else
               (sxml:xpointer-runtime-error "unbound variable - " name)
               '()))))))
    
    ; Function call implementation
    (function-call
     ,(lambda (fun-name-string arg-res-lst add-on)
        (let ((core-alist
               ; (list fun-name min-num-args max-num-args impl)
               `((last 0 0 ,sxml:core-last)
                 (position 0 0 ,sxml:core-position)
                 (count 1 1 ,sxml:core-count)
                 (id 1 1 ,sxml:core-id)
                 (local-name 0 1 ,sxml:core-local-name)
                 (namespace-uri 0 1 ,sxml:core-namespace-uri)
                 (name 0 1 ,sxml:core-name)
                 (string 0 1 ,sxml:core-string)
                 (concat 2 -1 ,sxml:core-concat)
                 (starts-with 2 2 ,sxml:core-starts-with)
                 (contains 2 2 ,sxml:core-contains)
                 (substring-before 2 2 ,sxml:core-substring-before)
                 (substring-after 2 2 ,sxml:core-substring-after)
                 (substring 2 3 ,sxml:core-substring)
                 (string-length 0 1 ,sxml:core-string-length)
                 (normalize-space 0 1 ,sxml:core-normalize-space)
                 (translate 3 3 ,sxml:core-translate)
                 (boolean 1 1 ,sxml:core-boolean)
                 (not 1 1 ,sxml:core-not)
                 (true 0 0 ,sxml:core-true)
                 (false 0 0 ,sxml:core-false)
                 (lang 1 1 ,sxml:core-lang)
                 (number 0 1 ,sxml:core-number)
                 (sum 1 1 ,sxml:core-sum)
                 (floor 1 1 ,sxml:core-floor)
                 (ceiling 1 1 ,sxml:core-ceiling)
                 (round 1 1 ,sxml:core-round))))
          (cond
           ((assq (string->symbol fun-name-string) core-alist)
            => (lambda (quad)  ; Core function found
                 (cond
                   ((< (length arg-res-lst) (cadr quad))
                    (txp:signal-semantic-error
                     "too few arguments for the Core Function call - "
                     fun-name-string))
                   ((and (> (caddr quad) 0)
                         (> (length arg-res-lst) (caddr quad)))
                    (txp:signal-semantic-error
                     "too many arguments for the Core Function call - "
                     fun-name-string))
                   (else  ; correct number of arguments
                    ; Producing a function implementation
                    (apply (cadddr quad) arg-res-lst)))))
           (else  ; function definition not found
            (txp:signal-semantic-error
             "function call to an unknown function - " fun-name-string))))))
    
    ; Primary expression
    (primary-expr
     ((literal
       ,(lambda (literal add-on)
          (lambda (nodeset root-node context var-binding) literal)))
      (number
       ,(lambda (number add-on)
          (lambda (nodeset root-node context var-binding) number)))))

    ; Filter expression
    (filter-expr
     ,(lambda (primary-expr-res predicate-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let ((nodeset
                 (primary-expr-res nodeset root-node context var-binding)))
            (sxml:xpath-nodeset-filter
             predicate-res-lst
             (cond
               ((nodeset? nodeset) nodeset)
               (else 
                (sxml:xpointer-runtime-error 
                 "expected - nodeset instead of " nodeset)
                '()))
             root-node var-binding)))))
    
    ; Path expression
    (path-expr
     ((slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (let ((nset
                   (filter-expr-res nodeset root-node context var-binding)))
              (let ((nset 
                     (cond
                       ((nodeset? nset) nset)
                       (else 
                        (sxml:xpointer-runtime-error 
                         "expected - nodeset instead of " nset)
                        '()))))
                (relative-lpath-res nset root-node context var-binding))))))
      (double-slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (let ((nset
                   (filter-expr-res nodeset root-node context var-binding)))
              (let ((nset 
                     (cond
                       ((nodeset? nset) nset)
                       (else 
                        (sxml:xpointer-runtime-error 
                         "expected - nodeset instead of " nset)
                        '()))))
                (let ((nset ((sxml:descendant-or-self sxml:node?) nset)))
                  (relative-lpath-res
                   nset root-node context var-binding)))))))))
    
    ; Union expression
    (union-expr
     ,(lambda (path-expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res '())
                    (fs path-expr-res-lst))
            (if
             (null? fs)
             res
             (let ((nset ((car fs) nodeset root-node context var-binding)))
               (rpt
                (append 
                 res
                 (cond
                   ((not (nodeset? nset))
                    (sxml:xpointer-runtime-error 
                     "expected - nodeset instead of " nset)
                    '())
                   (else nset)))
                (cdr fs))))))))
    
    ; Unary expression
    (unary-expr
     ,(lambda (union-expr-res num-minuses add-on)
        (if (even? num-minuses)
            (lambda (nodeset root-node context var-binding)
              (sxml:number
               (union-expr-res nodeset root-node context var-binding)))
            (lambda (nodeset root-node context var-binding)
              (- (sxml:number
                  (union-expr-res nodeset root-node context var-binding)))))))
    
    ; Different operations
    (operations
     ((* ,(lambda (add-on) *))
      (div ,(lambda (add-on) /))
      (mod ,(lambda (add-on) remainder))
      (+ ,(lambda (add-on) +))
      (- ,(lambda (add-on) -))
      (< ,(lambda (add-on) (sxml:relational-cmp <)))
      (> ,(lambda (add-on) (sxml:relational-cmp >)))
      (<= ,(lambda (add-on) (sxml:relational-cmp <=)))
      (>= ,(lambda (add-on) (sxml:relational-cmp >=)))
      (= ,(lambda (add-on) sxml:equal?))
      (!= ,(lambda (add-on) sxml:not-equal?))))
    
    ; Additive and multiplicative expressions
    (mul-expr ,sxml:arithmetic-eval)
    (add-expr ,sxml:arithmetic-eval)
    
    ; Relational expression
    (relational-expr
     ,(lambda (additive-expr-res-lst cmp-op-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res ((car additive-expr-res-lst)
                          nodeset root-node context var-binding))
                    (fs (cdr additive-expr-res-lst))
                    (ops cmp-op-lst))
            (if (null? fs)
                res
                (rpt ((car ops)
                      res
                      ((car fs) nodeset root-node context var-binding))
                     (cdr fs)
                     (cdr ops)))))))        
    
    ; Equality expression
    (equality-expr
     ,(lambda (relational-expr-res-lst cmp-op-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res ((car relational-expr-res-lst)
                          nodeset root-node context var-binding))
                    (fs (cdr relational-expr-res-lst))
                    (ops cmp-op-lst))
            (if (null? fs)
                res
                (rpt ((car ops) 
                      res 
                      ((car fs) nodeset root-node context var-binding))
                     (cdr fs)
                     (cdr ops)))))))
    
    ; And-expression
    ; Note that according to 3.4 in XPath specification, the right operand
    ; is not evaluated if the left operand evaluates to false
    (and-expr
     ,(lambda (equality-expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs equality-expr-res-lst))
            (cond
              ((null? fs) #t)
              ((not (sxml:boolean
                     ((car fs) nodeset root-node context var-binding))) #f)
              (else (rpt (cdr fs))))))))
    
    ; Or-expression
    (or-expr
     ,(lambda (and-expr-res-lst add-on)    
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs and-expr-res-lst))
            (cond
              ((null? fs) #f)
              ((sxml:boolean
                ((car fs) nodeset root-node context var-binding)) #t)
              (else (rpt (cdr fs))))))))
    
    ; Full XPointer
    (full-xptr
     ,(lambda (expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs expr-res-lst))
            (if (null? fs)
                '()
                (let ((nset ((car fs) nodeset root-node context var-binding)))
                  (if (null? nset)
                      (rpt (cdr fs))
                      nset)))))))
    
    ; XPointer child sequence
    (child-seq
     ((with-name
      ,(lambda (name-string number-lst add-on)
         (let ((funcs
                 (apply append
                        (map
                         (lambda (num)
                           (list (sxml:child (ntype?? '*)) (node-pos num)))
                         number-lst))))
           (lambda (nodeset root-node context var-binding)
             (let ((id-nset ((sxml:child (ntype?? 'id-index))
                             ((sxml:child (ntype?? '@@)) root-node))))
               (if
                (null? id-nset)  ; no id-index
                '()
                (let ((nd (sxml:lookup name-string (cdar id-nset))))
                  (if (not nd)
                      '()
                      (let rpt ((nset (list nd))
                                (fs funcs))
                        (if (null? fs)
                            nset
                            (rpt ((car fs) nset) (cdr fs))))))))))))
      (without-name
       ,(lambda (number-lst add-on)
          (let ((funcs
                 (apply append
                        (map
                         (lambda (num)
                           (list (sxml:child (ntype?? '*)) (node-pos num)))
                         number-lst))))
            (lambda (nodeset root-node context var-binding)
              (if (nodeset? nodeset)
                  (let rpt ((nodeset nodeset) (res '()))
                    (if (null? nodeset)
                        res
                        (let rpt2 ((nset (list (car nodeset))) 
                                   (fs funcs))
                          (if (null? fs)
                              (rpt (cdr nodeset) (append res nset))
                              (rpt2 ((car fs) nset) (cdr fs))))))
                  (let rpt ((nodeset nodeset) (fs funcs))
                    (if (null? fs)
                        nodeset
                        (rpt ((car fs) nodeset) (cdr fs)))))))))))                
    ))
     
;=========================================================================
; Highest level API functions

;------------------------------------------------
; 'sxml:xpath' and 'sxml:xpointer' functions
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (lambda (node . var-binding) ...)  
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node . var-binding) ...)  - an SXPath function
;  node - a node (or a node-set) of the SXML document
;  var-binding - XPath variable bindings (an optional argument)
;  var-binding = (list  (var-name . value)
;                       (var-name . value)
;                       ...)
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
; string, nodeset. NOTE: a node must be represented as a singleton nodeset
; 
; Administrative SXPath variables:
;  *root* - if presented in the 'var-binding', its value (a node or a nodeset)
; specifies the root of the SXML document

(define (sxml:api-helper0 parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (lambda (node . var-binding)
            (let ((node (as-nodeset node)))
              (if
               (null? var-binding)  ; no variables supplied
               (res node node (cons 1 1) '())
               (let ((var-binding (car var-binding)))
                 (res
                  node
                  (cond ((assq '*root* var-binding)
                         => (lambda (pair) (as-nodeset (cdr pair))))
                        (else node))
                  (cons 1 1)
                  var-binding)))))))))

(define sxml:classic-res (txp:parameterize-parser sxml:classic-params))

(define (sxml:api-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (lambda (node . var-binding)
            (let ((node (as-nodeset node)))
              (if
               (null? var-binding)  ; no variables supplied
               (res node node (cons 1 1) '())
               (let ((var-binding (car var-binding)))
                 (res
                  node
                  (cond ((assq '*root* var-binding)
                         => (lambda (pair) (as-nodeset (cdr pair))))
                        (else node))
                  (cons 1 1)
                  var-binding)))))))))
              
(define sxml:xpath
  (sxml:api-helper (cadr (assq 'xpath sxml:classic-res))))
(define sxml:xpointer
  (sxml:api-helper (cadr (assq 'xpointer sxml:classic-res))))
(define sxml:xpath-expr
  (sxml:api-helper (cadr (assq 'expr sxml:classic-res))))

; Some (deprecated!) aliases for backward compatibility
; which will be eventually removed
(define sxml:xpath+root+vars sxml:xpath)
(define sxml:xpointer+root+vars sxml:xpointer)
(define sxml:xpath+root sxml:xpath)
(define txpath sxml:xpath)


;------------------------------------------------
; 'sxml:xpath+index' and 'sxml:xpointer+index' functions
;
; NOTE: THESE FUNCTIONS ARE JUST STUBS NOW, BECAUSE THEY ALWAYS RETURN #t
; FOR 'index-required'. THESE FUNCTIONS ARE INCLUDED HERE FOR THE SAKE OF
; BACKWARD COMPATIBILITY ONLY.
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (cons (lambda (node . id-index) ...)  
;                              index-required )
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node) ...)  - an SXPath function
;  node - a root node of the SXML document
;  index-required - a boolean value: whether an id-index is required

(define (sxml:api-index-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (cons
           (lambda (node)
             (let ((node (as-nodeset node)))
               (res node node (cons 1 1) '())))
           #t)))))
     
(define sxml:xpath+index
  (sxml:api-index-helper (cadr (assq 'xpath sxml:classic-res))))
(define sxml:xpointer+index
  (sxml:api-index-helper (cadr (assq 'xpointer sxml:classic-res))))










;; W3C compliant extensions to SXPathlib
; $Id: sxpath-ext.scm,v 1.911 2002/12/06 22:10:53 kl Exp kl $:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; SXML counterparts to W3C XPath Core Functions Library

; The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
; Converts a given object to a string
; NOTE:
;  1. When converting a nodeset - a document order is not preserved
;  2. number->string function returns the result in a form which is slightly
; different from XPath Rec. specification
(define (sxml:string object)
  (cond
    ((string? object) object)
    ((nodeset? object) (if (null? object)
			 ""
			 (sxml:string-value (car object))))
    ((number? object)
     (if (and (rational? object) (not (integer? object)))  ; like 1/2
         (number->string (exact->inexact object))
         (number->string object)))
    ((boolean? object) (if object "true" "false"))
    (else "")))  ; Unknown type -> empty string. 
                 ; Option: write its value to string port?

; The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
; Converts its argument to a boolean
(define (sxml:boolean object)
  (cond
    ((boolean? object) object)
    ((number? object) (not (= object 0)))
    ((string? object) (> (string-length object) 0))
    ((nodeset? object) (not (null? object)))
    (else #f)))  ; Not specified in XPath Rec.

; The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
; Converts its argument to a number
; NOTE: 
;  1. The argument is not optional (yet?)
;  2. string->number conversion is not IEEE 754 round-to-nearest
;  3. NaN is represented as 0
(define (sxml:number obj)
  (cond
    ((number? obj) obj)
    ((string? obj)
     (let ((nmb (call-with-input-string obj read)))
       (if (number? nmb)
	 nmb
	 0))) ; NaN
    ((boolean? obj) (if obj 1 0))
    ((nodeset? obj) (sxml:number (sxml:string obj)))
    (else 0))) ; unknown datatype

; Returns a string value for a given node in accordance to
; XPath Rec. 5.1 - 5.7 
(define (sxml:string-value node)
  (cond
    ((not (pair? node))  ; a text node or data node
     (sxml:string node))
    ((null? (cdr node))
     "")
    (else
     (apply string-append  ; a list of arguments is always non-null
            (map
             (lambda (node)
               (if (sxml:node? node)  ; not annot-attr node or aux list node
                   (sxml:string-value node) ""))
             (cdr node))))))

; Select SXML element by its unique IDs
; XPath Rec. 4.1
;  object - a nodeset or a datatype which can be converted to a string by means
; of a 'string' function
;  id-index = ( (id-value . element) (id-value . element) ... ) 
; This index is used for selection of an element by its unique ID. 
; The result is a nodeset
(define (sxml:id id-index)
  (lambda(object)
    (if (nodeset? object)
      (let loop ((str-lst (map sxml:string-value object))
		 (res '()))
	(if (null? str-lst)
	  (reverse res)
	  (let ((node (sxml:lookup (car str-lst) id-index)))
	    (if (not node)  ; no such element
	      (loop (cdr str-lst) res)
	      (loop (cdr str-lst) (cons node res))))))
      (let rpt ((lst (string->list (sxml:string object)))
		(tmp '())
		(res '()))
	(cond
	  ((null? lst)
	   (if (null? tmp) 
	     (reverse res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (reverse res)
		 (reverse (cons node res))))))
	  ((member (car lst) '(#\space #\return #\newline #\tab))
	   (if (null? tmp)
	     (rpt (cdr lst) tmp res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (rpt (cdr lst) '() res)
		 (rpt (cdr lst) '() (cons node res))))))
	  (else (rpt (cdr lst) (cons (car lst) tmp) res)))))))
                       
             
;=========================================================================
; Comparators for XPath objects 

; Implements XPath equality comparison in a straightforward nested loop manner
(define (sxml:nested-loop-join string-set1 string-set2 string-op)
  (let first ((str-set1 string-set1)
              (str-set2 string-set2))
    (cond
      ((null? str-set1) #f)
      ((let second ((elem (car str-set1))
                    (set2 str-set2))
         (cond
           ((null? set2) #f)
           ((string-op elem (car set2)) #t)
           (else (second elem (cdr set2))))) #t)
      (else
       (first (cdr str-set1) str-set2)))))

;-------------------------------------------------
; Merge-sort for speeding up equality comparison of two nodesets

; Similar to R5RS 'list-tail' but returns the new list consisting of the first
; 'k' members of 'lst'
(define (sxml:list-head lst k)
  (if (or (null? lst) (zero? k))
      '()
      (cons (car lst) (sxml:list-head (cdr lst) (- k 1)))))

; Implements merge-sort of the given lst
; Returns the sorted list, the smallest member first
; less-than?-pred ::= (lambda (obj1 obj2) ...)
; less-than?-pred returns #t if obj1<obj2 with respect to the given ordering
(define (sxml:merge-sort less-than?-pred lst)
  (letrec
      ((merge-sorted-lists
        ; Merges 2 sorted lists into one sorted list
        (lambda (lst1 lst2)
          (cond
            ((null? lst1) lst2)
            ((null? lst2) lst1)
            ; both lists are non-null here
            ((less-than?-pred (car lst1) (car lst2))
             (cons (car lst1)
                   (merge-sorted-lists (cdr lst1) lst2)))
            (else
             (cons (car lst2)
                   (merge-sorted-lists lst1 (cdr lst2))))))))
    (if
     (or (null? lst) (null? (cdr lst)))  ; already sorted
     lst
     (let ((middle (inexact->exact (round (/ (length lst) 2)))))
       (merge-sorted-lists
        (sxml:merge-sort less-than?-pred (sxml:list-head lst middle))
        (sxml:merge-sort less-than?-pred (list-tail lst middle)))))))

; Implementation of XPath equality comparison for 2 string-sets with
; merge-sort join algorithm
(define (sxml:merge-sort-join string-set1 string-set2 string-op)
  (let loop ((str-set1 (sxml:merge-sort string<? string-set1))
             (str-set2 (sxml:merge-sort string<? string-set2)))
    (cond
      ((or (null? str-set1) (null? str-set2))
       #f)
      ((string-op (car str-set1) (car str-set2))
       ; comparison condition fulfilled for a pair of nodes
       #t)
      ((string<? (car str-set1) (car str-set2))
       ; we can remove (car str-set1) from our further consideration
       (loop (cdr str-set1) str-set2))
      (else  ; vice versa
       (loop str-set1 (cdr str-set2))))))

;-------------------------------------------------
; Radix-sort join for equality comparison of 2 nodesets
; The running time of the algorithm is proportional to the nodeset size and
; to node string-value length
; 
; Since each nodeset contains O(n) nodes and string-value for each node is
; O(n) in length, radix-sort join algorithm evaluates in O(n^2) time. By
; comparison, nested loop join requires O(n^3) time, merge-sort join
; implemented above requires O(n^2*log(n)).
;
; On the other hand, radix-sort join is time-ineffective for relatively small
; nodesets being joined. For small nodesets, the above implemented sort-merge
; join runs more effectively. Radix-sort join is promising for large nodesets.

; Represents a list of chars as a branch in the string-tree
; The list of chars must be non-empty
(define (sxml:charlst->branch lst)
  (if (null? (cdr lst))  ; this is the last character in the lst
      `(,(car lst) #t)
      `(,(car lst) #f ,(sxml:charlst->branch (cdr lst)))))

; Converts a string to a string-tree
(define (sxml:string->tree str)
  (let ((lst (string->list str)))
    (if (null? lst)   ; an empty string is given
        '(*top* #t)
        `(*top* #f ,(sxml:charlst->branch lst)))))

; Adds a new string to string-tree
; In a special case, tree257 may be #f. The function than creates a new tree,
; which contains just the representation for str
(define (sxml:add-string-to-tree str tree)
  (letrec
      ((add-lst-to-tree   ; adds the list of chars to tree
        (lambda (lst tree)
          (if
           (null? lst)  ; the lst is over
           (if
            (cadr tree)  ; whether it is already in the tree
            tree
            (cons (car tree)
                  (cons #t (cddr tree))))
           (let ((curr-char (car lst)))
             (let iter-alist ((alist (cddr tree))
                              (res (list (cadr tree) (car tree))))
               (cond
                 ((null? alist)  ; branch not in a tree
                  (reverse
                   (cons
                    (sxml:charlst->branch lst)
                    res)))
                 ((char=? (caar alist) curr-char)  ; entry found
                  (if
                   (null? (cdr alist))  ; nothing more in the alist
                   (reverse
                    (cons
                     (add-lst-to-tree (cdr lst) (car alist))
                     res))
                   (append
                    (reverse
                     (cons
                      (add-lst-to-tree (cdr lst) (car alist))
                      res))
                    (cdr alist))))
                 ((char>? (caar alist) curr-char)
                  (if
                   (null? (cdr alist))  ; nothing more in the alist
                   (reverse
                    (cons (car alist)
                          (cons (sxml:charlst->branch lst) res)))
                   (append
                    (reverse
                     (cons
                      (sxml:charlst->branch lst)
                      res))
                    alist)))
                 (else
                  (iter-alist (cdr alist)
                              (cons (car alist) res))))))))))
    (add-lst-to-tree (string->list str) tree)))

; Whether a given string is presented in the string-tree
(define (sxml:string-in-tree? str tree)  
  (let loop ((lst (string->list str))
             (tree tree))
    (cond
      ((null? lst)  ; the string is over
       (cadr tree))
      ((assv (car lst) (cddr tree))             
       => (lambda (new-tree)
            (loop (cdr lst) new-tree)))
      (else #f))))
        
; XPath equality comparison for 2 string-sets
;  bool-op - comparison function for 2 boolean values
(define (sxml:radix-sort-join string-set1 string-set2 bool-op)
  (if
   (null? string-set1)  ; always #f
   #f
   (let ((tree
          (let iter-1 ((set1 (cdr string-set1))
                       (tree (sxml:string->tree (car string-set1))))
            (if (null? set1)
                tree
                (iter-1 (cdr set1)
                        (sxml:add-string-to-tree (car set1) tree))))))
     (let iter-2 ((set2 string-set2))
       (cond
         ((null? set2)  ; equality not found
          #f)
         ((bool-op (sxml:string-in-tree? (car set2) tree) #t)
          #t)
         (else
          (iter-2 (cdr set2))))))))

;-------------------------------------------------
; Equality comparison

; A helper for XPath equality operations: = , !=
;  'bool-op', 'number-op' and 'string-op' are comparison operations for 
; a pair of booleans,  numbers and strings respectively
(define (sxml:equality-cmp bool-op number-op string-op)
  (lambda (obj1 obj2)
    (cond
      ((and (not (nodeset? obj1)) (not (nodeset? obj2)))  
       ; neither object is a nodeset
       (cond
         ((boolean? obj1) (bool-op obj1 (sxml:boolean obj2)))
         ((boolean? obj2) (bool-op (sxml:boolean obj1) obj2))
         ((number? obj1) (number-op obj1 (sxml:number obj2)))
         ((number? obj2) (number-op (sxml:number obj1) obj2))
         (else  ; both objects are strings
          (string-op obj1 obj2))))
      ((and (nodeset? obj1) (nodeset? obj2))  ; both objects are nodesets
       (let ((lng1 (length obj1))
             (lng2 (length obj2)))
         (cond
           ((and (< lng1 100000) (< lng2 100000))
            ((if  ; either nodeset is a short one              
              (or (<= lng1 2) (<= lng2 2))
              sxml:nested-loop-join
              sxml:merge-sort-join)
             (map sxml:string-value obj1)
             (map sxml:string-value obj2)
             string-op))
           ((< lng1 lng2)            
            (sxml:radix-sort-join (map sxml:string-value obj1)
                                  (map sxml:string-value obj2)
                                  bool-op))
           (else  ; lng2 < lng1
            (sxml:radix-sort-join (map sxml:string-value obj2)
                                  (map sxml:string-value obj1)
                                  bool-op)))))
      (else  ; one of the objects is a nodeset, another is not
       (call-with-values
        (lambda ()  ; Equality operations are commutative
          (if (nodeset? obj1) (values obj1 obj2) (values obj2 obj1)))
        (lambda (nset elem)
          (cond
            ((boolean? elem) (bool-op elem (sxml:boolean nset)))
            ((number? elem)
             (let loop ((nset 
                         (map
                          (lambda (node) (sxml:number (sxml:string-value node)))
                          nset)))
               (cond
                 ((null? nset) #f)
                 ((number-op elem (car nset)) #t)
                 (else (loop (cdr nset))))))
            ((string? elem)
             (let loop ((nset (map sxml:string-value nset)))
               (cond
                 ((null? nset) #f)
                 ((string-op elem (car nset)) #t)
                 (else (loop (cdr nset))))))
            (else  ; unknown datatype
             (cerr "Unknown datatype: " elem nl)
             #f))))))))

(define sxml:equal? (sxml:equality-cmp eq? = string=?))

(define sxml:not-equal?
  (sxml:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2)))))

;-------------------------------------------------
; Relational comparison

; Relational operation ( < , > , <= , >= ) for two XPath objects
;  op is comparison procedure: < , > , <= or >=
(define (sxml:relational-cmp op)
  (lambda (obj1 obj2)
    (cond
      ((not (or (nodeset? obj1) (nodeset? obj2)))  ; neither obj is a nodeset
       (op (sxml:number obj1) (sxml:number obj2)))
      ((boolean? obj1)  ; 'obj1' is a boolean, 'obj2' is a nodeset
       (op (sxml:number obj1) (sxml:number (sxml:boolean obj2))))
      ((boolean? obj2)  ; 'obj1' is a nodeset, 'obj2' is a boolean
       (op (sxml:number (sxml:boolean obj1)) (sxml:number obj2)))
      ((or (null? obj1) (null? obj2)) ; one of the objects is an empty nodeset
       #f)
      (else  ; at least one object is a nodeset
       (op
        (cond
          ((nodeset? obj1)  ; 'obj1' is a (non-empty) nodeset
           (let ((nset1 (map
                         (lambda (node) (sxml:number (sxml:string-value node)))
                         obj1)))
             (let first ((num1 (car nset1))
                         (nset1 (cdr nset1)))
               (cond
                 ((null? nset1) num1)
                 ((op num1 (car nset1)) (first num1 (cdr nset1)))
                 (else (first (car nset1) (cdr nset1)))))))
          ((string? obj1) (sxml:number obj1))
          (else  ; 'obj1' is a number
           obj1))
        (cond
          ((nodeset? obj2)  ; 'obj2' is a (non-empty) nodeset
           (let ((nset2 (map
                         (lambda (node) (sxml:number (sxml:string-value node)))
                         obj2)))
             (let second ((num2 (car nset2))
                          (nset2 (cdr nset2)))
               (cond
                 ((null? nset2) num2)
                 ((op num2 (car nset2)) (second (car nset2) (cdr nset2)))
                 (else (second num2 (cdr nset2)))))))
          ((string? obj2) (sxml:number obj2))
          (else  ; 'obj2' is a number
           obj2)))))))
           

;=========================================================================
; XPath axes
; An order in resulting nodeset is preserved

; Ancestor axis
(define (sxml:ancestor test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:ancestor test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    '()
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxml:filter test-pred?) (cdr path))
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxml:attribute (ntype?? '*)) (car path))
			   ((sxml:child sxml:node?) (car path))))
		       (cdr paths)))))))))))

; Ancestor-or-self axis
(define (sxml:ancestor-or-self test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:ancestor-or-self test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    ((sxml:filter test-pred?) (list node))
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxml:filter test-pred?) path)
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxml:attribute (ntype?? '*)) (car path))
			   ((sxml:child sxml:node?) (car path))))
		       (cdr paths)))))))))))
                                                                      
; Descendant axis
; It's similar to original 'node-closure' a resulting nodeset is 
; in depth-first order rather than breadth-first
; Fix: din't descend in non-element nodes!
(define (sxml:descendant test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxml:descendant test-pred?) node)
      (let rpt ((res '())
		(more ((sxml:child sxml:node?) node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxml:child sxml:node?) (car more))
		       (cdr more))))))))

; Descendant-or-self axis
(define (sxml:descendant-or-self test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxml:descendant-or-self test-pred?) node)
      (let rpt ((res '())
		(more (list node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxml:child sxml:node?) (car more))
		       ; sxml:node?
		       (cdr more))))))))

; Following axis
(define (sxml:following test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:following test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list root-node)
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((eq? (caar seq) node)
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append 
			res
			((sxml:descendant-or-self test-pred?) (car seq)))))))
	    ((and (sxml:element? (caar seq))
		  (memq node (sxml:attr-list (caar seq))))
	     (let rpt ((sq (cdr (apply append seq)))
		       (res ((sxml:descendant test-pred?) (caar seq))))
	       (if (null? sq)
		 res
		 (rpt (cdr sq)
		      (append res
			      ((sxml:descendant-or-self test-pred?) (car sq)))))))
	    (else
	      (loop (cons 
		      ((sxml:child sxml:node?) (caar seq))
		      (cons (cdar seq) (cdr seq)))))))))))

; Following-sibling axis
(define (sxml:following-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:following-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map (sxml:child sxml:node?)
			      (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxml:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))

; Namespace axis
(define (sxml:namespace test-pred?)
  (lambda (node)   ; node or nodeset
    ((sxml:filter test-pred?) 
     (sxml:ns-list node))))

; Preceding axis
(define (sxml:preceding test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxml:preceding test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list (reverse root-node))
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((or (eq? (caar seq) node)
		 (not (null? ((sxml:attribute 
				(lambda (n)
				  (eq? n node))) 
			      (caar seq)))))
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append res
			      (reverse ((sxml:descendant-or-self test-pred?) 
					(car seq))))))))
	    (else (loop (cons (reverse ((sxml:child sxml:node?) (caar seq)))
			      (cons (cdar seq) (cdr seq)))))))))))

; Preceding-sibling axis
(define (sxml:preceding-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if(nodeset? node)
	(map-union ((sxml:preceding-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map
			   (lambda (n)
			     (reverse ((sxml:child sxml:node?) n)))
			   (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxml:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))





;; $Id: sxpath.scm,v 1.5 2005/09/07 09:27:34 lizorkin Exp $
;; Highghest level SXPath 
;; Refactored from sxml-tools.scm and sxpathlib.scm

;==============================================================================
; Abbreviated SXPath

; Evaluate an abbreviated SXPath
;	sxpath:: AbbrPath -> Converter, or
;	sxpath:: AbbrPath -> Node|Nodeset -> Nodeset
; AbbrPath is a list. It is translated to the full SXPath according
; to the following rewriting rules
; (sxpath '()) -> (node-join)
; (sxpath '(path-component ...)) ->
;		(node-join (sxpath1 path-component) (sxpath '(...)))
; (sxpath1 '//) -> (sxml:descendant-or-self sxml:node?)
; (sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
; (sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
; (sxpath1 '(*or* ...))  -> (select-kids (ntype-names??
;                                          (cdr '(*or* ...))))
; (sxpath1 '(*not* ...)) -> (select-kids (sxml:complement 
;                                         (ntype-names??
;                                          (cdr '(*not* ...)))))
; (sxpath1 '(ns-id:* x)) -> (select-kids 
;                                      (ntype-namespace-id?? x))
; (sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
; (sxpath1 ?string)     -> (txpath ?string)
; (sxpath1 procedure)   -> procedure
; (sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))
; (sxpath1 '(path reducer ...)) ->
;		(node-reduce (sxpath path) (sxpathr reducer) ...)
; (sxpathr number)      -> (node-pos number)
; (sxpathr path-filter) -> (filter (sxpath path-filter))
(define (sxpath path . ns-binding)
  (let ((ns-binding (if (null? ns-binding) ns-binding (car ns-binding))))
  (let loop ((converters '())
             (root-vars '())  ; a list of booleans, one per location step:
	                      ;  #t - location step function is binary
                              ;  #f - location step function is unary
             (path (if (string? path) (list path) path)))
    (cond
      ((null? path)  ; parsing is finished
       (lambda (node . var-binding)
         (let ((var-binding
                (if (null? var-binding) var-binding (car var-binding))))
           (let rpt ((nodeset (as-nodeset node))
                     (conv (reverse converters))
                     (r-v (reverse root-vars)))
             (if
              (null? conv)  ; the path is over
              nodeset
              (rpt
               (if (car r-v)  ; the current converter consumes 2 arguments
                   ((car conv) nodeset var-binding)
                   ((car conv) nodeset))
               (cdr conv)
               (cdr r-v)))))))
      ; *or* handler
      ((and (pair? (car path)) 
            (not (null? (car path)))
            (eq? '*or* (caar path)))
       (loop (cons (select-kids (ntype-names?? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ; *not* handler 
      ((and (pair? (car path)) 
            (not (null? (car path)))
            (eq? '*not* (caar path)))
       (loop (cons
              (select-kids (sxml:complement (ntype-names?? (cdar path))))
              converters)
             (cons #f root-vars)
             (cdr path)))
      ((procedure? (car path))
       (loop (cons (car path) converters)
             (cons #t root-vars)
             (cdr path)))
      ((eq? '// (car path))
       (if (or (null? (cdr path))
               (not (symbol? (cadr path)))
               (eq? (cadr path) '@))
           (loop (cons (sxml:descendant-or-self sxml:node?)
                       converters)
                 (cons #f root-vars)
                 (cdr path))
           (loop (cons (sxml:descendant (ntype?? (cadr path)))
                       converters)
                 (cons #f root-vars)
                 (cddr path))))
      ((symbol? (car path))
       (loop (cons (select-kids (ntype?? (car path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ((string? (car path))
       (and-let*
        ((f (sxml:xpath-expr (car path) ns-binding)))  ; DL: was: txpath
        (loop (cons f converters)
              (cons #t root-vars)
              (cdr path))))
      ((and (pair? (car path)) (eq? 'equal? (caar path)))
       (loop (cons (select-kids (apply node-equal? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))
      ; ns-id:* handler 
      ((and (pair? (car path)) (eq? 'ns-id:* (caar path)))
       (loop
        (cons (select-kids (ntype-namespace-id?? (cadar path))) converters)
        (cons #f root-vars)
        (cdr path)))
      ((and (pair? (car path)) (eq? 'eq? (caar path)))
       (loop (cons (select-kids (apply node-eq? (cdar path))) converters)
             (cons #f root-vars)
             (cdr path)))      
      ((pair? (car path))
       (and-let*
        ((select
          (if
           (symbol? (caar path))
           (lambda (node . var-binding)
             ((select-kids (ntype?? (caar path))) node))
           (sxpath (caar path) ns-binding))))
        (let reducer ((reducing-path (cdar path))
                      (filters '()))
          (cond
            ((null? reducing-path)
             (loop
              (cons
               (lambda (node var-binding)
                 (map-union
                  (lambda (node)
                    (let label ((nodeset (select node var-binding))
                                (fs (reverse filters)))
                      (if
                       (null? fs)
                       nodeset
                       (label
                        ((car fs) nodeset var-binding)
                        (cdr fs)))))
                  (if (nodeset? node) node (list node))))
               converters)
              (cons #t root-vars)
              (cdr path)))
            ((number? (car reducing-path))
             (reducer
              (cdr reducing-path)
              (cons
               (lambda (node var-binding)
                 ((node-pos (car reducing-path)) node))
               filters)))
            (else
             (and-let*
              ((func (sxpath (car reducing-path) ns-binding)))
              (reducer
               (cdr reducing-path)
               (cons
                (lambda (node var-binding)
                  ((sxml:filter
                    (lambda (n) (func n var-binding)))
                    node))
                filters))))))))
      (else
       (cerr "Invalid path step: " (car path))
       #f)))))


;==============================================================================
; Wrappers 

; sxpath always returns a list, which is #t in Scheme 
; if-sxpath returns #f instead of empty list
(define (if-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) #f x))))

; Returns first node found, if any.
; Otherwise returns #f.
(define (if-car-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) #f (car x)))))

; Returns first node found, if any.
; Otherwise returns empty list.
(define (car-sxpath path)
  (lambda (obj)
   (let ((x ((sxpath path) obj)))
     (if (null? x) '() (car x)))))

;==============================================================================
; lookup by a value of ID type attribute
; See also sxml:lookup in sxml-tools

; Built an index as a list of (ID_value . element) pairs for given
; node. lpaths are location paths for attributes of type ID.
(define (sxml:id-alist node . lpaths)
  (apply
    append
    (map 
      (lambda(lp)
	(let ((lpr (reverse lp)))
	  (map 
	    (lambda (nd)
	      (cons (sxml:attr nd (car lpr))
		    nd))
	    ; Selects elements with ID attributes
	    ;  using (lpath ,(node-self (sxpath '(@ attrname))))
	    ((sxpath (reverse (cons 
			  (lambda(n r+v)
			   ((node-self (sxpath `(@ ,(car lpr)))) n))
				(cddr lpr)))) node))   
	  ))
      lpaths)))




))))
