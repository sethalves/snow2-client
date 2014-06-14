;; sxml-serializer -- Serialize SXML to XML and HTML4
;; Uses public domain code from sxml-tools by Dmitry Lizorkin
;; Chicken port Copyright (C) 2010 Jim Ursetto.  All Rights Reserved.
;; License: BSD.

;; Changes over stock:
;; Add allow-prefix-redeclarations? option.  Allows user to provide multiple namespace
;;   URIs that map to the same prefix in ns-prefixes.  Has no effect in attributes.
;;   Currently, does not permit redeclarations in *NAMESPACES* except via original-prefix
;;   (which is the same as using ns-prefixes).
;; Add fake namespace prefix *default*.  Namespace URIs associated with this prefix
;; are assigned the default namespace xmlns="..." and elements with
;; no namespace reset the default to xmlns="".

(define-library (seth xml sxml-serializer)
 (export serialize-sxml
         conventional-ns-prefixes
         allow-prefix-redeclarations?

  ;; These currently offer little benefit over plain serialize-sxml.
  ;; sxml->xml
  ;; sxml->xml/noindent
  ;; sxml->html
  ;; sxml->html/noindent
  
  )

(import (scheme base)
        (scheme char)
        (scheme write)
        (scheme cxr)
        (scheme file)
        (only (srfi 1) filter)
        (only (srfi 13) string-concatenate))

(include "serializer.scm")

(begin

(define uri-error error)

(define sxml->xml srl:sxml->xml)
(define sxml->xml/noindent srl:sxml->xml-noindent)
(define sxml->html srl:sxml->html)
(define sxml->html/noindent srl:sxml->html-noindent)
(define display-sxml srl:display-sxml)
(define sxml->string srl:sxml->string)

(define srl:apply-string-append string-concatenate)

;; override srl:conventional-ns-prefixes so that sxml->xml etc. use the extended list
(define srl:conventional-ns-prefixes
  '((admin . "http://webns.net/mvcb/")
    (atom . "http://www.w3.org/2005/Atom")
    (cc . "http://web.resource.org/cc/")
    (content . "http://purl.org/rss/1.0/modules/content/")
    (dc . "http://purl.org/dc/elements/1.1/")
    (feedburner . "http://rssnamespace.org/feedburner/ext/1.0")
    (fo . "http://www.w3.org/1999/XSL/Format")
    (geo . "http://www.w3.org/2003/01/geo/wgs84_pos#")
    (georss . "http://www.georss.org/georss")
    (itunes . "http://www.itunes.com/dtds/podcast-1.0.dtd")
    (media . "http://search.yahoo.com/mrss/")
    (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rng . "http://relaxng.org/ns/structure/1.0")
    (rss . "http://purl.org/rss/1.0/") 
    (slash . "http://purl.org/rss/1.0/modules/slash/")
    (sy . "http://purl.org/rss/1.0/modules/syndication/")
    (taxo . "http://purl.org/rss/1.0/modules/taxonomy/")
    (thr . "http://purl.org/syndication/thread/1.0")
    (wiki . "http://purl.org/rss/1.0/modules/wiki/")
    (wfw . "http://wellformedweb.org/CommentAPI/")
    (xhtml . "http://www.w3.org/1999/xhtml")
    (xlink . "http://www.w3.org/1999/xlink")
    (xqx . "http://www.w3.org/2005/XQueryX")
    (xsd . "http://www.w3.org/2001/XMLSchema")
    (xsi . "http://www.w3.org/2001/XMLSchema-instance")
    (xsl . "http://www.w3.org/1999/XSL/Transform")))

(define conventional-ns-prefixes srl:conventional-ns-prefixes)
(define allow-prefix-redeclarations? (make-parameter #t))  ; use #f for stock compatibility

;; serialize-sxml: replacement for srl:parameterizable using keyword args
;; instead of (k . v) pairs.
;; Currently disallows xml-declaration emission because the interface is silly and
;; it doesn't provide an "encoding" option, and because if there is a (*PI* xml ...)
;; in the document it will either emit two, or omit only one.
(define (serialize-sxml~ sxml-obj
                         output
                         cdata-section-elements
                         indent
                         method
                         ns-prefixes
                         allow-prefix-redeclarations)
  (let ((omit-xml-declaration #t)       ;; Force omission of xml-declaration
        (standalone 'omit)
        (version "1.0")
        ;; Don't prefix "" URIs.  Could go in conventional-ns-prefixes instead,
        ;; but overriding ns-prefixes would kill that behavior.
        (ns-prefixes (append ns-prefixes '((*default* . ""))))
        )
    (parameterize ((allow-prefix-redeclarations? allow-prefix-redeclarations)) ; redundant?
      (if output
          (srl:display-sxml sxml-obj output
                            cdata-section-elements indent
                            method ns-prefixes
                            omit-xml-declaration standalone version)
          (srl:sxml->string sxml-obj
                            cdata-section-elements indent
                            method ns-prefixes
                            omit-xml-declaration standalone version)
          ))))


(define (serialize-sxml sxml-obj . args)
  (let loop ((key/values args)
             (output #f)
             (cdata-section-elements '())
             (indent "  ")
             (method 'xml)
             (ns-prefixes conventional-ns-prefixes)
             (allow-prefix-redeclarations (allow-prefix-redeclarations?)))
    (cond
     ((null? key/values)
      (serialize-sxml~ sxml-obj output cdata-section-elements indent method
                       ns-prefixes allow-prefix-redeclarations))
     ((null? (cdr key/values))
      (uri-error "malformed arguments to serialize-sxml"))
     ((not (memq (car key/values)
                 '(output cdata-section-elements indent method ns-prefixes
                          allow-prefix-redeclarations)))
      (uri-error "unknown argument keyword to serialize-sxml" (car key/values)))
     (else
      (let ((key (car key/values))
            (value (cadr key/values)))
        (loop (cddr key/values)
              (if (eq? key 'output) value output)
              (if (eq? key 'cdata-section-elements)
                  value cdata-section-elements)
              (if (eq? key 'indent) value indent)
              (if (eq? key 'method) value method)
              (if (eq? key 'ns-prefixes) value ns-prefixes)
              (if (eq? key 'allow-prefix-redeclarations)
                  value allow-prefix-redeclarations)))))))




;;; changes


(define (srl:qname->string prefix-string local-part)
  (if (and prefix-string
           (not (string=? prefix-string "*default*")))
      (string-append prefix-string ":" local-part)
      local-part))

(define (srl:namespace-decl->str-lst prefix-string namespace-uri)
  (if (string=? prefix-string "*default*")
      (list " xmlns" "=\""
            (srl:string->att-value namespace-uri) "\"")
      (list " xmlns:" prefix-string "=\""
            (srl:string->att-value namespace-uri) "\"")))

;; Similar to data-structures#alist-update!, but is non-destructive.
;; Returns a new list with (key . val) consed onto the front;
;; if KEY already exists in the alist, that pair is omitted from the
;; returned list.  Currently traverses the entire list and removes all matching keys.
(define (alist-update key val alist . maybe-cmp)
  (let ((cmp (if (pair? maybe-cmp) (car maybe-cmp) eqv?)))
    (cons (cons key val)
          (let loop ((alist alist) (res '()))
            (cond ((null? alist)
                   (reverse res))
                  ((cmp key (caar alist))
                   (loop (cdr alist) res))
                  (else
                   (loop (cdr alist) (cons (car alist) res))))))))

;; Changes: When declaring a namespace prefix, remove any existing matching prefixes
;; from the declaration list, so new URIs shadow old ones with the same prefix.
;; Changes are marked with [+].
(define (srl:construct-start-end-tags
         elem method
         ns-prefix-assig namespace-assoc declared-ns-prefixes)
  (let ((ns-assoc-here (srl:namespace-assoc-for-elem elem))
        (empty? (srl:empty-elem? elem)))
    (let ((ns-prefix-assig
           (append
            (srl:extract-original-prefix-binding ns-assoc-here)
            ns-prefix-assig))
          (namespace-assoc
           (append ns-assoc-here namespace-assoc)))
      (call-with-values
       (lambda ()           
         (srl:name->qname-components  ; element name
          (car elem) ns-prefix-assig namespace-assoc declared-ns-prefixes #f))
       (lambda (elem-prefix elem-uri elem-local elem-decl-required?)
         (let loop ((attrs
                     (reverse
                      ((srl:select-kids 
                        (lambda (node)  ; not SXML 3.0 aux-list
                          (and (pair? node) (not (eq? (car node) '@)))))
                       ((srl:select-kids
                         (lambda (node)
                           (and (pair? node) (eq? (car node) '@))))
                        elem))))
                    (start-tag
                     (if
                      (or (not empty?)
                          (and (eq? method 'html)
                               (not elem-prefix)
                               (srl:member-ci
                                elem-local
                                ; ATTENTION: should probably move this list
                                ; to a global const
                                '("area" "base" "basefont" "br" "col"
                                  "frame" "hr" "img" "input" "isindex"
                                  "link" "meta" "param"))))
                      '(">") '(" />")))
                    (ns-prefix-assig ns-prefix-assig)
                    (namespace-assoc namespace-assoc)
                    (declared-ns-prefixes
                     ; As if element namespace already declared
                     (if elem-decl-required?
                         (alist-update elem-prefix elem-uri  ;; [+]
                                       declared-ns-prefixes
                                       string=?)
                         declared-ns-prefixes)))
           (if
            (null? attrs)  ; attributes scanned
            (let ((elem-name (srl:qname->string elem-prefix elem-local)))
              (values
               (cons "<"
                     (cons elem-name
                           (if
                            elem-decl-required?
                            (cons
                             (srl:namespace-decl->str-lst elem-prefix elem-uri)
                             start-tag)
                            start-tag)))
               (if empty? #f
                   (list "</" elem-name ">"))
               ns-prefix-assig
               namespace-assoc
               declared-ns-prefixes))
            (call-with-values
             (lambda ()
               (srl:name->qname-components
                (caar attrs)  ; attribute name
                ns-prefix-assig namespace-assoc declared-ns-prefixes #t))
             (lambda (attr-prefix attr-uri attr-local attr-decl-required?)
               (let ((start-tag
                      (cons
                       (srl:attribute->str-lst
                        attr-prefix attr-local
                        ; TODO: optimize for HTML output method
                        (if (null? (cdar attrs))  ; no attribute value
                            attr-local
                            (cadar attrs))
                        method)
                       start-tag)))
                 (loop
                  (cdr attrs)
                  (if attr-decl-required?
                      (cons (srl:namespace-decl->str-lst attr-prefix attr-uri)
                            start-tag)
                      start-tag)
                  ns-prefix-assig
                  namespace-assoc
                  (if attr-decl-required?
                      ;; alist-update not required if attributes cannot redeclare any prefixes,
                      ;; but to be on the safe side...
                      (alist-update attr-prefix attr-uri declared-ns-prefixes string=?)
                      declared-ns-prefixes))))))))))))

;; Changes: check (allow-prefix-redeclarations) parameter before denying XML prefix
;; redeclarations.  Requires declared-ns-prefixes to contain unique keys (prefixes).
;; - Also have empty namespace signal a declaration of "" is required if a non-empty
;; *default* namespace is defined.  Empty namespace declaration is considered
;; to be ("*default*" . "") so it overwrites any previous default declaration.
;; - Also disallow prefix redeclarations in attributes, avoiding multiple declarations
;; of the same namespace prefix in one tag.  An unintended consequence is that you
;; can't then redeclare a prefix declared in ANY parent; to fix this we'd have
;; to track namespaces assigned per-attribute.
(define (srl:name->qname-components
         name ns-prefix-assig namespace-assoc declared-ns-prefixes attribute?)
  (let ((use-ns-id-or-generate-prefix
         (lambda (ns-id)
           (if
            (and ns-id  ; try to use namespace-id as a prefix
                 (not (assq (string->symbol ns-id) ns-prefix-assig))
                 (not (assoc ns-id declared-ns-prefixes))   ;; FIXME: maybe remove
                 )
            ns-id
            ; Otherwise - generate unique prefix
            ; Returns a prefix-string not presented in ns-prefix-assig and
            ; declared-ns-prefixes
            (let loop ((i 1))
              (let ((candidate (string-append "prfx" (number->string i))))
                (if (or (assoc candidate declared-ns-prefixes)
                        (assq (string->symbol candidate) ns-prefix-assig))
                    (loop (+ i 1))
                    candidate))))))
        (n-parts (srl:split-name name)))
    (cond
      ((and attribute?
            (not (car n-parts)))         ; no namespace-id => no namespace
       (values #f #f (cdr n-parts)       ; name as a string
               #f))
      ((and (car n-parts)
            (string-ci=? (car n-parts) "xml")) ; reserved XML namespace
       (values (car n-parts) "http://www.w3.org/XML/1998/namespace"
               (cdr n-parts) #f))
      (else
       (call-with-values
        (lambda ()
          (let ((nid (or (car n-parts) "*default*")))
            (cond
             ((assq (string->symbol nid) ; suppose a namespace-id
                    namespace-assoc)
              => (lambda (lst)
                   (values (cadr lst) nid)))
             (else           ; first part of a name is a namespace URI
              (values (or (car n-parts) "") #f)))))
        (lambda (namespace-uri ns-id)
          (cond
            ((srl:assoc-cdr-string= namespace-uri declared-ns-prefixes)
             => (lambda (pair)
                  ; Prefix for that namespace URI already declared
                  (values (car pair) namespace-uri (cdr n-parts) #f)))
            (else  ; namespace undeclared
             (values
              (cond
                ((srl:assoc-cdr-string= namespace-uri ns-prefix-assig)
                 => (lambda (pair)
                      ; A candidate namespace prefix is supplied from the user
                      (let ((candidate (symbol->string (car pair))))
                        (if (and (or attribute?
                                     (not (allow-prefix-redeclarations?)))
                                 (assoc candidate declared-ns-prefixes))
                            ;; The prefix already bound to a different namespace
                            ;; Avoid XML prefix re-declaration
                            (use-ns-id-or-generate-prefix ns-id)
                            candidate))))
                (else
                 (if (and ns-id (allow-prefix-redeclarations?))
                     ns-id
                     (use-ns-id-or-generate-prefix ns-id))))
              namespace-uri
              (cdr n-parts)
              #t  ; in any case, prefix declaration is required
              )))))))))

;; Changes: (car node) is only treated as a tag when it is a symbol.
;; For indentation purposes we also treat a list starting with a
;; text node as a text node (to avoid ("foo") having extraneous
;; whitespace added around it).
(define (srl:display-node-out-recursive
         node port method
         ns-prefix-assig namespace-assoc declared-ns-prefixes
         indentation space-preserve?
         cdata-section-elements text-node-handler)
  (cond
   ((not (pair? node))                  ; text node
    (display (text-node-handler (srl:atomic->string node)) port))
   ((not (symbol? (car node)))          ;; ** change
    (for-each
     (lambda (kid)
       (srl:display-node-out-recursive
        kid port method
        ns-prefix-assig namespace-assoc declared-ns-prefixes
        indentation space-preserve?
        cdata-section-elements text-node-handler))
     node))
   (else
    (case (car node)  ; node name
      ((*COMMENT*)
       (for-each
        (lambda (x) (display x port))
        (srl:comment->str-lst node)))
      ((*PI*)
       (for-each
        (lambda (x) (display x port))
        (srl:processing-instruction->str-lst node method)))
      ((&)
       (display (srl:shtml-entity->char-data node) port))
      ((*DECL*)  ; recovering for non-SXML nodes
       #f)
      (else  ; otherwise - an element node
       (call-with-values
        (lambda ()
          (srl:construct-start-end-tags
           node method
           ns-prefix-assig namespace-assoc declared-ns-prefixes))
        (lambda (start-tag end-tag
                           ns-prefix-assig namespace-assoc declared-ns-prefixes)
          (begin
            (srl:display-fragments-2nesting start-tag port)
            (if
             end-tag  ; there exists content
             (let ((space-preserve?
                    (srl:update-space-specifier node space-preserve?))
                   (text-node-handler
                    (cond
                      ((memq (car node) cdata-section-elements)
                       srl:string->cdata-section)
                      ((and (eq? method 'html)
                            (srl:member-ci (symbol->string (car node))
                                           '("script" "style")))
                       ; No escaping for strings inside these HTML elements
                       (lambda (str) str))
                      (else
                       srl:string->char-data)))
                   (content ((srl:select-kids
                              (lambda (node)  ; TODO: support SXML entities
                                (not (and (pair? node)
                                          (memq (car node) '(@ @@ *ENTITY*))))))
                             node)))
               (call-with-values
                (lambda ()
                  (cond
                    ((or (not indentation)
                         (and (eq? method 'html)
                              (srl:member-ci
                               (symbol->string (car node))
                               '("pre" "script" "style" "textarea"))))
                     ; No indent - on this level and subsequent levels
                     (values #f #f))
                    ((or space-preserve?
                         (srl:mem-pred  ; at least a single text node
                          (lambda (node)
                            (or (not (pair? node))
                                (not (symbol? (car node)))))   ;; ** change
                          content))
                     ; No indent on this level, possible indent on nested levels
                     (values #f indentation))
                    (else
                     (values (cons srl:newline indentation)
                             (cons (car indentation) indentation)))))
                (lambda (indent-here indent4recursive)
                  (begin
                    (for-each  ; display content
                     (if
                      indent-here
                      (lambda (kid)
                        (begin
                          (for-each
                           (lambda (x) (display x port))
                           indent-here)
                          (srl:display-node-out-recursive
                           kid port method
                           ns-prefix-assig namespace-assoc declared-ns-prefixes
                           indent4recursive space-preserve?
                           cdata-section-elements text-node-handler)))
                      (lambda (kid)
                        (srl:display-node-out-recursive
                         kid port method
                         ns-prefix-assig namespace-assoc declared-ns-prefixes
                         indent4recursive space-preserve?
                         cdata-section-elements text-node-handler)))
                     content)
                    (if indent-here
                        (begin
                          (display srl:newline port)
                          (for-each
                           (lambda (x) (display x port))
                           (cdr indentation))))
                    (for-each
                     (lambda (x) (display x port))
                     end-tag))))))))))))))

;; Changes: (car node) is only treated as a tag when it is a symbol.
;; For indentation purposes we also treat a list starting with a
;; text node as a text node (to avoid ("foo") having extraneous
;; whitespace added around it).
;; This is almost a cut and paste of srl:display-node-out-recursive.
(define (srl:node->nested-str-lst-recursive
         node method
         ns-prefix-assig namespace-assoc declared-ns-prefixes
         indentation space-preserve?
         cdata-section-elements text-node-handler)
  (cond
   ((not (pair? node))                  ; text node
    (text-node-handler (srl:atomic->string node)))
   ((not (symbol? (car node)))          ;; ** change
    (map
     (lambda (kid)
       (srl:node->nested-str-lst-recursive
        kid method
        ns-prefix-assig namespace-assoc declared-ns-prefixes
        indentation space-preserve?
        cdata-section-elements text-node-handler))
     node))
   (else
    (case (car node)  ; node name
      ((*COMMENT*)
       (srl:comment->str-lst node))
      ((*PI*)
       (srl:processing-instruction->str-lst node method))
      ((&)
       (srl:shtml-entity->char-data node))
      ((*DECL*)  ; recovering for non-SXML nodes
       '())
      (else  ; otherwise - an element node
       (call-with-values
        (lambda ()
          (srl:construct-start-end-tags
           node method
           ns-prefix-assig namespace-assoc declared-ns-prefixes))
        (lambda (start-tag end-tag
                           ns-prefix-assig namespace-assoc declared-ns-prefixes)
          (if
           (not end-tag)  ; empty element => recursion stops
           start-tag
           (let ((space-preserve?
                  (srl:update-space-specifier node space-preserve?))
                 (text-node-handler
                  (cond
                    ((memq (car node) cdata-section-elements)
                     srl:string->cdata-section)
                    ((and (eq? method 'html)
                          (srl:member-ci (symbol->string (car node))
                                         '("script" "style")))
                     ; No escaping for strings inside these HTML elements
                     (lambda (str) str))
                    (else
                     srl:string->char-data)))
                 (content ((srl:select-kids
                            (lambda (node)  ; TODO: support SXML entities
                              (not (and (pair? node)
                                        (memq (car node) '(@ @@ *ENTITY*))))))
                           node)))
             (call-with-values
              (lambda ()
                (cond
                  ((or (not indentation)
                       (and (eq? method 'html)
                            (srl:member-ci
                             (symbol->string (car node))
                             '("pre" "script" "style" "textarea"))))
                   ; No indent - on this level and subsequent levels
                   (values #f #f))
                  ((or space-preserve?
                       (srl:mem-pred  ; at least a single text node
                        (lambda (node)
                          (or (not (pair? node))
                              (not (symbol? (car node)))))  ;; ** change
                        content))
                   ; No indent on this level, possible indent on nested levels
                   (values #f indentation))
                  (else
                   (values (cons srl:newline indentation)
                           (cons (car indentation) indentation)))))
              (lambda (indent-here indent4recursive)
                (if
                 indent-here
                 (append
                  start-tag
                  (map
                   (lambda (kid)
                     (list
                      indent-here
                      (srl:node->nested-str-lst-recursive
                       kid method
                       ns-prefix-assig namespace-assoc declared-ns-prefixes
                       indent4recursive space-preserve?
                       cdata-section-elements text-node-handler)))
                   content)
                  (cons srl:newline
                        (cons (cdr indentation) end-tag)))
                 (append
                  start-tag
                  (map
                   (lambda (kid)
                     (srl:node->nested-str-lst-recursive
                      kid method
                      ns-prefix-assig namespace-assoc declared-ns-prefixes
                      indent4recursive space-preserve?
                      cdata-section-elements text-node-handler))
                   content)
                  end-tag)))))))))))))

;; Changes: Declare the empty ("") namespace URI upfront so we do
;;          not get a spurious xmlns="" on the first unprefixed elt.
;; WARNING: *default* must be added to ns-prefixes, but this is done
;;          only in serialize-sxml and theoretically there are other
;;          entry points to this procedure (though not in practice,
;;          as the module hides them).
;;   Therefore, srl:top->nested-str-lst and srl:display-top-out
;;   should probably take a declared-ns-prefixes argument so that
;;   *default* is only automatically declared when it's
;;   already in ns-prefixes.
(define (srl:top->nested-str-lst doc
                                 cdata-section-elements indent
                                 method ns-prefix-assig
                                 omit-xml-declaration? standalone version)
  (let* ((namespace-assoc (srl:ns-assoc-for-top doc))
         (declared-ns-prefixes '(("*default*" . "")))        ;; [+]
         (ns-prefix-assig
          (append
           (srl:extract-original-prefix-binding namespace-assoc)
           ns-prefix-assig))
         (serialized-content
          (map
           (if
            indent  ; => output each member from the newline
            (let ((indentation (list indent)))  ; for nested elements
              (lambda (kid)
                (list
                 srl:newline
                 (srl:node->nested-str-lst-recursive
                  kid method
                  ns-prefix-assig namespace-assoc declared-ns-prefixes
                  indentation #f
                  cdata-section-elements srl:string->char-data))))
            (lambda (kid)
              (srl:node->nested-str-lst-recursive
               kid method
               ns-prefix-assig namespace-assoc declared-ns-prefixes
               indent #f
               cdata-section-elements srl:string->char-data)))
           ((srl:select-kids  ; document node content
             (lambda (node)  ; TODO: support SXML entities
               (not (and
                     (pair? node) (memq (car node) '(@ @@ *ENTITY*))))))
            doc))))
    (if (or (eq? method 'html) omit-xml-declaration?)
        (if (and indent (not (null? serialized-content)))
            ; Remove the starting newline
            ; ATTENTION: beware of `Gambit cadar bug':
            ; http://mailman.iro.umontreal.ca/pipermail/gambit-list/
            ;   2005-July/000315.html
            (cons (cadar serialized-content) (cdr serialized-content))
            serialized-content)
        (list (srl:make-xml-decl version standalone) serialized-content))))

(define (srl:display-top-out doc port
                             cdata-section-elements indent
                             method ns-prefix-assig
                             omit-xml-declaration? standalone version)  
  (let ((no-xml-decl?  ; no XML declaration was displayed?
         (if (not (or (eq? method 'html) omit-xml-declaration?))
             (begin
               (for-each  ; display xml declaration
                (lambda (x) (display x port))
                (srl:make-xml-decl version standalone))
               #f)
             #t))
        (content  ; document node content
         ((srl:select-kids
           (lambda (node)  ; TODO: support SXML entities
             (not (and
                   (pair? node) (memq (car node) '(@ @@ *ENTITY*))))))
          doc))
        (namespace-assoc (srl:ns-assoc-for-top doc))
        (declared-ns-prefixes '(("*default*" . ""))))      ; [+]
    (let ((ns-prefix-assig
           (append
            (srl:extract-original-prefix-binding namespace-assoc)
            ns-prefix-assig)))
      (cond
        ((null? content)  ; generally a rare practical situation
         #t)  ; nothing more to do
        ((and indent no-xml-decl?)
         ; We'll not display newline before (car content)
         (let ((indentation (list indent)))  ; for nested elements
           (for-each
            (lambda (kid put-newline?)
              (begin
                (if put-newline?
                    (display srl:newline port))
                (srl:display-node-out-recursive
                 kid port method
                 ns-prefix-assig namespace-assoc declared-ns-prefixes
                 indentation #f
                 cdata-section-elements srl:string->char-data)))
            content
            ; After sequence normalization, content does not contain #f
            (cons #f (cdr content)))))
        (else
         (for-each
          (if
           indent  ; => output each member from the newline
           (let ((indentation (list indent)))  ; for nested elements
             (lambda (kid)
               (begin
                 (display srl:newline port)
                 (srl:display-node-out-recursive
                  kid port method
                  ns-prefix-assig namespace-assoc declared-ns-prefixes
                  indentation #f
                  cdata-section-elements srl:string->char-data))))
           (lambda (kid)
             (srl:display-node-out-recursive
              kid port method
              ns-prefix-assig namespace-assoc declared-ns-prefixes
              indent #f
              cdata-section-elements srl:string->char-data)))
          content))))))

;; Changes: accept nulls, chars, and symbols as SXML nodes.  For some
;; reason numbers and bools were already accepted, which makes me think
;; this was an oversight.
(define (srl:atomic->string obj)
  (cond
    ((or (pair? obj)  ; non-atomic type
         (string? obj)) obj)
    ((null? obj)   "")
    ((char? obj)   (string obj))
    ((symbol? obj) (symbol->string obj))
    ((number? obj) (number->string obj))
    ((boolean? obj)
     (if obj "true" "false"))
    (else  ; unexpected type
     ; ATTENTION: should probably raise an error here
     obj)))


))



