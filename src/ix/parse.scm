(module ix.parse (ix flat-ix json->ix)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.format)
(import chicken.string)
(import chicken.keyword)
(import chicken.port)

(import tabulae)
(import tabulae.parsec)
(import tabulae.monad)
(import json)

(import (prefix ix.base ix:))
(import (prefix ix.build ix:))
(import ix.static)

(define uuid-p
  (do/m <parser>
    (s1 <- (precisely 8 hex))
    (value #\-)
    (s2 <- (precisely 4 hex))
    (value #\-)
    (value #\4)
    (s3 <- (precisely 3 hex))
    (value #\-)
    (c4 <- hex)
    (if (memv c4 '(#\8 #\9 #\a #\b)) (return '()) <parser>-mzero)
    (s5 <- (precisely 3 hex))
    (value #\-)
    (s6 <- (precisely 12 hex))
    (return (<> s1 "-" s2 "-4" s3 "-" (string c4) s5 "-" s6))))

; the (return (void)) shit is because the letrec and monad macros kinda fuck each other up
; XXX I should allow comments
; XXX I might want to make my parser an either rather than maybe? that would be a huge refactor tho
; before I do anything like that I should read the paper about making parsec efficient lol
; XXX note I changed parser to be sexp or bare list, but by definition only a sexp can validate
; XXX TODO FIXME I don't support escapes or double quotes in strings lol
; this plus the comment thing makes me think I should write a char-by-char parser
; XXX TODO FIXME consume leading whitespace
; XXX consider whether to restrict toplevel to "sexp or list of sexp" rather than "sexp or list"
; XXX should probably call validate at the end so types get fixed up
; XXX can I have like a parse-with to allow prototypes to be sent over the wire?
; XXX this is not long for the world now that I'm dumping the monad interface in favor of normal error messages
(define (ix-parser #!key flat) (letrec
  ((untroublesome-p (sat (lambda (c) (memv c untroublesome))))
   (keyval (do/m <parser>
     (return (void))
     (k <- ix-keyword)
     (many1 whitespace)
     (v <- term)
     (return `(,k ,v))))
   (term (do/m <parser>
     (return (void))
     (t <- (apply <?> `(,@(if flat '() `(,ix-sexp ,ix-list ,ix-product))
                        ,ix-uuid ,ix-string ,ix-int/nat/sci ,ix-bool ,ix-symbol)))
     (many whitespace)
     (return t)))
   (ix-uuid (do/m <parser>
     (value #\")
     (u <- uuid-p)
     (value #\")
     (return `(uuid ,u))))
   (ix-string (do/m <parser>
     (value #\")
     (s <- (many (sat (lambda (c) (not (eqv? c #\"))))))
     (value #\")
     (return `(string ,s))))
   (ix-int/nat/sci (do/m <parser>
     (sign <- (perhaps (<?> (value #\+) (value #\-))))
     (mantissa-int <- (many1 num))
     (mantissa-frac <- (perhaps (do/m <parser> (value #\.)
                                               (n <- (many1 num))
                                               (return (<> "." n)))))
     (exponent <- (perhaps (do/m <parser> (value #\e)
                                          (sign <- (perhaps (<?> (value #\+) (value #\-))))
                                          (n <- (many1 num))
                                          (return (<> "e" sign n)))))
     (return `(,(cond ((and (null*? sign) (null*? mantissa-frac) (null*? exponent)) 'natural)
                      ((and (null*? mantissa-frac) (null*? exponent)) 'integer)
                      (else 'scientific))
               ,(string->number (<> sign mantissa-int mantissa-frac exponent))))))
   (ix-bool (do/m <parser>
     (value #\#)
     (b <- (<?> (value #\t) (value #\f)))
     (return `(boolean ,(if (eq? b #\t) #t #f)))))
   ; XXX TODO FIXME this should be more sophisticated!!! alphanum/untroublesome sepby1 colon
   ; I need to impl sepby first tho
   (ix-ident (do/m <parser>
     (return (void))
     (s <- (many1 (<?> alphanum untroublesome-p (value #\:))))
     (return `(identifier ,@(map string->symbol (string-split s ":"))))))
   (ix-symbol (do/m <parser>
     (return (void))
     (s1 <- (<?> alphanum untroublesome-p))
     (sn <- (many (<?> alphanum untroublesome-p (value #\:))))
     (return `(symbol ,(string->symbol (cons* s1 sn))))))
   (ix-keyword (do/m <parser>
     (value #\:)
     (s <- (many1 (<?> alphanum untroublesome-p)))
     (return `(keyword ,(string->keyword s)))))
   (ix-product (do/m <parser>
     lcbracket
     (many whitespace)
     (t <- (many-l term))
     rcbracket
     (return `(product ,@t))))
   (ix-list (do/m <parser>
     lsbracket
     (many whitespace)
     (t <- (many-l term))
     rsbracket
     (return `(list ,@t))))
   (ix-sexp (do/m <parser>
     lparen
     (i <- ix-ident)
     (many1 whitespace)
     (kv <- (many1-l keyval))
     rparen
     (return `(sexp ,i ,@(join kv))))))
  (<?> ix-sexp ix-list)))

(define (ix sx)
  (let ((r (parse->maybe ((ix-parser) sx))))
       (if (just? r) (from-just r) (die "failed to parse: ~S" sx))))

(define (flat-ix sx)
  (let ((r (parse->maybe ((ix-parser :flat #t) sx))))
       (if (just? r) (from-just r) (die "failed to flat parse: ~S" sx))))

; so the way json->ix works is, first to cut corners I parse the json to scheme with a library
; this produces vectors of pairs for objects, lists for arrays, unspecified for null, what you'd expect for rest
; so for anything that will become an ix sexp, we look for our special ##identifier key (created by ix->json)
; for generic objects we have to wrap everything for build, but if we have a real tag build can handle it
; ix->json downcasts numbers and symbols to strings. validate-value in build can upcast them back
; thus generic ix conversions are lossy but typed ix conversions use the prototype to achieve losslessness
(define json->ix (letrec
   ; trivial helper
  ((wrap (lambda (to-wrap tag val) (if to-wrap (ix:wrap tag val) val)))
   ; transforms parsed json values to ix values, wrapped or unwrapped based on tag
   (parse-value (lambda (tw val)
     (cond ((vector? val) (parse-obj val))
           ((list? val) (wrap tw 'list (map ((curry* parse-value) tw) val)))
           ((string? val) (wrap tw 'string val))
           ((and (exact-integer? val) (>= val 0)) (wrap tw 'natural val))
           ((exact-integer? val) (wrap tw 'integer val))
           ((number? val) (wrap tw 'scientific val))
           ((boolean? val) (wrap tw 'boolean val))
           ((eqv? (void) val) (die "null is (emphatically) not supported"))
           (else (die "not a value" val)))))
   ; so we convert to list and check for an identifier, defaulting to generic ix of course
   ; for a generic ix object, we need to wrap all our values (not keywords) before calling build
   ; but for a typed ix object, we leave them unwrapped, since our prototype promotes certain types up from strings
   (parse-obj (lambda (vec)
     (let* ((ident/rest (partition* (lambda (k/v) (equal? (car k/v) "##identifier"))
                                    (vector->list vec)))
            (tag (if (not (null? (first* ident/rest)))
                     (string->symbol (cdar (first* ident/rest)))
                     'ix))
            (build-args (join (map (lambda (k/v) (parse-kv (eqv? tag 'ix) (car k/v) (cdr k/v)))
                                   (second* ident/rest)))))
           (apply ix:build `(,tag ,@build-args)))))
   ; don't bother checking eg that symbols are in the charset, build checks well-typedness
   (parse-kv (lambda (to-wrap key val)
     `(,(string->keyword key) ,(parse-value to-wrap val))))
   ; ecma-404 isn't the boss of me
   ; I see no point in an object format supporting toplevel primitives
   (json->ix (lambda (js)
     (let ((raw (call-with-input-string js json-read)))
          (cond ((vector? raw) (parse-obj raw))
                ((and (list? raw) (all* vector? raw)) (map parse-obj raw))
                (else (die "json->ix accepts an object or a list of objects" js)))))))
  json->ix))

)
