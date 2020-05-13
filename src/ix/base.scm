(module ix.base (init prototype ix? primitive? sexp? list? product? enum? identifier?
                 keyword? symbol? oid? string? integer? natural? scientific? boolean?
                 well-typed? tag->ident ident->tag ident=? wrap unwrap unwrap!)

(import (except scheme list? string? symbol? integer? number? boolean?))
(import (prefix scheme scheme:))
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import (prefix chicken.keyword chicken:))

(import tabulae)
(import tabulae.monad)
(import uuid)

; note there's no way to do unchecked builds, may add if I need it
(define (prototype _) (error "must init ix:prototype with a function of type `tag -> maybe prototype`"))
(define (init f) (set! prototype f))

(define (is-type t v)
  (and (scheme:list? v)
       (not (null? v))
       (eqv? (car v) t)))

(define (sexp? v) (and (is-type 'sexp v) (not (null? (cdr v))) (is-type 'identifier (cadr v))))
(define (list? v) (is-type 'list v))
(define (product? v) (is-type 'product v))
(define (enum? v) (is-type 'enum v))
(define (identifier? v) (is-type 'identifier v))
(define (keyword? v) (is-type 'keyword v))
(define (symbol? v) (is-type 'symbol v))
(define (oid? v) (is-type 'oid v))
(define (string? v) (is-type 'string v))
(define (integer? v) (is-type 'integer v))
(define (natural? v) (is-type 'natural v))
(define (scientific? v) (is-type 'scientific v))
(define (boolean? v) (is-type 'boolean v))

(define (ix? v)
  (or (sexp? v) (list? v) (product? v) (enum? v) (identifier? v) (keyword? v) (symbol? v)
      (oid? v) (string? v) (integer? v) (natural? v) (scientific? v) (boolean? v)))

(define (primitive? v)
  (or (identifier? v) (keyword? v) (symbol? v) (oid? v) (string? v) (integer? v) (natural? v) (scientific? v) (boolean? v)))

(define (wrap t v)
  (if (memv t `(sexp list product identifier))
      `(,t ,@v)
      `(,t ,v)))

; XXX this definition of unwrap makes no sense for sexp
; it should strip the identifier so it can at least be used with chicken keyword functions
; that said unwrapping a sexp is of dubious value anyway
(define (unwrap v)
  (cond ((or (sexp? v) (list? v) (product? v) (identifier? v))
         (<maybe>-return (cdr v)))
        ((ix? v)
         (<maybe>-return (cadr v)))
        (else (<maybe>-fail))))

(define (unwrap! v) (from-just (unwrap v)))

(define (tag->ident t)
  `(identifier ,@(map string->symbol (string-split (symbol->string t) ":"))))

(define (ident->tag i)
  (string->symbol (string-intersperse (map symbol->string (cdr i)) ":")))

(define (ident=? tag sx)
  (and (sexp? sx) (eq? (ident->tag (cadr sx)) tag)))

(define untroublesome-symbols (string->list "!$%&*+-./;<=>?@^_~"))
(define (untroublesome? c) (memq c untroublesome-symbols))
(define (valid-symbol? s) (let ((cs (string->list (symbol->string s))))
                               (and (not (eq? #\: (car cs)))
                                    (all* (lambda (c) (or (char-alphabetic? c)
                                                          (char-numeric? c)
                                                          (untroublesome? c)
                                                          (eq? #\: c)))
                                          cs))))

(define (well-typed? tv)
  (define v (and (ix? tv) (unwrap! tv)))
  (cond ((identifier? tv) (and (not (null? v)) (all* scheme:symbol? v)))
        ((keyword? tv) (chicken:keyword? v))
        ((symbol? tv) (and (scheme:symbol? v) (valid-symbol? v)))
        ((oid? tv) (uuid? v))
        ((string? tv) (scheme:string? v))
        ((integer? tv) (exact-integer? v))
        ((natural? tv) (and (exact-integer? v) (>= v 0)))
        ((scientific? tv) (scheme:number? v))
        ((boolean? tv) (scheme:boolean? v))
        (else #f)))

)
