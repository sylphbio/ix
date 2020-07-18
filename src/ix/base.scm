(module ix.base (register! prototype ix? primitive? sexp? list? product? enum? identifier?
                 keyword? symbol? uuid? string? integer? natural? scientific? boolean?
                 well-typed? tag->ident ident->tag ident=? wrap unwrap)

(import (except scheme list? string? symbol? integer? number? boolean?))
(import (prefix scheme scheme:))
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import (prefix chicken.keyword chicken:))

(import tabulae)
(import (prefix uuid uuid:))

(import ix.static)

; alist. only set by register, only accessed by prototype
(define prototypes '())

; takes a list of prototypes, uses tag to make alist
(define (register! plist)
  (when (find* (lambda (p) (eqv? (car p) 'ix)) plist) (die "cannot override the special identifier ix"))
  (when (find* (lambda (p) (eqv? (car p) '*)) plist) (die "* has special meaning for prototypes and is disallowed"))
  (define pts (foldl (lambda (acc p) (alist-update (car p) p acc))
                     prototypes
                     plist))
  (set! prototypes pts))

; this is used by build/validate, but users can look up prototypes for whatever purpose if they like
; XXX returns a maybe but I'm stripping monads out of the interface soon
(define (prototype tag)
  (alist-ref tag prototypes))

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
(define (uuid? v) (is-type 'uuid v))
(define (string? v) (is-type 'string v))
(define (integer? v) (is-type 'integer v))
(define (natural? v) (is-type 'natural v))
(define (scientific? v) (is-type 'scientific v))
(define (boolean? v) (is-type 'boolean v))

; I unroll this and primitive by hand because to or over all the predicates makes call chains useless
(define (ix? v)
  (let ((t (and (scheme:list? v) (not (null? v)) (car v))))
       (if (eqv? t 'sexp)
           (sexp? v)
           (memv t `(list product enum identifier keyword symbol
                     uuid string integer natural scientific boolean)))))

(define (primitive? v)
  (let ((t (and (scheme:list? v) (not (null? v)) (car v))))
       (if (eqv? t 'sexp)
           (sexp? v)
           (memv t `(identifier keyword symbol uuid string integer natural scientific boolean)))))

(define (wrap t v)
  (if (memv t `(sexp list product identifier))
      `(,t ,@v)
      `(,t ,v)))

; XXX should this recursively unwrap lists and products?
; XXX impl that lens suggestion lol
(define (unwrap v)
  (cond ((sexp? v) (die "unwrapping sexp is nonsensical, extract the key/val list via lens"))
        ((or (list? v) (product? v) (identifier? v)) (cdr v))
        ((ix? v) (cadr v))
        (else (die "cannot unwrap non-ix value: ~S" v))))

; XXX change these to "identifier", I don't want a profusion of shorthands
(define (tag->ident t)
  `(identifier ,@(map string->symbol (string-split (symbol->string t) ":"))))

(define (ident->tag i)
  (string->symbol (string-intersperse (map symbol->string (cdr i)) ":")))

(define (ident=? tag sx)
  (and (sexp? sx) (eq? (ident->tag (cadr sx)) tag)))

(define (untroublesome? c) (memq c untroublesome))
(define (valid-symbol? s) (let ((cs (string->list (symbol->string s))))
                               (and (not (eq? #\: (car cs)))
                                    (all* (lambda (c) (or (char-alphabetic? c)
                                                          (char-numeric? c)
                                                          (untroublesome? c)
                                                          (eq? #\: c)))
                                          cs))))

; XXX have sexp actually check that it goes ident/kw/ix/kw/ix/kw/ix. ignore schema
(define (well-typed? tv)
  (if (not (ix? tv))
      #f
      (let ((t (car tv))
            (v (if (sexp? tv) (cdr tv) (unwrap tv))))
           (case t
             ((sexp list product) (all* well-typed? v))
             ((identifier) (and (not (null? v)) (all* scheme:symbol? v)))
             ((keyword) (chicken:keyword? v))
             ((symbol enum) (and (scheme:symbol? v) (valid-symbol? v)))
             ((uuid) (uuid:uuid? v))
             ((string) (scheme:string? v))
             ((integer) (exact-integer? v))
             ((natural) (and (exact-integer? v) (>= v 0)))
             ((scientific) (scheme:number? v))
             ((boolean) (scheme:boolean? v))
             (else #f)))))

)
