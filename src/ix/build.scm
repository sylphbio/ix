(module ix.build (validate validate-as build build!)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.keyword)

(import tabulae)
(import tabulae.monad)

(import (prefix ix.base ix:))
(import ix.lens)

; validate that a given ix sexp matches its declared prototype
; XXX TODO fn like `checked-parse tag text` might be nice to reduce boilerplate
; XXX TODO great idea, I should have a function that turns a prototype into a markdown-formatted string
; so that prototypes can be trivially inserted into documentation without having to worry about keeping it up to date
; XXX TODO FIXME aside from generics, my number one priority is error messages for build/validate
; the lib is unusable otherwise. ix itself works fine but there's no way to tell what's wrong with an input
(define validate (letrec 
  ; simple helper to locate the right sum type
  ; since find doesn't return its result and fold doesn't short-circuit
  ((sum-find (lambda (proto obj)
    (and (not (null? proto))
         (let ((obj^ (validate-value (car proto) obj)))
              (or obj^ (sum-find (cdr proto) obj))))))
   ; compare a given prototype marker to a matching field of an object
   ; this returns the object field, possibly with a type conversion, or #f
   (validate-value (lambda (proto obj)
     (define proto-tag (cond ((keyword? proto) 'keyword)
                             ((symbol? proto)  proto)
                             ((list? proto)    (car proto))
                             (else             (error "bad prototype value" proto))))
     (case proto-tag
       ; straightforward. if an optional value is present just check it normally
       ; optional is not an ix type, rather just a syntactic marker solely for prototypes
       ; XXX I should make it so optional can only occur once, as the outermost type
       ((optional)   (validate-value (cadr proto) obj))
       ; validate against each type on the sum, taking the first that succeeds
       ; XXX it would be nice if I could ban "overlapping" sums
       ; ie, sum cannot contain both symbol and enum, or oid and string, or multiple number types
       ((sum)        (sum-find (cdr proto) obj))
       ; validate each type of the product in turn
       ((product)    (and (ix:product? obj)
                          (= (length (ix:unwrap! obj)) (length (cdr proto)))
                          (let ((ret (map (lambda (t/o) (validate-value (car t/o) (cdr t/o)))
                                          (zip* (cdr proto) (ix:unwrap! obj)))))
                               (and (all* identity ret) (ix:wrap 'product ret)))))
       ; check if sexp, tags match, and the object itself validates
       ((sexp)       (and (ix:sexp? obj)
                          (eq? (ix:ident->tag ((^.! ident) obj))
                               (cadr proto))
                          (let ((ret (validate obj)))
                               (and (just? ret) (from-just ret)))))
       ; check if list and that all items validate against the list item type
       ((list)       (and (ix:list? obj)
                          (let ((ret (map ((curry* validate-value) (cadr proto)) (ix:unwrap! obj))))
                               (and (all* identity ret) (ix:wrap 'list ret)))))
       ; check if identifier and compare by converting to tag, ie join to symbol with colons
       ((identifier) (and (ix:identifier? obj)
                          (eq? proto (ix:ident->tag obj))
                          obj))
       ; straightforward
       ((keyword)    (and (ix:keyword? obj)
                          (eq? proto (ix:unwrap! obj))
                          obj))
       ; if object is tagged enum, compare it to the list of allowed values from the prototype
       ; if it's a symbol, convert and recurse.we don't recheck in the cond arm for (hopeful) correctness sake
       ; as with number conversions below, all valid results ultimately come from the same brach
       ((enum)       (cond ((and (ix:enum? obj) (memq (ix:unwrap! obj) (cdr proto)))
                            obj)
                           ((ix:symbol? obj)
                            (validate-value proto (ix:wrap 'enum (ix:unwrap! obj))))
                           (else #f)))
       ; check well-typedness if integer, otherwise if natural convert and recurse
       ; we only ever move from more specific to more general types and never touch the values
       ((integer)    (cond ((and (ix:integer? obj) (ix:well-typed? obj))
                            obj)
                           ((ix:natural? obj)
                            (validate-value proto (ix:wrap 'integer (ix:unwrap! obj))))
                           (else #f)))
       ; as above. note this means a sci may contain a scheme fixnum, bignum, or flonum
       ; I haven't decided yet if this is a problem but it is unwise to throw away precision
       ((scientific) (cond ((and (ix:scientific? obj) (ix:well-typed? obj))
                            obj)
                           ((or (ix:natural? obj) (ix:integer? obj))
                            (validate-value proto (ix:wrap 'scientific (ix:unwrap! obj))))
                           (else #f)))
       ; straightforward
       ((symbol
         oid
         string
         natural
         boolean)    (and (ix:ix? obj)
                          (eq? proto-tag (car obj))
                          (ix:well-typed? obj)
                          obj))
       ((else        (error "unimplemented type" proto-tag))))))
   ; filter out unused optional kvs from the prototype
   ; specifically the three properties we desire are:
   ; * every obj key corresponds to a proto key
   ; * every required proto key corresponds to an obj key
   ; * obj and proto have the same key ordering
   ; validate-value covers one and three, and to get two we check the lengths before zip
   (filter-proto (lambda (proto obj acc)
     (cond ((null? proto) (reverse* acc))
           ((just? ((^. (keyw (first* proto))) obj))
            (filter-proto (drop* 2 proto) obj (cons (second* proto) (cons (first* proto) acc))))
           ((and (list? (second* proto)) (eq? (car (second* proto)) 'optional))
            (filter-proto (drop* 2 proto) obj acc))
           (else #f))))
   ; load prototype by tag, filter optionals, validate all keys and values, rebuild the object if all checks succeed
   (validate (lambda (sx) (do/m <maybe>
     (i <- ((^. ident) sx))
     (declare sx-kvs (cddr sx))
     (proto <- (ix:prototype (ix:ident->tag i)))
     (filtered-proto <- (to-maybe (filter-proto (cdr proto) sx-kvs '())))
     (to-maybe (= (length filtered-proto) (length sx-kvs)))
     (obj-body <- (let ((ret (map (lambda (p) (validate-value (car p) (cdr p)))
                                  (zip* filtered-proto sx-kvs))))
                       (if (all* identity ret) (return ret) (fail))))
     (return `(sexp ,i ,@obj-body))))))
    validate))

; pretty self-explanatory
(define (validate-as tag sx)
  (if (and (ix:sexp? sx)
           (eq? (ix:ident->tag ((^.! ident) sx)) tag))
      (validate sx)
      (<maybe>-fail)))

; this wraps a given value with a given type
; split out from its parent below because it needs to recurse
(define (wrap-build-value type value)
  (define type-tag (if (symbol? type) type (car type)))
  (define raw-value (if (ix:ix? value) (ix:unwrap! value) value))
  (case type-tag
    ; all these we can just add the tag to the raw value
    ((sexp enum symbol oid string integer natural scientific boolean) (ix:wrap type-tag raw-value))
    ; add the tag and wrap all list values
    ((list) (ix:wrap type-tag (map ((curry* wrap-build-value) (cadr type))
                                   raw-value)))
    ; as above but zip the product subtypes
    ((product) (ix:wrap type-tag (map (lambda (t/v) (wrap-build-value (car t/v) (cdr t/v)))
                                      (zip* (cdr type) raw-value))))
    ; jfc sum types are always the worst to handle
    ((sum) (cond ((or (ix:list? value) (ix:product? value))
                  (let ((full (find* (lambda (t) (and (list? t) (eq? (car t) (car value)))) (cdr type))))
                       (wrap-build-value full value)))
                 ((ix:ix? value) value)
                 (else (error "sum type inference is not supported, tag your values"))))
    ; this is explicitly for kv pairs
    ((keyword identifier) (error "keywords and identifiers should not be here"))
    (else (error "type not implemented" type value))))

; this extracts a type and value for a given keyword and handles optionalness
(define (wrap-build-kv-pair types kvs kw)
  (define is-optional (and (list? (get-keyword kw types))
                           (eq? (car (get-keyword kw types)) 'optional)))
  (define type (if is-optional
                   (cadr (get-keyword kw types))
                   (get-keyword kw types)))
  ; because you can pass in a bare #f for an ix boolean
  (define value (get-keyword kw kvs (lambda () '無)))
  (to-maybe (cond ((and type (not (eq? value '無))) `(,(ix:wrap 'keyword kw) ,(wrap-build-value type value)))
                  ((and type is-optional) '())
                  (else #f))))

; takes an object tag and keyword arguments for all its fields
; prototype lookup, merge prototype types with input values, validate the result
(define (build tag . kvs)
  (do/m <maybe>
    (proto <- (ix:prototype tag))
    (to-maybe (all* (lambda (k) (get-keyword k (cdr proto))) (filter* keyword? kvs)))
    (kvs^ <- (sequence (map (lambda (k) (wrap-build-kv-pair (cdr proto) kvs k))
                            (filter* keyword? proto))))
    (declare sx `(sexp ,(ix:tag->ident tag) ,@(join kvs^)))
    (validate sx)))

(define (build! . args) (from-just (apply build args)))

)
