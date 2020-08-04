(module ix.build (validate validate-as build)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.keyword)

(import tabulae)
(import tabulae.monad)
(import srfi-34)

(import (prefix ix.base ix:))
(import ix.lens)
(import ix.static)

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
                             (else             (raise (exn '(ix validate) "bad prototype value: ~S" proto)))))
     (case proto-tag
       ; straightforward. if an optional value is present just check it normally
       ; optional is not an ix type, rather just a syntactic marker solely for prototypes
       ; XXX I should make it so optional can only occur once, as the outermost type
       ((optional)   (validate-value (cadr proto) obj))
       ; validate against each type on the sum, taking the first that succeeds
       ; XXX it would be nice if I could ban "overlapping" sums
       ; ie, sum cannot contain both symbol and enum, or uuid and string, or multiple number types
       ; XXX UPDATE nevermind sum type of numbers is it turns out desirable lol
       ((sum)        (sum-find (cdr proto) obj))
       ; validate each type of the product in turn
       ((product)    (and (ix:product? obj)
                          (= (length (ix:unwrap obj)) (length (cdr proto)))
                          (let ((ret (map (lambda (t/o) (validate-value (car t/o) (cdr t/o)))
                                          (zip* (cdr proto) (ix:unwrap obj)))))
                               (and (all* identity ret) (ix:wrap 'product ret)))))
       ; check if sexp, tags match, and the object itself validates
       ; star is a special tag matching any valid sexp
       ((sexp)       (and (ix:sexp? obj)
                          (or (eq? (ix:ident->tag ((^. ident) obj)) (cadr proto))
                              (eq? (cadr proto) '*))
                          (let ((ret (validate obj)))
                               (and (just? ret) (from-just ret)))))
       ; check if list and that all items validate against the list item type
       ((list)       (and (ix:list? obj)
                          (let ((ret (map ((curry* validate-value) (cadr proto)) (ix:unwrap obj))))
                               (and (all* identity ret) (ix:wrap 'list ret)))))
       ; check if identifier and compare by converting to tag, ie join to symbol with colons
       ((identifier) (and (ix:identifier? obj)
                          (eq? proto (ix:ident->tag obj))
                          (ix:well-typed? obj)
                          obj))
       ; straightforward
       ((keyword)    (and (ix:keyword? obj)
                          (eq? proto (ix:unwrap obj))
                          (ix:well-typed? obj)
                          obj))
       ; if object is tagged enum, compare it to the list of allowed values from the prototype
       ; if it's a symbol, convert and recurse.we don't recheck in the cond arm for (hopeful) correctness sake
       ; as with number conversions below, all valid results ultimately come from the same brach
       ((enum)       (cond ((and (ix:enum? obj) (memq (ix:unwrap obj) (cdr proto)))
                            obj)
                           ((ix:symbol? obj)
                            (validate-value proto (ix:wrap 'enum (ix:unwrap obj))))
                           (else #f)))
       ; check well-typedness if integer, otherwise if natural convert and recurse
       ; we only ever move from more specific to more general types and never touch the values
       ((integer)    (cond ((and (ix:integer? obj) (ix:well-typed? obj))
                            obj)
                           ((ix:natural? obj)
                            (validate-value proto (ix:wrap 'integer (ix:unwrap obj))))
                           (else #f)))
       ; as above. note this means a sci may contain a scheme fixnum, bignum, or flonum
       ; I haven't decided yet if this is a problem but it is unwise to throw away precision
       ((scientific) (cond ((and (ix:scientific? obj) (ix:well-typed? obj))
                            obj)
                           ((or (ix:natural? obj) (ix:integer? obj))
                            (validate-value proto (ix:wrap 'scientific (ix:unwrap obj))))
                           (else #f)))
       ; straightforward
       ((symbol
         uuid
         string
         natural
         boolean)    (and (ix:ix? obj)
                          (eq? proto-tag (car obj))
                          (ix:well-typed? obj)
                          obj))
       ((else        (raise (exn '(ix validate) "unimplemented type" proto-tag)))))))
   ; filter out unused optional kvs from the prototype
   ; specifically the three properties we desire are:
   ; * every obj key corresponds to a proto key
   ; * every required proto key corresponds to an obj key
   ; * obj and proto have the same key ordering
   ; validate-value covers one and three, and to get two we check the lengths before zip
   (filter-proto (lambda (proto obj acc)
     (cond ((null? proto) (reverse* acc))
           (((^.? (keyw (first* proto))) obj)
            (filter-proto (drop* 2 proto) obj (cons (second* proto) (cons (first* proto) acc))))
           ((and (list? (second* proto)) (eq? (car (second* proto)) 'optional))
            (filter-proto (drop* 2 proto) obj acc))
           (else #f))))
   ; special validate for generic ix that doesn't depend on a prototype
   ; XXX TODO FIXME this is wrong in a potentially very dangerous way because it short-circuits recursive transformations
   ; what it should actually do is... three-way branch:
   ; * primitive, check well-typed
   ; * sexp, validate (which further branches in ident to generic/typed)
   ; * list/product, map this hypothetical three-way branch over all members
   ; the other approach is to reuse all the typed machinery and have a function that generates fake prototypes for generic ix
   ; this introduces its own problems tho, namely how to resolve elided sum and list types
   ; for now I will do the simple thing and just, not nest ambiguous typed sexps in untyped sexps
   ; but moreover we are way fucking past the point where we need to formalize ix, the semantics are becoming very complicated
   ; XXX generic should as a rule not contain enums
   (validate-generic (lambda (sx) (do/m <maybe>
     (sequence (map (lambda (k/v) (to-maybe (and (ix:keyword? (first* k/v)) (ix:well-typed? (second* k/v)))))
                    (chop (drop* 2 sx) 2)))
     (return sx))))
   ; load prototype by tag, filter optionals, validate all keys and values, rebuild the object if all checks succeed
   (validate-typed (lambda (sx i) (do/m <maybe>
     (declare sx-kvs (cddr sx))
     (proto <- (to-maybe (ix:prototype (ix:ident->tag i))))
     (filtered-proto <- (to-maybe (filter-proto (cdr proto) sx-kvs '())))
     (to-maybe (= (length filtered-proto) (length sx-kvs)))
     (obj-body <- (let ((ret (map (lambda (p) (validate-value (car p) (cdr p)))
                                  (zip* filtered-proto sx-kvs))))
                       (if (all* identity ret) (return ret) (fail))))
     (return `(sexp ,i ,@obj-body)))))
    (validate (lambda (sx) (do/m <maybe>
     (i <- (to-maybe (and (ix:sexp? sx) ((^. ident) sx))))
     (if (eq? (ix:ident->tag i) 'ix)
         (validate-generic sx)
         (validate-typed sx i))))))
    ; XXX refactor obv, we want to error inside and report what actually went wrong lol
    (lambda (sx) (let* ((r (validate sx))
                        (tag (ix:ident->tag ((^. ident) sx))))
                       (if (just? r)
                           (from-just r)
                           (raise (exn `((ix) (validate tag ,tag)) "validation failed")))))))

; pretty self-explanatory
; XXX TODO I'd like to be able to validate anything as any ix type
; but I'd need to split validate-value out and it's too enmeshed in all the other recursive calls
; overall I've been very undisciplined tho and honestly want to rewrite all the build/validate machinery from scratch
; XXX also wanted to have a thing to indicate I want certain optional fields to be there
; XXX TODO FIXME I should just write an inference function, I can use that once I split out lex and parse too
(define (validate-as tag sx)
  (let ((sxtag (and (ix:sexp? sx) (ix:ident->tag ((^. ident) sx)))))
      (if (eqv? sxtag tag) (validate sx) (raise (exn `((ix) (validate tag ,sxtag)) "tag mismatch: expected ~S" tag)))))

; this wraps a given value with a given type
; split out from its parent below because it needs to recurse
; we do string conversions here rather than in vv to avoid ever constructing invalid wrapped items
(define (wrap-build-value type value)
  (define type-tag (if (symbol? type) type (car type)))
  (define raw-value (if (and (ix:ix? value) (not (ix:sexp? value))) (ix:unwrap value) value))
  (case type-tag
    ; sexp cannot be unwrapped
    ((sexp) raw-value)
    ; all these we can just add the tag to the raw value
    ((uuid string boolean) (ix:wrap type-tag raw-value))
    ; these may upcast from string
    ((enum symbol) (ix:wrap type-tag (if (string? raw-value) (string->symbol raw-value) raw-value)))
    ((integer natural scientific) (ix:wrap type-tag (if (string? raw-value) (string->number raw-value) raw-value)))
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
                 (else (raise (exn '(ix build) "sum type inference not supported yet, please tag your values: ~S" value)))))
    ; this is explicitly for kv pairs
    ((keyword identifier) (raise (exn '(ix build) "keywords and identifiers should not be here")))
    (else (raise (exn '(ix build) "type ~S not implemented" type)))))

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

; make sure we have pairs of keys and well-typed values, then just assemble it
; values must be wrapped!! I am not writing type inference, if needed just parse from string
(define (build-free kvs)
  (do/m <maybe>
    (k/vs <- (sequence (map (lambda (k/v)
      (let ((k (car  k/v))
            (v (cadr k/v)))
           (if (and (keyword? k) (ix:well-typed? v))
               (return `(,(ix:wrap 'keyword k) ,v))
               (fail))))
      (chop kvs 2))))
    (declare sx `(sexp (identifier ix) ,@(join k/vs)))
    (to-maybe (validate sx))))

; takes an object tag and keyword arguments for all its fields
; prototype lookup, merge prototype types with input values, validate the result
(define (build-typed tag kvs)
  (do/m <maybe>
    (proto <- (to-maybe (ix:prototype tag)))
    (to-maybe (all* (lambda (k) (get-keyword k (cdr proto))) (filter* keyword? kvs)))
    (kvs^ <- (sequence (map (lambda (k) (wrap-build-kv-pair (cdr proto) kvs k))
                            (filter* keyword? proto))))
    (declare sx `(sexp ,(ix:tag->ident tag) ,@(join kvs^)))
    (to-maybe (validate sx))))

; ix is the standard identifier for arbitrary ix
(define (build tag . kvs)
  (let ((r (if (and (eqv? tag 'ix)
                    (even? (length kvs)))
               (build-free kvs)
               (build-typed tag kvs))))
       (if (just? r)
           (from-just r)
           (raise (exn `((ix) (build tag ,tag)) "build failed")))))

)
