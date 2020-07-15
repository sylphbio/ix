(module ix.stringify (ix ix->json prototype)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.format)
(import chicken.string)
(import chicken.keyword)

(import tabulae)

(import (prefix ix.base ix:))
(import ix.static)

; XXX FIXME when I impl escapes note this naive treatment of string is inadequate
; specifically because scheme has nonstandard multichar escapes like \null and \tab
; I'm... inclined to ban null, support the usual whitespace escapes plus backslash and quote
; allow full utf8 and don't provide any hex escape mechanism or anything like that
; XXX sig wanted a binary rep for sci but there are algorithms that produce correct text reps for 100% of values
(define (ix sx)
  (let ((into (lambda (l) (string-intersperse (map ix l) " "))))
       (case (and (list? sx) (car sx))
         ((sexp) (<> "(" (into (cdr sx)) ")"))
         ((list) (<> "[" (into (cdr sx)) "]"))
         ((product) (<> "{" (into (cdr sx)) "}"))
         ((identifier) (string-intersperse (map symbol->string (cdr sx)) ":"))
         ((keyword) (<> ":" (keyword->string (cadr sx))))
         ((enum symbol) (symbol->string (cadr sx)))
         ((uuid string) (<> "\"" (cadr sx) "\""))
         ((integer natural scientific) (number->string (cadr sx)))
         ((boolean) (if (cadr sx) "#t" "#f"))
         (else (die "malformed input: ~S" sx)))))

; js types are number/string/object/array/bool/null
; sexp/list/kw/string/bool are encoded directly
; int/nat/sci are stringified because js numbers are trash
; sym/enum also stringified for obvious reason
; uuid and product are elided without conversion
; identifier is converted to tag string and gets special ##identifier key
(define (ix->json sx)
  (case (and (list? sx) (car sx))
    ((sexp) (<> "{ "
                (ix->json (cadr sx))
                ", "
                (string-intersperse (map (lambda (k/v) (<> (car k/v) " " (cadr k/v)))
                                         (chop (map ix->json (cddr sx)) 2))
                                    ", ")
                " }"))
    ((list product) (<> "[ " (string-intersperse (map ix->json (cdr sx)) ", ") " ]"))
    ((identifier) (<> "\"##identifier\": \"" (string-intersperse (map symbol->string (cdr sx)) ":") "\""))
    ((keyword) (<> "\"" (keyword->string (cadr sx)) "\":"))
    ((enum symbol) (<> "\"" (symbol->string (cadr sx)) "\""))
    ((uuid string) (<> "\"" (cadr sx) "\""))
    ((integer natural scientific) (<> "\"" (number->string (cadr sx)) "\""))
    ((boolean) (if (cadr sx) "true" "false"))
    (else (die "malformed input: ~S" sx))))

; dead simple
; XXX this could check prototype-tags but it would need some context awareness to handle sexp idents correctly
(define (prototype p)
  (cond ((list? p) (<> "(" (string-intersperse (map prototype p)) ")"))
        ((keyword? p) (<> ":" (keyword->string p)))
        ((symbol? p) (symbol->string p))
        (else (error "invalid type in prototype" p))))

)
