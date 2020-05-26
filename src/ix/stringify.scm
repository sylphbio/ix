(module ix.stringify (ix)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.format)
(import chicken.string)
(import chicken.keyword)

(import tabulae)

; XXX ix->json
; what json types are there. number, string, object, array, bool, null
; sexp/list/keyword/string/boolean are obvious
; sum and and enum aren't encoded
; int/nat/sci/sym/prod/uuid require tags
; identifiers also have a special "unnamed" key like #identifier

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
         ((boolean) (<> (if (cadr sx) "#t" "#f")))
         (else #f))))

)
