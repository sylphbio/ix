(module ix.static *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.condition)

(import tabulae)

; allowed "special" characters in symbols and keywwords
; symbols additionally permit non-leading colons, so they can carry identifiers
; the actual "troublesome" characters are !"#'(),:[]\`{}|
; doublequote bracket paren backslash are obvious
; singlequote backtick comma colon hash bar have special meaning in scheme
; bang is by authorial fiat because it breaks tcsh
; but I can't just write those characters and allow everything else
; because I also want to ban nonprinting ascii, whitespace...
; weird control characters like combiners and direction-reversers and shit...
; for now it's a-zA-Z0-9 and these symbols, I would like to at least support accents and japanese tho
; afaik unicode is so absurdly complicated there's no way to enforce "ok do whatever except no weird shit ok!!"
(define untroublesome (string->list "$%&*+-./;<=>?@^_~"))

; what it says
(define tags `(sexp list product sum enum identifier keyword symbol uuid string integer natural scientific boolean))

; identifier and keyword are purely structural and do not appear tagged in prototypes
(define prototype-tags (difference* tags `(identifier keyword)))

; sum is reified into a concrete type on objects
(define object-tags (difference* tags `(sum)))

; these appear on prototypes in list head position
(define complex-tags `(sexp list product sum enum identifier))

; these appear on prototypes as simple symbols
(define primitive-tags (difference* tags complex-tags))

)
