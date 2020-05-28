(module ix.parse (ix flat-ix)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.format)
(import chicken.string)
(import chicken.keyword)

(import tabulae)
(import tabulae.parsec)
(import tabulae.monad)

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

(define (ix sx) (parse->maybe ((ix-parser) sx)))
(define (flat-ix sx) (parse->maybe ((ix-parser :flat #t) sx)))

; XXX json->ix
; js string/obj/array/bool go straight to ix string/sexp/list/bool
; js number just take to whitespace and use scheme string->number
; and assign the narrowest valid ix numeric type to it
; for keywords make sure to check for blacklisted symbols
; null is an error
; in this way arbitary json (without null or fucky characters in keywords) can be turned to (shitty) ix
; as for our own semantics...
; js keywords of the form "name#type" we use the type following the hash blindly
; this is produced by ix->json for int/nat/sci/sym/uuid (all stored as string) and product (as list)
; sums and enums are not part of generic ix so validate can set them up as needed
; also a special js keyword simply "#identifier" which does what you'd expect
; the one tricky thing is json may trash key order so we need to parse the whole thing to check for ident
; call validate on any resulting object(s)
; allow object or list as the toplevel
; in this manner we get (almost) arbitrary json->ix and also no information loss for ix->json->ix
; XXX I don't support js string escapes yet but valid ones are \b \f \n \r \t \" \\

; hm wait so why can't I just say, ok, identified sexps convert as per their prototypes
; generic ix... just accept that it's lossy? numbers symbols and products are the only things we really lose
; so I could parse out all the keys/values including identifier
; then run everything through build if we have an ident, or set what we can by hand and validate if not
; only support needed is to allow numbers and symbols to convert up from strings
; products get tagged properly in wrap-build-kv-pair, enums get caught by symbol

;(define (json->ix js)

)
