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

(define oid
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
(define (ix-parser #!key flat) (letrec
  ((untroublesome (sat (lambda (c) (memv c (string->list "!$%&*+-./;<=>?@^_~")))))
   (keyval (do/m <parser>
     (return (void))
     (k <- ix-keyword)
     (many1 whitespace)
     (v <- term)
     (return `(,k ,v))))
   (term (do/m <parser>
     (return (void))
     (t <- (apply <?> `(,@(if flat '() `(,ix-sexp ,ix-list ,ix-product))
                        ,ix-oid ,ix-string ,ix-int/nat/sci ,ix-bool ,ix-symbol)))
     (many whitespace)
     (return t)))
   (ix-oid (do/m <parser>
     (value #\")
     (o <- oid)
     (value #\")
     (return `(oid ,o))))
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
     (s <- (many1 (<?> alphanum untroublesome (value #\:))))
     (return `(identifier ,@(map string->symbol (string-split s ":"))))))
   (ix-symbol (do/m <parser>
     (return (void))
     (s1 <- (<?> alphanum untroublesome))
     (sn <- (many (<?> alphanum untroublesome (value #\:))))
     (return `(symbol ,(string->symbol (cons* s1 sn))))))
   (ix-keyword (do/m <parser>
     (value #\:)
     (s <- (many1 (<?> alphanum untroublesome)))
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

)
