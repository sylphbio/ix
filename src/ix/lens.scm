(module ix.lens (keyw idx ident ^. .~ %~ ^.? ^.v ^.M .~M %~M)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.keyword)

(import tabulae)
(import tabulae.monad)

(import (prefix ix.base ix:))
(import ix.static)

; XXX TODO FIXME I should rewrite this, especially now that the monads have become an impediment
; main thing is by keyw/ident/etc returning lambdas, I can't explain where/why something failed
; which was fine when everything got transmuted into Nothing but now it sucks
; thinking they should actually return commands in a dsl
; and my actual lens application is a simple interpreter
; would be a good toy problem in that vein to work out my style
; before I fuck with this tho I need to define what all I actually need and design it out

; get index of the keyword, take/drop one past it (ie the value is the car of the righthand list)
; then get can extract the value, set/apply rebuild the structure with the new value
; XXX TODO this doesn't admit any way to add a key (relevant to ix generics)
(define (keyw k) (lambda (l)
  (do/m <maybe>
    (k^ <- (cond ((ix:keyword? k) (return k))
                 ((keyword? k) (return `(keyword ,k)))
                 (else (fail))))
    (n <- (let ((i (findi* (lambda (v) (equal? v k^)) l)))
               (if i (return (+ i 1)) (fail))))
    (declare left (take* n l))
    (declare right (drop* n l))
    (if (null? right) (fail) (return `(,left . ,right))))))

; split a list in the same manner on explicit index
(define (idx i) (lambda (l)
  (do/m <maybe>
    ; this weird thing is, we want this lens to work on ix lists and products, or normal lists
    ; and adjust an offset to handle the type tag in the former case
    (i^ <- (to-maybe (cond ((and (or (ix:list? l) (ix:product? l)) (>= (length l) (+ i 2))) (+ i 1))
                           ((and (not (ix:ix? l)) (list? l) (>= (length l) (+ i 1))) i)
                           (else #f))))
    (return `(,(take* i^ l) . ,(drop* i^ l))))))

; get the identifier of a sexp
(define (ident l)
  (if (ix:sexp? l)
      (<maybe>-return `(,(take* 1 l) . ,(drop* 1 l)))
      (<maybe>-fail)))

; simple getter for an arbitrary number of lenses
(define (get g1 . gs) (lambda (l)
  (foldl (lambda (r g) (do/m <maybe>
                         (r^ <- (>>= r g))
                         (return (cadr r^))))
         (<maybe>-return l)
         (cons g1 gs))))

; this rather ugly thing applies a bunch of lenses, then applies the function, then rewraps the structure
; the clever part is I realized set is actually a special case of function application, so one impl for both
; f is unary and acts on a type-wrapped input
; ss is a list of lens function things
; p is a maybe containing a pair of left and right (eg as returned by keyw)
(define (apply-in f ss p)
  (do/m <maybe>
    (p^ <- p)
    (declare left (car p^))
    (declare mid (cadr p^))
    (declare right (cddr p^))
    (mid^ <- (if (null? ss)
                 (let ((mid^ (f mid)))
                      ; XXX TODO FIXME I should use prototype lookup to typecheck sexp/list/enum
                      ; sexp is straightforward, compare tags and validate
                      ; list and enum tho I need to find the containing sexp tag and use validate-value
                      ; so making this improvement is nontrivial
                      (if (ix:well-typed? mid^)
                          (return mid^)
                          (fail)))
                 (apply-in f (cdr ss) ((car ss) mid))))
    (return `(,@left ,mid^ ,@right))))

; XXX I don't have a way to add to or remove from an ix list, I can only replace an inhabited index
(define (set r s1 . ss) (lambda (l)
  ; XXX FIXME this is wrong, it double-wraps unwrapped lists/products. need better strat
  (define r^ (if (ix:ix? r) (cdr r) `(,r)))
  (define (f v) `(,(car v) ,@r^))
  (do/m <maybe>
    (fin <- (apply-in f (cons s1 ss) (<maybe>-return `(() . (,l)))))
    (return (car fin)))))

; we lift f over the type because schema types should never change
; XXX I don't have a mechanism to nest maps tho, eg a function over a list needs to unwrap items itself
(define (app f s1 . ss) (lambda (l)
  (define (g v) (ix:wrap (car v) (f (ix:unwrap v))))
  (do/m <maybe>
    (fin <- (apply-in g (cons s1 ss) (<maybe>-return `(() . (,l)))))
    (return (car fin)))))

(define (^. . args) (lambda (l)
  (let ((r ((apply get args) l)))
       (if (just? r) (from-just r) #f))))

(define (.~ . args) (lambda (l)
  (let ((r ((apply set args) l)))
       (if (just? r) (from-just r) #f))))

(define (%~ . args) (lambda (l)
  (let ((r ((apply app args) l)))
       (if (just? r) (from-just r) #f))))

(define (^.? . args) (lambda (l) (just? ((apply get args) l))))
(define (^.v . args) (lambda (l) (ix:unwrap ((apply ^. args) l))))

; I hate making this not part of ix.monad but it really just makes sense to do it this way
(define ^.M get)
(define .~M set)
(define %~M app)

)
