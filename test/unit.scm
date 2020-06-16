(module test.unit ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)
(import ix)
(import uuid)
(import test)

; XXX TODO FIXME this fucking sucks
; I think what I want is to split the gneral ix testing into two pieces
; declarative alist or whatever of like, ix text, ix object, prototype if applicable, json text, etc
; and then a battery function that does all the various things like make sure it parses, builds whatever

(test-group "ix"
  (define in "(some:object :key \"hello\" :t #t :f #f :another-key (subtask :key \"omg\" :n -97 :sym hello :ll [1 2 3] :sci -2.3e+77))")
  (define out `(sexp (identifier some object) (keyword :key) (string "hello") (keyword :t) (boolean #t) (keyword :f) (boolean #f) (keyword :another-key) (sexp (identifier subtask) (keyword :key) (string "omg") (keyword :n) (integer -97) (keyword :sym) (symbol hello) (keyword :ll) (list (natural 1) (natural 2) (natural 3)) (keyword :sci) (scientific -2.3e+77))))
  (test "ix parses fine" out (parse:ix in))
  (test-error "ix with nested sexp fails flat parser" (parse:flat-ix in))
  (test "ix string roundtrip" in (stringify:ix (parse:ix in)))
  (test "ix structure roundtrip" out (parse:ix (stringify:ix out)))
  (test "ix list parses" `(list ,out ,out) (parse:ix (<> "[" in " " in "]")))
  (test "unwrap string" "hello" (ix:unwrap `(string "hello")))
  (define fancy `(sexp (identifier another object) (keyword :key) (string "hello") (keyword :another-key) (sexp (identifier subtask) (keyword :key) (string "omg") (keyword :n) (integer -97) (keyword :sym) (symbol hello) (keyword :ll) (list (natural 1) (natural 2) (natural 3)) (keyword :third) (sexp (identifier lol) (keyword :ok) (string "pls") (keyword :more) (string "blah")) (keyword :sci) (scientific -2.3e+77))))
  (test "keyword access" `(string "hello") ((^. (keyw :key)) fancy))
  (test "nested keyword access" `(string "pls") ((^. (keyw :another-key) (keyw :third) (keyw :ok)) fancy))
  (test "fake keys fail" #f ((^. (keyw :x) (keyw :y) (keyw :z)) fancy))
  (define simple-str "(something :key \"lala\")")
  (define simple `(sexp (identifier something) (keyword :key) (string "lala")))
  (test "ix parses simple" simple (parse:ix simple-str))
  (test "flat-ix parses simple" simple (parse:flat-ix simple-str))
  (test "keyword set" `(sexp (identifier something) (keyword :key) (string "singsong")) ((.~ "singsong" (keyw :key)) simple))
  (test "mistyped keyword set fails" #f ((.~ 3 (keyw :key)) simple))
  (define simple2 `(sexp (identifier something) (keyword :key) (sexp (identifier next) (keyword :key2) (string "hello"))))
  (test "sexp set doesn't add parens" simple2 ((.~ `(sexp (identifier next) (keyword :key2) (string "hello")) (keyw :key)) simple2))
  (define simple3 `(sexp (identifier a) (keyword :n) (natural 1)))
  (test "sexp apply works" `(sexp (identifier a) (keyword :n) (natural 2)) ((%~ (lambda (i) (* i 2)) (keyw :n)) simple3))
  (define simple4 `(sexp (identifier a) (keyword :l) (list (natural 2) (natural 4) (natural 6))))
  (test "list index works" `(natural 4) ((^. (keyw :l) (idx 1)) simple4))
  (test "list apply works" `(natural 8) ((^. (keyw :l) (idx 1)) ((%~ (lambda (i) (* i 2)) (keyw :l) (idx 1)) simple4)))
  (test "list index composes (ix)" `(natural 4) ((^. (idx 0) (keyw :l) (idx 1)) `(list ,simple4)))
  (test "list index composes (scheme)" `(natural 4) ((^. (idx 0) (keyw :l) (idx 1)) `(,simple4)))
  (define id (uuid))
  (define all-prims `(ix:test :sym symbol :uuid uuid :str string :int integer :nat natural :sci scientific :t boolean :f boolean))
  (ix:register `(,all-prims))
  (define prims-s (<> "(ix:test :sym a :uuid \"" id "\" :str \"ok\" :int -4 :nat 3 :sci 4.5 :t #t :f #f)"))
  (define prims-oj (parse:ix prims-s))
  (test "build all primitive types (proto)" prims-oj (ix:build 'ix:test :sym 'a :uuid id :str "ok" :int -4 :nat 3 :sci 4.5 :t #t :f #f))
  (test-assert "all primitive types well-typed (proto)" (ix:well-typed? (ix:build 'ix:test :sym 'a :uuid id :str "ok" :int -4 :nat 3 :sci 4.5 :t #t :f #f)))
  (define prims-s-un (<> "(ix:* :sym a :uuid \"" id "\" :str \"ok\" :int -4 :nat 3 :sci 4.5 :t #t :f #f)"))
  (define prims-oj-un (parse:ix prims-s-un))
  (test "build all primitive types (generic)" prims-oj-un (ix:build 'ix:* :sym `(symbol a) :uuid `(uuid ,id) :str `(string "ok") :int `(integer -4) :nat `(natural 3) :sci `(scientific 4.5) :t `(boolean #t) :f `(boolean #f)))
  (test-assert "all primitive types well-typed (generic)" (ix:well-typed? (ix:build 'ix:* :sym `(symbol a) :uuid `(uuid ,id) :str `(string "ok") :int `(integer -4) :nat `(natural 3) :sci `(scientific 4.5) :t `(boolean #t) :f `(boolean #f))))
  (test "validate empty" `(sexp (identifier ix *)) (ix:validate `(sexp (identifier ix *))))
  (test "build empty" `(sexp (identifier ix *)) (ix:build 'ix:*))
  (define bare-proto `(ix:test))
  (ix:register `(,bare-proto))
  (test "zero keyword proto builds" `(sexp (identifier ix test)) (ix:build 'ix:test))
  (test-error "zero keyword proto fails when given something" (ix:build 'ix:test :a 1))
  (define sum-prod-proto `(ix:test :s (sum natural string (product natural natural))
                                   :p (product (list string) (sum natural string))))
  (ix:register `(,sum-prod-proto))
  (define spo1 `(sexp (identifier ix test) (keyword :s) (natural 1) (keyword :p) (product (list (string "hello")) (natural 2))))
  (define spo2 `(sexp (identifier ix test) (keyword :s) (string "a") (keyword :p) (product (list (string "s")) (string "d"))))
  (define spo3 `(sexp (identifier ix test) (keyword :s) (product (natural 3) (natural 4)) (keyword :p) (product (list (string "f")) (string "g"))))
  (define sps1 "(ix:test :s 1 :p {[\"hello\"] 2})")
  (define sps2 "(ix:test :s \"a\" :p {[\"s\"] \"d\"})")
  (define sps3 "(ix:test :s {3 4} :p {[\"f\"] \"g\"})")
  (test "sum/product parse take 1" spo1 (parse:ix sps1))
  (test "sum/product parse take 2" spo2 (parse:ix sps2))
  (test "sum/product parse take 3" spo3 (parse:ix sps3))
  (test-error "flat parser rejects products" (parse:flat-ix sps1))
  (test "sum/product validate take 1" spo1 (ix:validate (parse:ix sps1)))
  (test "sum/product validate take 2" spo2 (ix:validate (parse:ix sps2)))
  (test "sum/product validate take 3" spo3 (ix:validate (parse:ix sps3)))
  (test "sum/product build take 1" spo1 (ix:build 'ix:test :s `(natural 1) :p `(("hello") (natural 2))))
  (test "sum/product build take 2" spo2 (ix:build 'ix:test :s `(string "a") :p `(("s") (string "d"))))
  (test "sum/product build take 3" spo3 (ix:build 'ix:test :s `(product 3 4) :p `(("f") (string "g"))))
  (define sum-list-proto `(ix:test :l (list (sum natural string))))
  (ix:register `(,sum-list-proto))
  (define slo1 `(sexp (identifier ix test) (keyword :l) (list (natural 1) (natural 2) (natural 3))))
  (define slo2 `(sexp (identifier ix test) (keyword :l) (list (natural 4) (string "five") (natural 6))))
  (define slo3 `(sexp (identifier ix test) (keyword :l) (list (natural 7) (symbol eight))))
  (define sls1 "(ix:test :l [1 2 3])")
  (define sls2 "(ix:test :l [4 \"five\" 6])")
  (define sls3 "(ix:test :l [7 eight])")
  (test "homogenous sum list parses" slo1 (parse:ix sls1))
  (test "heterogenous sum list parses" slo2 (parse:ix sls2))
  (test "ill-typed sum list parses" slo3 (parse:ix sls3))
  (test "homogenous sum list validates" slo1 (ix:validate (parse:ix sls1)))
  (test "heterogenous sum list validates" slo2 (ix:validate (parse:ix sls2)))
  (test-error "ill-typed sum list fails to validate" (ix:validate (parse:ix sls3)))
  (test "homogenous sum list builds" slo1 (ix:build 'ix:test :l `((natural 1) (natural 2) (natural 3))))
  (test "heterogenous sum list builds" slo2 (ix:build 'ix:test :l `((natural 4) (string "five") (natural 6))))
  (test-error "ill-typed sum list fails to build" (ix:build 'ix:test :l `((natural 7) (symbol eight))))
  (define opt-proto `(ix:test :r1 natural :o1 (optional natural) :r2 natural :o2 (optional natural)))
  (ix:register `(,opt-proto))
  (define soo1 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :o1) (natural 1) (keyword :r2) (natural 2) (keyword :o2) (natural 2)))
  (define soo2 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :r2) (natural 2) (keyword :o2) (natural 2)))
  (define soo3 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :r2) (natural 2)))
  (define soo4 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :o1) (natural 1) (keyword :o2) (natural 2)))
  (define sos1 "(ix:test :r1 1 :o1 1 :r2 2 :o2 2)")
  (define sos2 "(ix:test :r1 1 :r2 2 :o2 2)")
  (define sos3 "(ix:test :r1 1 :r2 2)")
  (define sos4 "(ix:test :r1 1 :o1 1 :o2 2)")
  (test "required/optional parses" soo1 (parse:ix sos1))
  (test "required/optional parses missing one optional" soo2 (parse:ix sos2))
  (test "required/optional parses missing both optionals" soo3 (parse:ix sos3))
  (test "required/optional parses missing required" soo4 (parse:ix sos4))
  (test "required/optional validates" soo1 (ix:validate (parse:ix sos1)))
  (test "required/optional validates missing one optional" soo2 (ix:validate (parse:ix sos2)))
  (test "required/optional validates missing both optionals" soo3 (ix:validate (parse:ix sos3)))
  (test-error "required/optional fails to validate missing required" (ix:validate (parse:ix sos4)))
  (test "required/optional builds" soo1 (ix:build 'ix:test :r1 1 :o1 1 :r2 2 :o2 2))
  (test "required/optional builds missing one optional" soo2 (ix:build 'ix:test :r1 1 :r2 2 :o2 2))
  (test "required/optional builds missing both optionals" soo3 (ix:build 'ix:test :r1 1 :r2 2))
  (test-error "required/optional fails to build missing required" (ix:build 'ix:test :r1 1 :o1 1 :o2 2)))

(test-group "json"
  (define in "(some:object :key \"hello\" :t #t :f #f :another-key (subtask :key \"omg\" :n -97 :sym hello :ll [1 2 3] :sci -2.3e+77))")
  (printf "MAZ stringify json: ~A\n" (stringify:ix->json (parse:ix in)))
  (define all-prims `(ix:test :sym symbol :uuid uuid :str string :int integer :nat natural :sci scientific :t boolean :f boolean))
  (ix:register `(,all-prims))
  (printf "MAZ parse json: ~S\n" (parse:json->ix "[{\"key\": \"val\", \"list\": [\"one\", 2, -0, +9.4, true]}]"))
  (define id (uuid))
  (define all-prim-json #<#EOM
{ "####identifier": "ix:test", "sym": "hello", "uuid": "#id", "str": "ok", "int": 1, "nat": "2", "sci": "3.4", "t": true, "f": false }
EOM
)
  (printf "MAZ parse json proto: ~S\n" (parse:json->ix all-prim-json)))

(test-exit)

)
