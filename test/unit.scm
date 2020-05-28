(module test.unit ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae)
(import tabulae.monad)
(import ix)
(import uuid)
(import test)

(test-group "ix"
  (define in "(some:object :key \"hello\" :t #t :f #f :another-key (subtask :key \"omg\" :n -97 :sym hello :ll [1 2 3] :sci -2.3e+77))")
  (define out `(sexp (identifier some object) (keyword :key) (string "hello") (keyword :t) (boolean #t) (keyword :f) (boolean #f) (keyword :another-key) (sexp (identifier subtask) (keyword :key) (string "omg") (keyword :n) (integer -97) (keyword :sym) (symbol hello) (keyword :ll) (list (natural 1) (natural 2) (natural 3)) (keyword :sci) (scientific -2.3e+77))))
  (test "ix parses fine" `(Just ,out) (parse:ix in))
  (test "ix with nested sexp fails flat parser gracefully" 'Nothing (parse:flat-ix in))
  (test "ix string roundtrip" in (stringify:ix (from-just (parse:ix in))))
  (test "ix structure roundtrip" `(Just ,out) (parse:ix (stringify:ix out)))
  (test "ix list parses" `(Just (list ,out ,out)) (parse:ix (<> "[" in " " in "]")))
  (test "unwrap string" `(Just "hello") (ix:unwrap `(string "hello")))
  (test "unwrap sexp" `(Just ((identifier something) (string "hello"))) (ix:unwrap `(sexp (identifier something) (string "hello"))))
  (define fancy `(sexp (identifier another object) (keyword :key) (string "hello") (keyword :another-key) (sexp (identifier subtask) (keyword :key) (string "omg") (keyword :n) (integer -97) (keyword :sym) (symbol hello) (keyword :ll) (list (natural 1) (natural 2) (natural 3)) (keyword :third) (sexp (identifier lol) (keyword :ok) (string "pls") (keyword :more) (string "blah")) (keyword :sci) (scientific -2.3e+77))))
  (test "keyword access" `(Just (string "hello")) ((^. (keyw :key)) fancy))
  (test "nested keyword access" `(Just (string "pls")) ((^. (keyw :another-key) (keyw :third) (keyw :ok)) fancy))
  (test "fake keys fail gracefully" `Nothing ((^. (keyw :x) (keyw :y) (keyw :z)) fancy))
  (define simple-str "(something :key \"lala\")")
  (define simple `(sexp (identifier something) (keyword :key) (string "lala")))
  (test "ix parses simple" `(Just ,simple) (parse:ix simple-str))
  (test "flat-ix parses simple" `(Just ,simple) (parse:flat-ix simple-str))
  (test "keyword set" `(Just (sexp (identifier something) (keyword :key) (string "singsong"))) ((.~ "singsong" (keyw :key)) simple))
  (test "mistyped keyword set fails" 'Nothing ((.~ 3 (keyw :key)) simple))
  (define simple2 `(sexp (identifier something) (keyword :key) (sexp (identifier next) (keyword :key2) (string "hello"))))
  (test "sexp set doesn't add parens" `(Just ,simple2) ((.~ `(sexp (identifier next) (keyword :key2) (string "hello")) (keyw :key)) simple2))
  (define simple3 `(sexp (identifier a) (keyword :n) (natural 1)))
  (test "sexp apply works" `(Just (sexp (identifier a) (keyword :n) (natural 2))) ((%~ (lambda (i) (* i 2)) (keyw :n)) simple3))
  (define simple4 `(sexp (identifier a) (keyword :l) (list (natural 2) (natural 4) (natural 6))))
  (test "list index works" `(Just (natural 4)) ((^. (keyw :l) (idx 1)) simple4))
  (test "list apply works" `(Just (natural 8)) ((^. (keyw :l) (idx 1)) (cadr ((%~ (lambda (i) (* i 2)) (keyw :l) (idx 1)) simple4))))
  (test "list index composes (ix)" `(Just (natural 4)) ((^. (idx 0) (keyw :l) (idx 1)) `(list ,simple4)))
  (test "list index composes (scheme)" `(Just (natural 4)) ((^. (idx 0) (keyw :l) (idx 1)) `(,simple4)))
  (define id (uuid))
  (define all-prims `(ix:test :sym symbol :uuid uuid :str string :int integer :nat natural :sci scientific :t boolean :f boolean))
  (ix:init (lambda (_) `(Just ,all-prims)))
  (define prims-s (<> "(ix:test :sym a :uuid \"" id "\" :str \"ok\" :int -4 :nat 3 :sci 4.5 :t #t :f #f)"))
  (define prims-oj (parse:ix prims-s))
  (printf "MAZ id: ~S\n" id)
  (test "build all primitive types (proto)" prims-oj (ix:build 'ix:test :sym 'a :uuid id :str "ok" :int -4 :nat 3 :sci 4.5 :t #t :f #f))
  (test-assert "all primitive types well-typed (proto)" (ix:well-typed? (from-just (ix:build 'ix:test :sym 'a :uuid id :str "ok" :int -4 :nat 3 :sci 4.5 :t #t :f #f))))
  (define prims-s-un (<> "(ix:* :sym a :uuid \"" id "\" :str \"ok\" :int -4 :nat 3 :sci 4.5 :t #t :f #f)"))
  (define prims-oj-un (parse:ix prims-s-un))
  (test "build all primitive types (generic)" prims-oj-un (ix:build 'ix:* :sym `(symbol a) :uuid `(uuid ,id) :str `(string "ok") :int `(integer -4) :nat `(natural 3) :sci `(scientific 4.5) :t `(boolean #t) :f `(boolean #f)))
  (test-assert "all primitive types well-typed (generic)" (ix:well-typed? (from-just (ix:build 'ix:* :sym `(symbol a) :uuid `(uuid ,id) :str `(string "ok") :int `(integer -4) :nat `(natural 3) :sci `(scientific 4.5) :t `(boolean #t) :f `(boolean #f)))))
  (test "validate empty" `(Just (sexp (identifier ix *))) (ix:validate `(sexp (identifier ix *))))
  (test "build empty" `(Just (sexp (identifier ix *))) (ix:build 'ix:*))
  (define bare-proto `(ix:test))
  (ix:init (lambda (_) `(Just ,bare-proto)))
  (test "zero keyword proto builds" `(Just (sexp (identifier ix test))) (ix:build 'ix:test))
  (test "zero keyword proto fails when given something" 'Nothing (ix:build 'ix:test :a 1))
  (define sum-prod-proto `(ix:test :s (sum natural string (product natural natural))
                                   :p (product (list string) (sum natural string))))
  (ix:init (lambda (_) `(Just ,sum-prod-proto)))
  (define spo1 `(sexp (identifier ix test) (keyword :s) (natural 1) (keyword :p) (product (list (string "hello")) (natural 2))))
  (define spo2 `(sexp (identifier ix test) (keyword :s) (string "a") (keyword :p) (product (list (string "s")) (string "d"))))
  (define spo3 `(sexp (identifier ix test) (keyword :s) (product (natural 3) (natural 4)) (keyword :p) (product (list (string "f")) (string "g"))))
  (define sps1 "(ix:test :s 1 :p {[\"hello\"] 2})")
  (define sps2 "(ix:test :s \"a\" :p {[\"s\"] \"d\"})")
  (define sps3 "(ix:test :s {3 4} :p {[\"f\"] \"g\"})")
  (test "sum/product parse take 1" `(Just ,spo1) (parse:ix sps1))
  (test "sum/product parse take 2" `(Just ,spo2) (parse:ix sps2))
  (test "sum/product parse take 3" `(Just ,spo3) (parse:ix sps3))
  (test "flat parser rejects products" 'Nothing (parse:flat-ix sps1))
  (test "sum/product validate take 1" `(Just ,spo1) ((<maybe>-bind (parse:ix sps1)) ix:validate))
  (test "sum/product validate take 2" `(Just ,spo2) ((<maybe>-bind (parse:ix sps2)) ix:validate))
  (test "sum/product validate take 3" `(Just ,spo3) ((<maybe>-bind (parse:ix sps3)) ix:validate))
  (test "sum/product build take 1" `(Just ,spo1) (ix:build 'ix:test :s `(natural 1) :p `(("hello") (natural 2))))
  (test "sum/product build take 2" `(Just ,spo2) (ix:build 'ix:test :s `(string "a") :p `(("s") (string "d"))))
  (test "sum/product build take 3" `(Just ,spo3) (ix:build 'ix:test :s `(product 3 4) :p `(("f") (string "g"))))
  (define sum-list-proto `(ix:test :l (list (sum natural string))))
  (ix:init (lambda (_) `(Just ,sum-list-proto)))
  (define slo1 `(sexp (identifier ix test) (keyword :l) (list (natural 1) (natural 2) (natural 3))))
  (define slo2 `(sexp (identifier ix test) (keyword :l) (list (natural 4) (string "five") (natural 6))))
  (define slo3 `(sexp (identifier ix test) (keyword :l) (list (natural 7) (symbol eight))))
  (define sls1 "(ix:test :l [1 2 3])")
  (define sls2 "(ix:test :l [4 \"five\" 6])")
  (define sls3 "(ix:test :l [7 eight])")
  (test "homogenous sum list parses" `(Just ,slo1) (parse:ix sls1))
  (test "heterogenous sum list parses" `(Just ,slo2) (parse:ix sls2))
  (test "ill-typed sum list parses" `(Just ,slo3) (parse:ix sls3))
  (test "homogenous sum list validates" `(Just ,slo1) ((<maybe>-bind (parse:ix sls1)) ix:validate))
  (test "heterogenous sum list validates" `(Just ,slo2) ((<maybe>-bind (parse:ix sls2)) ix:validate))
  (test "ill-typed sum list fails to validate" 'Nothing ((<maybe>-bind (parse:ix sls3)) ix:validate))
  (test "homogenous sum list builds" `(Just ,slo1) (ix:build 'ix:test :l `((natural 1) (natural 2) (natural 3))))
  (test "heterogenous sum list builds" `(Just ,slo2) (ix:build 'ix:test :l `((natural 4) (string "five") (natural 6))))
  (test "ill-typed sum list fails to build" 'Nothing (ix:build 'ix:test :l `((natural 7) (symbol eight))))
  (define opt-proto `(ix:test :r1 natural :o1 (optional natural) :r2 natural :o2 (optional natural)))
  (ix:init (lambda (_) `(Just ,opt-proto)))
  (define soo1 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :o1) (natural 1) (keyword :r2) (natural 2) (keyword :o2) (natural 2)))
  (define soo2 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :r2) (natural 2) (keyword :o2) (natural 2)))
  (define soo3 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :r2) (natural 2)))
  (define soo4 `(sexp (identifier ix test) (keyword :r1) (natural 1) (keyword :o1) (natural 1) (keyword :o2) (natural 2)))
  (define sos1 "(ix:test :r1 1 :o1 1 :r2 2 :o2 2)")
  (define sos2 "(ix:test :r1 1 :r2 2 :o2 2)")
  (define sos3 "(ix:test :r1 1 :r2 2)")
  (define sos4 "(ix:test :r1 1 :o1 1 :o2 2)")
  (test "required/optional parses" `(Just ,soo1) (parse:ix sos1))
  (test "required/optional parses missing one optional" `(Just ,soo2) (parse:ix sos2))
  (test "required/optional parses missing both optionals" `(Just ,soo3) (parse:ix sos3))
  (test "required/optional parses missing required" `(Just ,soo4) (parse:ix sos4))
  (test "required/optional validates" `(Just ,soo1) ((<maybe>-bind (parse:ix sos1)) ix:validate))
  (test "required/optional validates missing one optional" `(Just ,soo2) ((<maybe>-bind (parse:ix sos2)) ix:validate))
  (test "required/optional validates missing both optionals" `(Just ,soo3) ((<maybe>-bind (parse:ix sos3)) ix:validate))
  (test "required/optional fails to validate missing required" 'Nothing ((<maybe>-bind (parse:ix sos4)) ix:validate))
  (test "required/optional builds" `(Just ,soo1) (ix:build 'ix:test :r1 1 :o1 1 :r2 2 :o2 2))
  (test "required/optional builds missing one optional" `(Just ,soo2) (ix:build 'ix:test :r1 1 :r2 2 :o2 2))
  (test "required/optional builds missing both optionals" `(Just ,soo3) (ix:build 'ix:test :r1 1 :r2 2))
  (test "required/optional fails to build missing required" 'Nothing (ix:build 'ix:test :r1 1 :o1 1 :o2 2)))

(test-group "json"
  (define in "(some:object :key \"hello\" :t #t :f #f :another-key (subtask :key \"omg\" :n -97 :sym hello :ll [1 2 3] :sci -2.3e+77))")
  (printf "MAZ json: ~A\n" (stringify:ix->json (from-just (parse:ix in)))))

(test-exit)

)
