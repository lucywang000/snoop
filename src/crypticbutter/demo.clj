(ns crypticbutter.demo
  (:require [crypticbutter.snoop :as snoop :refer [>defn =>]]))

(>defn f1
  [(x :int)
   (y :int)
   z]
  (+ x y z))

(>defn f2
  ([x] (f2 x 1))
  ([x y] (+ x y)))

(>defn add [_ _]
  [:=> [:cat int? int?] string?]
  "0")

(>defn prepost
  [x _]
  {:=> [:any int? => int?]}
  x)

(>defn inline-variadic [(_ int?) & (_more [:* string?])]
  true)

(>defn ^{::snoop/macro-config {:enabled? false}}
  d-m
  []
  [int? => :nil]
  5)

(>defn d-a2
      ([]
       [int? => :nil]
       5)
      #_:clj-kondo/ignore
      {::snoop/macro-config {:enabled? false}})

(comment

  (macroexpand-1 '(>defn
                   f1
                   [(x :int)
                    (y :int)
                    z]
                   (+ x y z)))

  (macroexpand-1 '(>defn inline-variadic [(_ int?) & (_more [:* string?])]
                    true))

  (f1 1 "2" "3")
  (add 1 2)
  (add "1" 2)
  (inline-variadic 1)
  (inline-variadic 1 1)
  (inline-variadic 1 "1")
  (d-m)

  (d-a2)

  :end-comment)
