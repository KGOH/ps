(ns ps-test
  (:require [ps :as sut]
            [clojure.test :as t]))


(t/deftest persist-scope-test

  (t/testing "simple let"
    (let [a 1]
      (sut/persist-scope)
      a)

    (t/is (= 1 a)))


  (t/testing "inside fn"
    (defn foo [x]
      (let [x2 (* 2 x)]
        (sut/persist-scope)
        x2))

    (foo 2)

    (t/is (= 2 x))
    (t/is (= 4 x2)))


  (t/testing "inside anon fn"
    (defn bar [strs]
      (mapv (fn [s]
              (mapv #(do (sut/persist-scope)
                         (int %))
                    s))
           strs))

    (bar ["baz" "quux"])

    (t/is (= ["baz" "quux"] strs))
    (t/is (= "quux" s))
    #_"NOTE: % is not persisted"))
