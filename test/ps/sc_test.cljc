(ns ps.sc-test
  (:require [ps.sc :as sut]
            [clojure.test :as t]
            [sc.api]))


(t/deftest last-ep-id-test
  #_"NOTE: This test contains eval magic.
     It is needed to delay letsc/defsc expansion or variable access.
     At lesat one ep must be defined at the expansion time.
     First ep is defined during the test run"

  (defn bar [strs]
    (mapv (fn [s]
            (mapv #(do (sc.api/spy)
                       (int %))
                  s))
          strs))

  (bar ["baz" "quux"])

  (t/testing "letsc-last"
    (t/is (= ["baz" "quux"] (eval (list `sut/letsc-last 'strs))))
    (t/is (= "quux" (eval (list `sut/letsc-last 's)))))

  (t/testing "defsc-last"
    (eval (list `sut/defsc-last))

    (t/is (= ["baz" "quux"] (eval 'strs)))
    (t/is (= "quux" (eval 's)))

    (t/testing "undefsc-last"
      (eval (list `sut/undefsc-last))

      (t/is (instance? java.lang.RuntimeException
                       (try (eval 'strs)
                            (catch java.lang.RuntimeException e e))))

      (t/is (instance? java.lang.RuntimeException
                       (try (eval 's)
                            (catch java.lang.RuntimeException e e)))))))
