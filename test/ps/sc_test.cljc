(ns ps.sc-test
  (:require [ps.sc :as sut]
            [clojure.test :as t]
            [sc.api]))


(defmacro undefined-sym? [quoted-sym]
  `(instance? java.lang.RuntimeException
              (try (eval ~quoted-sym)
                   (catch java.lang.RuntimeException e# e#))))


(t/deftest last-ep-id-test
  (defn bar [strs]
    (mapv (fn [s]
            (mapv #(do (sc.api/spy)
                       (int %))
                  s))
          strs))


  (t/testing "letsc-last"
    (bar ["baz" "quux"])

    (t/is (= ["baz" "quux"] (sut/letsc-last 'strs)))
    (t/is (= "quux" (sut/letsc-last 's))))


  (t/testing "defsc-last"
    (bar ["baz1" "quux1"])

    (sut/defsc-last)

    (t/is (= ["baz1" "quux1"] (eval `strs)))
    (t/is (= "quux1" (eval `s)))


    (t/testing "last ep id is calculated at the call time"
      (bar ["baz2" "quux2"])

      (sut/defsc-last)

      (t/is (= ["baz2" "quux2"] (eval `strs)))
      (t/is (= "quux2" (eval `s))))


    (t/testing "undefsc-last"
      (sut/undefsc-last)

      (t/is (undefined-sym? `strs))

      (t/is (undefined-sym? `s)))))


(t/deftest letsc-all-test
  (sc.api/dispose-all!)
  (sut/drop-letsc-select!)

  (defn collatz
    ([x] (collatz x 0))
    ([x i]
     (sc.api/spy)
     (cond
       (= 1 x)   i
       (even? x) (recur (quot x 2) (inc i))
       (odd? x)  (recur (inc (* 3 x)) (inc i)))))

  (t/testing "letsc-all"
    (t/is (= [] (sut/letsc-all 'x)))

    (collatz 5)

    (t/is (= [5 16 8 4 2 1]
             (sut/letsc-all 'x)))
    (t/is (= [0 1 2 3 4 5]
             (sut/letsc-all 'i)))
    (t/is (= [[0 5]
              [1 16]
              [2 8]
              [3 4]
              [4 2]
              [5 1]]
             (sut/letsc-all '[i x]))))

  (t/testing "select ep id"
    (t/is (= [5 16 8 4 2 [1]]
             (sut/letsc-select-start! 'x)))

    (t/testing "can't go next after last"
      (t/is (= [5 16 8 4 2 [1]]
               (sut/letsc-select-next!))))

    (t/is (= [5 16 8 4 [2] 1]
             (sut/letsc-select-prev!)))

    (t/testing "start on the same expr does nothing"
      (t/is (= [5 16 8 4 [2] 1]
               (sut/letsc-select-start! 'x))))

    (t/testing "can swictch to other expr and back"
      (t/is (= [0 1 2 3 [4] 5]
               (sut/letsc-select-start! 'i)))

      (t/is (= [5 16 8 4 [2] 1]
               (sut/letsc-select-start! 'x))))

    (t/is (= [5 16 8 [4] 2 1]
             (sut/letsc-select-prev!)))

    (t/is (= [5 16 [8] 4 2 1]
             (sut/letsc-select-prev!)))

    (t/is (= [5 [16] 8 4 2 1]
             (sut/letsc-select-prev!)))

    (t/is (= [[5] 16 8 4 2 1]
             (sut/letsc-select-prev!)))

    (t/testing "can't go prev after first"
      (t/is (= [[5] 16 8 4 2 1]
               (sut/letsc-select-prev!))))

    (t/is (= [5 16 8 4 2 [1]]
             (sut/letsc-select-last!)))

    (t/is (= [5 16 [8] 4 2 1]
             (sut/letsc-select-nth! 2)))

    (t/is (= [5 16 [8] 4 2 1]
             (sut/letsc-select-current!)))

    (t/is (= [[5] 16 8 4 2 1]
             (sut/letsc-select-nth! -10)))

    (t/is (= [5 16 8 4 2 [1]]
             (sut/letsc-select-nth! 100)))

    (t/is (= [[5] 16 8 4 2 1]
             (sut/letsc-select-first!)))

    (t/is (= [5 [16] 8 4 2 1]
             (sut/letsc-select-next!)))

    (t/is (= [5 16 [8] 4 2 1]
             (sut/letsc-select-next!)))

    (t/testing "letsc selected"
      (t/is (= 8 (sut/letsc-selected 'x)))
      (t/is (= 2 (sut/letsc-selected 'i))))

    (t/testing "defsc selected"
      (sut/defsc-selected)

      (t/is (= 8 (eval `x)))
      (t/is (= 2 (eval `i)))

      (sut/undefsc-lastdef)

      (t/is (undefined-sym? `x))
      (t/is (undefined-sym? `i)))))


(t/deftest undefsc-restore-test
  (def this-ns (find-ns 'ps.sc-test)) #_"NOTE: can't trust *ns*"
  (defn this-ns? [v] (= this-ns (:ns (meta v))))

  (def z 3)

  (defn f1 [x y]
    (sc.api/spy)
    (zipmap `[x y z] [x y z]))

  (defn f2 [x y z]
    (sc.api/spy)
    (zipmap `[x y z] [x y z]))

  (t/is (= {`x 1 `y 2 `z 3} (f1 1 2)))
  (sut/undefsc-lastdef)
  (t/is (every? this-ns? (sut/defsc-last)))
  (t/is (= {`x 1 `y 2 `z 3} (f1 (eval `x) (eval `y))))

  (t/is (= {`x 4 `y 5 `z 6} (f2 4 5 6)))
  (sut/undefsc-lastdef)
  (t/is (every? this-ns? (sut/defsc-last)))
  (t/is (contains? @sut/original-var-values `z))
  (t/is (= {`x 4 `y 5 `z 6} (f2 (eval `x) (eval `y) (eval `z))))

  (t/is (= {`x 7 `y 8 `z 6} (f1 7 8)))
  (sut/undefsc-lastdef)
  (t/is (= {`x 9 `y 10 `z 3} (f1 9 10)))
  (t/is (every? this-ns? (sut/defsc-last)))
  (t/is (= {`x 9 `y 10 `z 3} (f1 (eval `x) (eval `y))))

  (sut/undefsc-lastdef)

  (comment #_"NOTE: cleanup if something breaks"
    (ns-unmap *ns* 'x)
    (ns-unmap *ns* 'y)
    (ns-unmap *ns* 'z)
    (reset! sut/original-var-values {})))


(t/deftest undefsc-all-test
  (def this-ns (find-ns 'ps.sc-test))
  (defn this-ns? [v] (= this-ns (:ns (meta v))))

  (def z2 -3)
  (def y2 -2)

  (defn f1 [x2 y2 zz2]
    (sc.api/spy)
    (zipmap `[x2 y2 z2 zz2] [x2 y2 z2 zz2]))

  (defn f2 [x2 z2]
    (sc.api/spy)
    (zipmap `[x2 z2] [x2 z2]))

  (t/is (= {`x2 1 `y2 2 `z2 -3 `zz2 4} (f1 1 2 4)))
  (sut/defsc-last)
  (t/is (= 1 (eval `x2)))
  (t/is (= 2 (eval `y2)))
  (t/is (= -3 (eval `z2)))
  (t/is (= 4 (eval `zz2)))

  (t/is (= {`x2 5 `z2 6} (f2 5 6)))
  (sut/defsc-last)
  (t/is (= 5 (eval `x2)))
  (t/is (= 6 (eval `z2)))
  (t/is (= 2 (eval `y2)))
  (t/is (= 4 (eval `zz2)))

  (sut/undefsc-all)

  (t/is (= -3 (eval `z2)))
  (t/is (= -2 (eval `y2)))

  (t/is (undefined-sym? `zz2))
  (t/is (undefined-sym? `x2)))
