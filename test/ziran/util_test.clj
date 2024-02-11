(ns ziran.util-test
  (:require [clojure.test :refer :all]
            [ziran.util :refer :all])
  (:import [java.io StringReader]))

(def test-string
  "123\nqwe\n\nzzz")

(defn tr [str]
  (token-reader (StringReader. str)))

(deftest token-reader-tests

  (testing "Testing read-ch and unread-ch"
    (is (= \1
           (let [x (tr "123")
                 res (read-ch x)]
             res)))

    (is (= \2
           (let [x (tr "123")
                 _ (read-ch x)
                 res (read-ch x)]
             res)))

    (is (= \1
           (let [x (tr "123")
                 _ (read-ch x)
                 _ (unread-ch x 1)
                 res (read-ch x)]
             res)))

    (is (= \3 (let [x (tr "123")
                    _ (read-ch x)
                    _ (read-ch x)
                    _ (read-ch x)
                    _ (unread-ch x 1)
                    res (read-ch x)]
                res)))

    (is (= \1
           (let [x (tr "123")
                 _ (read-ch x)
                 _ (read-ch x)
                 _ (read-ch x)
                 _ (unread-ch x 999)
                 res (read-ch x)]
             res)))

    (is (= nil
           (let [x (tr "123")
                 _ (read-ch x)
                 _ (read-ch x)
                 _ (read-ch x)
                 res (read-ch x)]
             res)))

    (is (= \newline
           (let [x (tr "\n1")
                 res (read-ch x)]
             res)))

    (is (= \1
           (let [x (tr "\n1")
                 _ (read-ch x)
                 res (read-ch x)]
             res))))

  (testing "Testing collect-token"
    (is (= {:string     ""
            :len        0
            :pos-start  0
            :pos-end    0
            :line-start 0 `:line-end   0
            :col-start  0
            :col-end    0}
           (let [x (tr "123")
                 res (collect-token x)]
             res)))

    (is (= {:string     "1"
            :len        1
            :pos-start  0
            :pos-end    1
            :line-start 0
            :line-end   0
            :col-start  0
            :col-end    1}
           (let [x (tr "123")
                 _ (read-ch x)
                 res (collect-token x)]
             res)))

    (is (= {:string     ""
            :len        0
            :pos-start  0
            :pos-end    0
            :line-start 0
            :line-end   0
            :col-start  0
            :col-end    0}
           (let [x (tr "123")
                 _ (read-ch x)
                 _ (unread-ch x 1)
                 res (collect-token x)]
             res)))

    (is (= [{:string     "12"
             :len        2
             :pos-start  0
             :pos-end    2
             :line-start 0
             :line-end   0
             :col-start  0
             :col-end    2}
            {:string     "34"
             :len        2
             :pos-start  2
             :pos-end    4
             :line-start 0
             :line-end   0
             :col-start  2
             :col-end    4}]
           (let [x (tr "1234")
                 _ (read-ch x)
                 _ (read-ch x)
                 res1 (collect-token x)
                 _ (read-ch x)
                 _ (read-ch x)
                 res2 (collect-token x)]
             [res1 res2])))

    (testing "Testing more ops with nils"
      (is (= \1
             (let [x (tr "123\n123\n\n123")]
               (dotimes [n 11]
                 (read-ch x))
               (reset-token x)
               (read-ch x))))

      (let [x     (tr "123\nqwe\n\nasd")
            _     (read-ch x)
            _     (read-ch x)
            _     (reset-token x)
            ch1   (read-ch x)
            _     (is (= \1 ch1))
            _     (read-ch x)
            _     (read-ch x)
            _     (read-ch x)
            res1  (collect-token x)
            _     (is (= {:string "123\n"
                          :len 4
                          :pos-start 0
                          :pos-end 4
                          :line-start 0
                          :line-end 1
                          :col-start 0
                          :col-end 0}
                         res1))
            _     (read-ch x)
            _     (read-ch x)
            _     (reset-token x)
            ch2   (read-ch x)
            _     (is (= \q
                         ch2))
            _     (dotimes [n 5]
                    (read-ch x))
            res2  (collect-token x)
            _     (is (= {:string "qwe\n\na"
                          :len 6
                          :pos-start 4
                          :pos-end 10
                          :line-start 1
                          :line-end 3
                          :col-start 0
                          :col-end 1}
                         res2))]))))
