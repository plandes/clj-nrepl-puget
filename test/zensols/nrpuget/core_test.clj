(ns zensols.nrpuget.core-test
  (:refer-clojure :exclude [println])
  (:require [clojure.test :refer :all])
  (:require [zensols.nrpuget.core :refer :all]))

(defn- init-options []
  (->> {:print-color true
        :color-scheme {:keyword [:none]
                       :delimiter [:blue]
                       :number [:magenta]
                       :tag [:black]
                       :symbol [:red]
                       :string [:green]
                       :character [:bold :green]
                       :boolean [:black]
                       :nil [:black]}}
       set-options!))

(deftest options-not-set-test
  (testing "options"
    (set-options! nil)
    (is (= "[#{\"set\" hash} {:a 1, :b 2} \"string\" symbol]\n"
           (with-out-str
             (pprint [#{'hash "set"} {:a 1 :b 2} "string" 'symbol]))))))

(deftest options-set-test
  (testing "options"
    (init-options)
    (is (= "[34m[[0m[34m#{[0m[32m\"set\"[0m [31mhash[0m[34m}[0m [34m{[0m[0m:a[0m [35m1[0m, [0m:b[0m [35m2[0m[34m}[0m [32m\"string\"[0m [31msymbol[0m[34m][0m\n")
        (with-out-str
          (pprint [#{'hash "set"} {:a 1 :b 2} "string" 'symbol])))))
