(ns ^{:doc "Configure and pprint using [puget](https://github.com/greglook/puget)."
      :author "Paul Landes"}
    zensols.nrpuget.core
  (:refer-clojure :exclude [println])
  (:require [clojure.pprint :refer [*print-right-margin*]])
  (:require [puget.printer :as puget :refer (with-options)]))

(defonce ^:private options-inst (atom nil))
(defonce ^:private options-merged-inst (atom nil))

(defn set-options!
  "Set puget options as documented in the [puget
  API](https://greglook.github.io/puget/api/puget.printer.html)."
  [options]
  (reset! options-merged-inst nil)
  (reset! options-inst options))

(defn- merge-printer-opts
  "Create optimized (merged) parameters."
  []
  (->> (merge @options-inst
              {:width (or *print-right-margin* 72)}
              (if *print-length* {:seq-limit *print-length*}))
       (puget/merge-options puget/*options*)))

(defn- printer-opts
  "Get merged parameters."
  []
  (swap! options-merged-inst #(or % (merge-printer-opts))))

(defn pprint
  "Use Puget to pretty print an object using options set with [[set-options!]]."
  [obj]
  (with-options (printer-opts)
    (puget/pprint obj)))

(defn println
  "Call [println](https://clojuredocs.org/clojure.core/println) with pretty
  printed data using [[pprint]]."
  [& more]
  (pprint more))
