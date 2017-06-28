(ns ^{:doc "Configure and pprint using [puget](https://github.com/greglook/puget)."
      :author "Paul Landes"}
    zensols.nrpuget.core
  (:refer-clojure :exclude [println])
  (:require [clojure.pprint :refer [*print-right-margin*]])
  (:require [puget.printer :as puget :refer (with-options)]))

(defonce ^:private options-inst (atom nil))

(defn set-options!
  "Set puget options as documented in the [puget
  API](https://greglook.github.io/puget/api/puget.printer.html)."
  [options]
  (reset! options-inst options))

(defn option-assoc!
  "Update only a single key/value pair rather than having to set everything
  over again with [[set-options!]]."
  [key value]
  (swap! options-inst assoc key value))

(defn options
  "Get the options currently used by puget."
  []
  @options-inst)

(defn- printer-opts
  "Get merged parameters."
  []
  (->> (options)
       (puget/merge-options puget/*options*)))

(defn pprint
  "Use Puget to pretty print an object using options set with [[set-options!]]."
  [obj]
  (with-options (printer-opts)
    (puget/pprint obj)))

(defn println
  "Call [println](https://clojuredocs.org/clojure.core/println) with pretty
  printed data using [[pprint]]."
  [& more]
  (let [set-opts (options)]
   (with-options (merge {:width (or (:width-override set-opts)
                                    *print-right-margin*
                                    72)}
                        (or (:seq-limit-override set-opts)
                            *print-length*
                            {:seq-limit *print-length*}))
     (pprint more))))
