(defproject com.zensols.tools/nrepl-puget "0.1.0-SNAPSHOT"
  :description "Configure and use Puget in Cider."
  :url "https://github.com/plandes/clj-nrepl-puget"
  :license {:name "Apache License version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"
            :distribution :repo}
  :plugins [[lein-codox "0.10.1"]
            [org.clojars.cvillecsteele/lein-git-version "1.2.7"]]
  :codox {:metadata {:doc/format :markdown}
          :project {:name "Configure and use puget in Cider."}
          :output-path "target/doc/codox"
          :source-uri "https://github.com/plandes/clj-nrepl-puget/blob/v{version}/{filepath}#L{line}"}
  :git-version {:root-ns "zensols.nrpuget"
                :path "src/clojure/zensols/nrpuget"
                :version-cmd "git describe --match v*.* --abbrev=4 --dirty=-dirty"}
  :source-paths ["src/clojure"]
  :test-paths ["test" "test-resources"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:unchecked"]
  :jar-exclusions [#".gitignore"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [mvxcvi/puget "1.0.1"]]
  :profiles {:appassem {:aot :all}
             :snapshot {:git-version {:version-cmd "echo -snapshot"}}
             :dev
             {:exclusions [org.slf4j/slf4j-log4j12
                           log4j/log4j
                           ch.qos.logback/logback-classic]
              :dependencies [[org.apache.logging.log4j/log4j-core "2.7"]
                             [org.apache.logging.log4j/log4j-slf4j-impl "2.7"]
                             [org.apache.logging.log4j/log4j-1.2-api "2.7"]
                             [org.apache.logging.log4j/log4j-jcl "2.7"]
                             [org.apache.logging.log4j/log4j-jul "2.7"]]}})
