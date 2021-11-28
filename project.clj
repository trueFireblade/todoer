(defproject todoer "1.0.0"
  :description "Returns the todos and their prioritys"
  :url "https://github.com/trueFireblade/todoer.git"
  :plugins [[cider/cider-nrepl "0.24.0"]]
  :license {:name "MIT"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot todoer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
