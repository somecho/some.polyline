(defproject org.clojars.some/polyline "0.0.2"
  :description "Helper functions for dealing with 2D polylines. A polyline is
  just a vector of 2D (x y) vectors."
  :url ""
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojars.some/vec "0.0.1"]]
  :repositories
  {"clojars" {:url "https://clojars.org/repo"
              :sign-releases false}}
  :source-paths ["src"])
