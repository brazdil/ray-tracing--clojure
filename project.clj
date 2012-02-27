(defproject ray-tracing "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.0"]
  				 [org.clojure/clojure-contrib "1.2.0"]
                 [lamina "0.4.0"]]
  :aot [#".*"] 
  :java-source-path "java/"
  :repl-init ray-tracing.main

  :profiles { :dev {:warn-on-reflection true} })
