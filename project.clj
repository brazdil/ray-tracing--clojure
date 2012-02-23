(defproject ray-tracing "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :aot [#".*"] 
  :java-source-path "java/src/"
  :repl-init ray-tracing.main)
