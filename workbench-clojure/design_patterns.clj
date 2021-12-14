;; http://mishadoff.com/blog/clojure-design-patterns/#episode-1-command

;; Command Pattern

(defn mysym [ & args]
  (reduce + args))


(defn execute [command & args]
  (apply command args))

(execute mysym 1 2 3 4)

;;