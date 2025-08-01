(defmacro hi [] '(reverse "hello"))

(macroexpand '(hi))
