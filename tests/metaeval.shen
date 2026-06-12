\\ Fixture: genuinely requires runtime eval (needs-eval=true).
\\ Builds expressions as data - a list, a runtime define, a string -
\\ and evaluates them.  Each computes 42.

(define make-expr
  N -> [+ N [* N N]])

(output "eval list: ~A~%" (eval (make-expr 6)))

(eval [define triple X -> [* 3 X]])
(output "eval define: ~A~%" (eval [triple 14]))

(output "eval string: ~A~%" (eval (hd (read-from-string "(- (* 6 8) 6)"))))
