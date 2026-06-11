"Copyright (c) 2010-2021, Mark Tarver

   3 clause BSD; see license"

(DEFMACRO trap-error (X F) 
`(HANDLER-CASE ,X (ERROR (Condition) (FUNCALL ,F Condition))))

(DEFUN pos (X N) (trap-error (COERCE (LIST (CHAR X N)) 'STRING)
                       (LAMBDA (E)
                         (IF (NOT (STRINGP X)) 
                             (ERROR "~A is not a string~%" X)
                             (ERROR "~A is not a natural number less than the length of the string~%" 
                                    N)))))
 