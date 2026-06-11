(DEFUN vector (N) 
  (LET* ((Vector (MAKE-ARRAY (LIST N) :INITIAL-ELEMENT 'shen.fail!)))
      (SETF (SVREF Vector 0) N)))   