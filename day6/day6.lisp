(defun pairwise-uneq (&rest args)
  (loop with vals = (make-hash-table :size (length args) :test #'eql)
        for x in args
        never (gethash x vals)
        do (setf (gethash x vals) t)))

(defun part-1 ()
  (with-open-file (file "input.txt")
    (loop with file-str = (coerce (read-line file nil) 'list)
          for (a b c d) on file-str
          for i from 4
          when (pairwise-uneq a b c d)
            return i)))

(defun part-2 ()
  (with-open-file (file "input.txt")
    (loop with file-str = (coerce (read-line file nil) 'list)
          for (a b c d e f g h i j k l m n) on file-str
          for idx from 14
          when (pairwise-uneq a b c d e f g h i j k l m n)
            return idx)))
