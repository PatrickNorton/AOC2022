(defun part-1 ()
  (car (sort-calories)))

(defun part-2 ()
  (apply #'+ (subseq (sort-calories) 0 3)))

(defun sort-calories ()
  (with-open-file (file "input.txt")
    (sort (loop with current = 0
                for line = (read-line file nil)
                while line
                if (= (length line) 0)
                  collect current
                  and do (setf current 0)
                else do (incf current (parse-integer line)))
          #'>)))
