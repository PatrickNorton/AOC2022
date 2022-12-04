(defun parse-line (line)
  (loop with start = 0
        while (< start (length line))
        for int = (multiple-value-bind (int next) (parse-integer line :start start :junk-allowed t)
                    (setf start (1+ next))
                    int)
        while int
        collect int))

(defun range-contains (a-start a-end b-start b-end)
  (and (<= a-start b-start) (>= a-end b-end)))

(defun part-1 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for (a-start a-end b-start b-end) = (parse-line line)
          count (or (range-contains a-start a-end b-start b-end)
                    (range-contains b-start b-end a-start a-end)))))

(defun range-overlaps (a-start a-end b-start b-end)
  (if (<= a-start b-start)
      (>= a-end b-start)
      (>= b-end a-start)))

(defun part-2 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for (a-start a-end b-start b-end) = (parse-line line)
          count (range-overlaps a-start a-end b-start b-end))))
