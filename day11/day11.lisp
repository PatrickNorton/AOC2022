(ql:quickload :cl-ppcre)

(defun parse-monkey-number (line)
  (cl-ppcre:register-groups-bind
      ((#'parse-integer number))
      ("Monkey (\\d+):" line)
    number))

(defun parse-monkey-items (line)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" line)))

(defun parse-monkey-operation (line)
  (cl-ppcre:register-groups-bind
      (lhs op rhs)
      ("Operation: new = (old|\\d+) (\\+|\\*) (old\|\\d+)" line)
    (let ((lhs (parse-integer lhs :junk-allowed t))
          (rhs (parse-integer rhs :junk-allowed t))
          (op (read-from-string op)))
      (lambda (old)
        (funcall op (or lhs old) (or rhs old))))))

(defun parse-monkey-test (line)
  (cl-ppcre:register-groups-bind
      ((#'parse-integer number))
      ("Test: divisible by (\\d+)" line)
    number))

(defun parse-monkey-throw (line)
  (cl-ppcre:register-groups-bind
      (_ (#'parse-integer number))
      ("If (true|false): throw to monkey (\\d+)" line)
    number))

(defun parse-monkeys (input)
  (loop for (n items op test next-true next-false) on input by (lambda (l) (nthcdr 7 l))
        while (and n items op test next-true next-false)
        collect (list (parse-monkey-number n)
                      (parse-monkey-items items)
                      (parse-monkey-operation op)
                      (parse-monkey-test test)
                      (parse-monkey-throw next-true)
                      (parse-monkey-throw next-false))))

(defun simulate (monkeys &optional (rounds 20))
  (let* ((count (length monkeys))
         (items (make-array count :initial-contents (copy-tree (mapcar #'second monkeys))))
         (ops (make-array count :initial-contents (mapcar #'third monkeys)))
         (tests (make-array count :initial-contents (mapcar #'fourth monkeys)))
         (true-throws (make-array count :initial-contents (mapcar #'fifth monkeys)))
         (false-throws (make-array count :initial-contents (mapcar #'sixth monkeys))))
    (loop repeat rounds
          with monkey-count = (make-hash-table)
          do (loop for monkey from 0 below (length monkeys)
                   for item = (aref items monkey)
                   for op = (aref ops monkey)
                   for test = (aref tests monkey)
                   for true = (aref true-throws monkey)
                   for false = (aref false-throws monkey)
                   do (loop for it in item
                            do (incf (gethash monkey monkey-count 0))
                            do (setf it (funcall op it))
                               (setf it (floor it 3))
                               (if (zerop (mod it test))
                                   (setf (aref items true)
                                         (append (aref items true) (cons it nil)))
                                   (setf (aref items false)
                                         (append (aref items false) (cons it nil))))
                            finally (setf (aref items monkey) nil)))
          finally (return monkey-count))))

(defun monkey-business-level (counts)
  (reduce #'* (subseq (sort (loop for v being the hash-values of counts
                                  collect v)
                            #'>)
                      0 2)))

(defun part-1 () (monkey-business-level (simulate (parse-monkeys (read-file)))))

(defun read-file ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          collect line)))

(defun monkey-mod (tests)
  (apply #'* tests))

(defun simulate-worried (monkeys &optional (rounds 10000))
  (let* ((count (length monkeys))
         (items (make-array count :initial-contents (copy-tree (mapcar #'second monkeys))))
         (ops (make-array count :initial-contents (mapcar #'third monkeys)))
         (tests (make-array count :initial-contents (mapcar #'fourth monkeys)))
         (mod (monkey-mod tests))
         (true-throws (make-array count :initial-contents (mapcar #'fifth monkeys)))
         (false-throws (make-array count :initial-contents (mapcar #'sixth monkeys))))
    (loop repeat rounds
          with monkey-count = (make-hash-table)
          do (loop for monkey from 0 below (length monkeys)
                   for item = (aref items monkey)
                   for op = (aref ops monkey)
                   for test = (aref tests monkey)
                   for true = (aref true-throws monkey)
                   for false = (aref false-throws monkey)
                   do (loop for it in item
                            do (incf (gethash monkey monkey-count 0))
                            do (setf it (mod (funcall op it) mod))
                               (if (zerop (mod it test))
                                   (setf (aref items true)
                                         (append (aref items true) (cons it nil)))
                                   (setf (aref items false)
                                         (append (aref items false) (cons it nil))))
                            finally (setf (aref items monkey) nil)))
          finally (return monkey-count))))

(defun part-2 () (monkey-business-level (simulate-worried (parse-monkeys (read-file)))))
