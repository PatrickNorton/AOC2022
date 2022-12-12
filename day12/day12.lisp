(defun char-to-height (char)
  (- (char-int char) (char-int #\a)))

(defun parse-input ()
  (with-open-file (file "input.txt")
    (let ((input (loop for line = (read-line file nil)
                       while line
                       collect line)))
      (let ((array (make-array (list (length input) (length (first input)))))
            (start nil)
            (end nil))
        (loop for line in input
              for i from 0 below (length input)
              do (loop for char in (coerce line 'list)
                       for j from 0 below (length line)
                       do (progn
                            (when (char= #\S char)
                              (setf start (cons i j)
                                    char #\a))
                            (when (char= #\E char)
                              (setf end (cons i j)
                                    char #\z))
                            (setf (aref array i j) (char-to-height char)))))
        (values array start end)))))

(defun neighbours (map location)
  "Returns neighbours in map with at max +1 height"
  (destructuring-bind ((x . y) (xmax ymax)) (list location (array-dimensions map))
    (loop for dx in '(-1 0 1 0)
      for dy in '(0 1 0 -1)
      for xx = (+ x dx)
      for yy = (+ y dy)
      if (and (<= 0 xx (1- xmax))
              (<= 0 yy (1- ymax))
              (<= (aref map xx yy) (1+ (aref map x y))))
        collect (cons xx yy))))

;; simple queue definition
;; backed by a list. represented by 2 pointers: first cons cell, last cons cell.
(defun make-queue () (list nil nil))

(defun pushq (q elem)
  ;; push it & update the last cons cell.
  (setf (cdr q)
    (setf (cddr q) (cons elem nil)))
  (when (emptyq q)
    ;; fix the head pointer.
    (setf (car q) (cdr q)))
  q)

(defun popq (q)
  (prog1 (caar q)
    (setf (car q) (cdar q))))

(defun emptyq (q) (null (car q)))

(defun find-shortest-path (map start end)
  (let ((queue (make-queue))
        (visited (make-hash-table :test #'equal)))
    (pushq queue (cons start 0))
    (setf (gethash start visited) t)
    (loop while (not (emptyq queue))
          for (current . score) = (popq queue)
          if (equal current end)
            return score
          else do (loop for n in (neighbours map current)
                        unless (gethash n visited)
                          do (setf (gethash n visited) t)
                             (pushq queue (cons n (1+ score)))))))

(defun part-1 ()
  (multiple-value-bind (map start end) (parse-input)
    (find-shortest-path map start end)))

(defun starting-points-list (map)
  (loop for i from 0 below (array-dimension map 0)
    nconc (loop for j from 0 below (array-dimension map 1)
            if (eql 0 (aref map i j))
              collect (cons i j))))

(defun part-2 ()
  (multiple-value-bind (map _ end) (parse-input)
    (apply #'min
       (remove nil
           (loop for start in (starting-points-list map)
             collect (find-shortest-path map start end))))))
