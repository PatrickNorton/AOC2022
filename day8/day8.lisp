(defun parse-file ()
  (with-open-file (file "input.txt")
    (let ((parsed-file (loop for line = (read-line file nil)
                             while line
                             collect (map 'list #'digit-char-p line))))
      (make-array (list (length parsed-file) (length (car parsed-file)))
                        :initial-contents parsed-file))))

(defun visible-top (trees already-seen)
  (loop with (a b) = (array-dimensions trees)
        for i below b
        sum (loop with height = -1
                  for j below a
                  for tree = (aref trees i j)
                  when (> tree height)
                    do (setf height tree)
                    and unless (gethash (cons i j) already-seen)
                          do (setf (gethash (cons i j) already-seen) tree)
                          and sum 1)))

(defun visible-bottom (trees already-seen)
  (loop with (a b) = (array-dimensions trees)
        for i below b
        sum (loop with height = -1
                  for j downfrom (1- a) to 0
                  for tree = (aref trees i j)
                  when (> tree height)
                    do (setf height tree)
                    and unless (gethash (cons i j) already-seen)
                          do (setf (gethash (cons i j) already-seen) tree)
                          and sum 1)))

(defun visible-left (trees already-seen)
  (loop with (a b) = (array-dimensions trees)
        for j below a
        sum (loop with height = -1
                  for i below b
                  for tree = (aref trees i j)
                  when (> tree height)
                    do (setf height tree)
                    and unless (gethash (cons i j) already-seen)
                          do (setf (gethash (cons i j) already-seen) tree)
                          and sum 1)))

(defun visible-right (trees already-seen)
  (loop with (a b) = (array-dimensions trees)
        for j below a
        sum (loop with height = -1
                  for i downfrom (1- b) to 0
                  for tree = (aref trees i j)
                  when (> tree height)
                    do (setf height tree)
                    and unless (gethash (cons i j) already-seen)
                          do (setf (gethash (cons i j) already-seen) tree)
                          and sum 1)))

(defun senic-score-down (trees a j)
  (loop with height = (aref trees a j)
        for i from (1+ a) below (array-dimension trees 0)
        count i
        while (< (aref trees i j) height)))

(defun senic-score-up (trees a j)
  (loop with height = (aref trees a j)
        for i downfrom (1- a) to 0
        count i
        while (< (aref trees i j) height)))

(defun senic-score-right (trees i b)
  (loop with height = (aref trees i b)
        for j from (1+ b) below (array-dimension trees 1)
        count j
        while (< (aref trees i j) height)))

(defun senic-score-left (trees i b)
  (loop with height = (aref trees i b)
        for j downfrom (1- b) to 0
        count j
        while (< (aref trees i j) height)))

(defun senic-score (trees i j)
  (* (senic-score-down trees i j)
     (senic-score-up trees i j)
     (senic-score-right trees i j)
     (senic-score-left trees i j)))

(defun part-1 ()
  (let ((trees (parse-file))
        (already-seen (make-hash-table :test 'equal)))
    (+ (visible-top trees already-seen)
       (visible-bottom trees already-seen)
       (visible-left trees already-seen)
       (visible-right trees already-seen))))

(defun part-2 ()
  (let ((trees (parse-file)))
    (loop with (a b) = (array-dimensions trees)
          for i below a
          maximize (loop for j below b
                         maximize (senic-score trees i j)))))
