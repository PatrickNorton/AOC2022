(defvar stacks
  '((N H S J F W T D)
    (G B N T Q P R H)
    (V Q L)
    (Q R W S B N)
    (B M V T F D N)
    (R T H V B D M)
    (J Q B D)
    (Q H Z R V J N D)
    (S M H N B)))

(defun parse-file ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          until (= (length line) 0))
    (loop for line = (read-line file nil)
          while line
          collect (read-from-string (concatenate 'string "(" line ")")))))

(defun part-1 ()
  (let ((stacks (copy-tree stacks))
        (moves (parse-file)))
    (loop for (_x count _y from _z to) in moves
          do (dotimes (_ count)
               (push (pop (nth (1- from) stacks))
                     (nth (1- to) stacks))))
    (mapcar #'car stacks)))

(defun part-2 ()
  (let ((stacks (copy-tree stacks))
        (moves (parse-file)))
    (loop for (_x count _y from _z to) in moves
          do (let ((popped (subseq (nth (1- from) stacks) 0 count)))
               (setf (nth (1- from) stacks)
                     (subseq (nth (1- from) stacks) count))
               (setf (nth (1- to) stacks)
                     (append popped (nth (1- to) stacks)))))
    (mapcar #'car stacks)))
