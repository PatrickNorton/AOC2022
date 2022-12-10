(defun part-1 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for parsed = (read-from-string (concatenate 'string "(" line ")"))
          with strength = 1
          with cycle = 0
          with answer = 0
          do (case (car parsed)
               (noop
                (incf cycle)
                (when (= 20 (mod cycle 40))
                  (incf answer (* cycle strength))))
               (addx
                (incf cycle 2)
                (when (or (= 20 (mod cycle 40))
                          (= 21 (mod cycle 40)))
                  (incf answer (* (if (= 20 (mod cycle 40)) cycle (1- cycle))
                                  strength)))
                (incf strength (nth 1 parsed))))
          finally (return answer))))

(defun high-mod (number divisor)
  (let ((true-mod (mod number divisor)))
    (if (zerop true-mod) divisor true-mod)))

(defun print-arr (value)
  (loop for i from 0 below (array-dimension value 0)
        do (loop for j from 0 below (array-dimension value 1)
                 do (princ (aref value i j)))
           (terpri)))

(defun part-2 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for parsed = (read-from-string (concatenate 'string "(" line ")"))
          with cycle = 0
          with x-reg = 1
          do (flet ((tick ()
                      (princ (if (<= -1 (- x-reg cycle) 1) #\# #\.))
                      (incf cycle)
                      (when (= cycle 40)
                        (setf cycle 0)
                        (terpri))))
               (case (car parsed)
                 (noop (tick))
                 (addx (tick) (tick) (incf x-reg (nth 1 parsed))))))))
