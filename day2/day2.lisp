(defun char-to-type (char)
  (cond
    ((or (eq char #\A) (eq char #\X)) 0)
    ((or (eq char #\B) (eq char #\Y)) 1)
    ((or (eq char #\C) (eq char #\Z)) 2)))

(defun score (opponent self)
  (cond
    ((= opponent self) (+ 3 (1+ self))) ;; draw
    ((= self (mod (1+ opponent) 3)) (+ 6 (1+ self))) ;; win
    (t (+ 0 (1+ self))))) ;; loss

(defun part-1 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for opponent = (aref line 0)
          and self = (aref line 2)
          sum (score (char-to-type opponent) (char-to-type self)))))

(defun result-score (opponent result)
  (cond
    ((eq result #\Y) (+ 3 (1+ opponent))) ;; draw
    ((eq result #\X) (+ 0 (1+ (mod (1- opponent) 3)))) ;; loss
    ((eq result #\Z) (+ 6 (1+ (mod (1+ opponent) 3)))))) ;; win

(defun part-2 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for opponent = (char-to-type (aref line 0))
          and result = (aref line 2)
          sum (result-score opponent result))))
