(defun read-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil) while line
          unless (equal line "")
            collect (parse-line line))))

(defun parse-line (line)
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (set-macro-character #\[ #'|[-reader|)
    (set-syntax-from-char #\] #\))
    (read-from-string line)))

(defun |[-reader| (stream char)
  (declare (ignore char))
  (read-delimited-list #\] stream))

(defun correct-order (a b)
  (cond
    ((and (numberp a) (numberp b))
     (cond
       ((= a b) -1)
       (t (< a b))))
    ((and (listp a) (listp b))
     (let ((a-len (length a))
           (b-len (length b)))
       (loop for i in a
             for j in b
             for ord = (correct-order i j)
             unless (equal ord -1)
               return ord
             finally
                (return (cond
                          ((< a-len b-len) t)
                          ((> a-len b-len) nil)
                          ((= a-len b-len) -1))))))
    ((and (listp a) (numberp b))
     (correct-order a (list b)))
    ((and (numberp a) (listp b))
     (correct-order (list a) b))
    (t (error "Incorrect values"))))

(defun part-1 ()
  (let ((lines (read-file "input.txt")))
    (loop for i from 1
          for (first second) on lines by #'cddr while second
          for order = (correct-order first second)
          do (assert (not (equal order -1)))
          when order
            sum i)))

(defun part-2 ()
  (let* ((lines (read-file "input.txt"))
         (dividers '(((2)) ((6))))
         (proper-lines (concatenate 'list dividers lines))
         (sorted (sort proper-lines #'correct-order)))
      (apply
       #'* (mapcar (lambda (div) (1+ (position div sorted :test #'equal)))
                   dividers))))
