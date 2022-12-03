(defun priority (chr)
  (cond
    ((<= (char-code #\a) (char-code chr) (char-code #\z))
     (- (char-code chr) (char-code #\`)))
    ((<= (char-code #\A) (char-code chr) (char-code #\Z))
     (+ 27 (- (char-code chr) (char-code #\A))))))

(defun uniquify (str)
  (let ((result nil))
    (map nil (lambda (x) (pushnew x result)) str)
    result))

(defun part-1 ()
  (with-open-file (file "input.txt")
    (loop for line = (read-line file nil)
          while line
          for front = (subseq line 0 (/ (length line) 2))
          for back = (subseq line (/ (length line) 2))
          sum (apply #'+ (map 'list (lambda (a)
                                      (if (find a back) (priority a) 0))
                              (uniquify front))))))

(defun part-2 ()
  (with-open-file (file "input.txt")
    (loop for line-a = (read-line file nil)
          and line-b = (read-line file nil)
          and line-c = (read-line file nil)
          while (and line-a line-b line-c)
          sum (priority
               (find-if (lambda (a)
                          (and (find a line-b) (find a line-c)))
                        line-a)))))
