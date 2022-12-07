(defun dir-sum (tree)
  (loop for dir being the hash-keys of tree using (hash-value subdirs)
        with cache = (make-hash-table :test 'equal :size (hash-table-size tree))
        for size = (dir-sum-one tree dir subdirs cache)
        do (setf (gethash dir cache) size)
        when (<= size 100000)
          sum size))

(defun dir-min (tree)
  (let ((cache (make-hash-table :test 'equal :size (hash-table-size tree))))
    (loop for dir being the hash-keys of tree using (hash-value subdirs)
          for size = (dir-sum-one tree dir subdirs cache)
          do (setf (gethash dir cache) size))
    (let ((free-space (- 70000000 (gethash "/" cache))))
      (loop for size being the hash-values of cache
            when (>= (+ free-space size) 30000000)
              minimize size))))

(defun dir-sum-one (tree dir subdirs cache)
  (loop for (name . size) in subdirs
        when size
          sum size
        else
          sum (let ((full-name (concatenate 'string dir name "/")))
                (or (gethash full-name cache)
                    (let ((dir-sum (dir-sum-one tree full-name (gethash full-name tree) cache)))
                      (setf (gethash full-name cache) dir-sum)
                      dir-sum)))))

(defun parse-file ()
  (with-open-file (file "input.txt")
    (let ((pwd (list "/")))
    (loop for line = (read-line file nil)
          while line
          with tree = (make-hash-table :test 'equal)
          if (eq (aref line 0) #\$)
            do (cond
                 ((string= (subseq line 2 4) "cd")
                  (let ((dir (subseq line 5)))
                    (cond
                      ((string= dir "..") (pop pwd))
                      ((string= dir "/") (setf pwd (list "/")))
                      (t (setf pwd (cons (concatenate 'string (subseq line 5) "/") pwd))))))
                 ((string= (subseq line 2 4) "ls") nil)
                 (t nil))
          else
            do (let* ((space-pos (position #\Space line))
                      (name (subseq line (1+ space-pos)))
                      (size (parse-integer (subseq line 0 space-pos)
                                           :junk-allowed t))
                      (dir-name (apply #'concatenate 'string (reverse pwd))))
                 (push (cons name size) (gethash dir-name tree nil)))
          finally (return tree)))))

(defun part-1 ()
  (dir-sum (parse-file)))

(defun part-2 ()
  (dir-min (parse-file)))
