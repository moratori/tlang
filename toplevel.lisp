
(ql:quickload :yacc)
(ql:quickload :cl-lex)

(use-package :yacc)
(use-package :cl-lex)

(defun muffle (proc)
  (handler-bind
    ((warning (lambda (x) (declare (ignore x)) (muffle-warning))))
    (funcall proc)))


(muffle 
  (lambda () (load "grammer.lisp")))
 

(defun prompt () 
  (format t ">>> ") 
  (force-output *standard-output*))

(defun handling (proc)
  (handler-case
    (funcall proc)
           (yacc-parse-error (c) (declare (ignore c))
            (format t "parse error occurred~%")
            )
          ($undefined-variable (c) (declare (ignore c))
            (format t "reference of undefined variable~%")                   
            )
          ($type-error (c) (declare (ignore c))
            (format t "type error occurred~%")                   
            )
          ($undefined-form (c) (declare (ignore c))
            (format t "undefined form~%")                   
            )
          ($undefined-function-call (c) (declare (ignore c))
            (format t "undefined function call~%")                   
            )
          (file-error (c) (declare (ignore c))
            (format  t "file error occurred"))
          (error (c) (print c) 
            (format t "unexpected error occurred~%"))))

(defun convert (val)
  (typecase val
    (number val)
    (boolean (if val "true" "false"))
    (t (if (typep val 'function) "function" val))))

(defun evaluation (source tenv deb)
  (let* ((obj (lang source)))
    (multiple-value-bind
      (typ tenv) (typecheck obj tenv)
      (let* ((sexpr (->sexpr obj))
            (val (eval (if deb (print sexpr) sexpr))))
        (values 
          (convert val)
          typ
          tenv)))))

(defun repl ()
  (prompt)
  (loop 
        with flag = nil
        with old = nil
        for in = (read-line *standard-input* nil nil)
        while in
        if (string/= "" in)
        do 
        (handling 
          (lambda ()
            (cond 
            ((string= in ":debug")
             (setf flag (not flag)))
            (t
             (multiple-value-bind 
              (val typ new) (muffle (lambda () (evaluation in old flag)))
              (setf old new)
              (format *standard-output* "~A : ~A~%" val typ) )))))
        (prompt)
        else do (prompt))
  (sb-ext:exit))


(defun read-file (name)
  (with-open-file (in name)
    (loop with result = ""
          finally (Return result)
          for line = (read-line in nil nil)
          while line
          do (setf result (format nil "~A~%~A" result line)))))

(defun write-file (name contents)
  (with-open-file (out name :direction :output :if-exists :supersede)
    (print contents out)))

(defun main ()
  (let ((argv sb-ext:*posix-argv*))
    (if (< (length argv) 2)
      (repl)
      (destructuring-bind 
        (self src . more) argv
        (declare (ignore self))
        (handling
          (lambda ()
            (let* ((source (read-file src))
                   (obj (lang source)))
              (typecheck obj nil)
              (if more
                (write-file (car more) (->sexpr obj))
                (write-file "out.lisp" (->sexpr obj))))))
        (sb-ext:exit)))))


(main)



