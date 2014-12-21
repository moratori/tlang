

(load "inner.lisp")



(defmacro %deflexer% (name &rest clauses)
  `(define-string-lexer ,name 
        ,@(mapcar (lambda (clause)
                    (destructuring-bind (pattern kind) clause
                      `(,pattern (return (values ,kind $@))))) clauses)))

(defmacro defparser (name defterm &rest body)
  (let ((lexer (gensym))
        (parser (gensym)))
    `(progn 
       (%deflexer% ,lexer ,@defterm)
       (define-parser ,parser 
          (:terminals
            ,(mapcar #'second defterm))            
          ,@body)
       (defun ,name (target)
         (parse-with-lexer (,lexer target) ,parser)))))


(defun bin (a b sym)
  ($pcall 
    :callee sym
    :exprs 
    ($exprseq :container (list a b))))


(defparser lang
  (("[0-9]+"      :integer)
   ("true|false"  :boolean)
   ("->"          :arrow)
   ("def"         :def)
   ("\\+"         :plus)
   ("\\*"         :mult)
   ("%"           :mod)
   ("="           :equal)
   ("-"           :minus)
   ("and"         :and)
   ("or"          :or)
   ("print"       :print)
   ("not"         :not)
   ("if"          :if)
   ("Int"         :tint)
   ("Bool"        :tbool)
   (":"           :colon)
   (","           :comma)
   ("\\("         :sparen)
   ("\\)"         :eparen)
   ("\\["         :sbracket)
   ("\\]"         :ebracket)
   ("[a-zA-Z]+"   :symbol))         

  (:start-symbol program)
  (:precedence 
    ((:left :mult )
     (:left :plus :minus :and :or)
     (:left :mod)
     (:left :equal)
     (:left :if)
     (:left :arrow)
     (:right :not)
     (:left :print)))

  (program 
    (expr 
      (lambda (x)
        ($program 
          :container (list x))))
    (program expr
      (lambda (x y)
        (setf ($program.container x)
              (append ($program.container x) (list y)))
        x)))

  (expr
    (:integer 
      (lambda (x)
        ($integer :value x)))
    (:boolean
      (lambda (x)
        ($boolean :value x)))
    (:sparen expr :eparen
      (lambda (a b c) 
        (declare (ignore a c)) b)) 
    (:not expr
      (lambda (a b)
        (declare (ignore a))
        ($pcall 
          :callee "not"
          :exprs ($exprseq :container (list b)))))
    (expr :equal expr
       (lambda (a b c)
         (declare (ignore b))
         (bin a c "=")))
    (expr :plus expr
       (lambda (a b c)
         (declare (ignore b))
         (bin a c "+")))
    (expr :mod expr
       (lambda (a b c)
         (declare (ignore b))
         (bin a c "mod")))
    (expr :mult expr
       (lambda (a b c)
         (declare (ignore b))
         (bin a c "*")))
    (expr :minus expr
       (lambda (a b c)
         (declare (ignore b))
         (bin a c "-")) )
    (expr :and expr
       (lambda (a b c)
         (declare (ignore b))
         (bin a c "and")))
    (expr :or expr 
          (lambda (a b c)
            (declare (ignore b))
            (bin a c "or")))
    (:if expr expr expr
       (lambda (a b c d)
         (declare (ignore a))
         ($special 
           :ident "if"
           :exprs ($exprseq :container (list b c d)))))
    (:print expr
       (lambda (a b)
         (declare (ignore a))
         ($special 
           :ident "print" 
           :exprs ($exprseq :container (list b)))))
    (expr :sbracket expr :ebracket
       (lambda (a b c d)
         (declare (ignore b d))
         ($lcall 
           :callee a
           :expr c)))
    (:def :symbol function 
       (lambda (a b c)
         (declare (ignore a))
        ($defunc :name b :lambdaform c)))
    function
    variable)

  (function 
   (:sparen variable :colon type :eparen :colon type :arrow expr
     (lambda (a b c d e f g h i)
       (declare (ignore a c e f h))
       ($function 
         :typedvar ($typedvar :var b :typing d)
         :typing g
         :expr i))))
  (variable
    (:symbol 
      (lambda (a) 
        ($var :value a))))
  (type 
    (:tint
      (lambda (x) 
        (declare (ignore x))
        ($tint)))
    (:tbool
	(lambda (x) (declare (ignore x)) ($tbool)))
    (type :arrow type
	(lambda (a b c) 
		(declare (ignore b))
 		($tfunc :domain a :range c)))
    (:sparen type :eparen
     (lambda (a b c)
       (declare (ignore a c)) b))))

