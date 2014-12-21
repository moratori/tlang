


(define-condition uninitialized-error () ())
(define-condition $undefined-variable () ())
(define-condition $type-error () ())
(define-condition $undefined-form () ())
(define-condition $undefined-function-call () ())


(defun uninit_error ()
  (error (make-condition '$uninitialized-error)))

(defun undefvar_error ()
  (error (make-condition '$undefined-variable)))

(defun undefform_error ()
  (error (make-condition '$undefined-form)))

(defun undefcall-error ()
  (error (make-condition '$undefined-function-call)))

(defun type_error ()
  (error (make-condition '$type-error)))

(defstruct $type)

(defstruct ($tint (:constructor $tint)
                  (:include $type)))

(defstruct ($tbool (:constructor $tbool)
                   (:include $type)))

(defstruct ($tfunc (:constructor $tfunc)
                   (:conc-name $tfunc.)
                   (:include $type))
  (domain (uninit_error) :type $type)
  (range  (uninit_error) :type $type))

(defstruct $expr)

(defstruct ($var  (:constructor $var)
                  (:include $expr)
                  (:conc-name $var.))
  (value (uninit_error) :type string))

(defstruct ($typedvar (:constructor $typedvar)
                      (:conc-name $typedvar.))
  (var    (uninit_error) :type $var)
  (typing (uninit_error) :type $type)) 

(defstruct ($exprseq  (:constructor $exprseq)
                     (:conc-name $exprseq.))
  (container nil :type list))

(defstruct ($program (:constructor $program)
                     (:conc-name $program.))
  (container nil :type list))

(defstruct ($integer (:constructor $integer)
                     (:include $expr)
                     (:conc-name $integer.))
  (value (uninit_error) :type string))

(defstruct ($boolean (:constructor $boolean)
                     (:include $expr)
                     (:conc-name $boolean.))
  (value (uninit_error) :type string))

(defstruct ($primitive-call 
                  (:constructor $pcall)
                  (:include $expr)
                  (:conc-name $pcall.))
  (callee (uninit_error) :type string)
  (exprs  (uninit_error) :type $exprseq))

(defstruct ($lambda-call 
                  (:constructor $lcall)
                  (:include $expr)
                  (:conc-name $lcall.))
  (callee (uninit_error))
  (expr  (uninit_error) :type $expr))

(defstruct ($special (:constructor $special)
                     (:include $expr)
                     (:conc-name $special.))
  (ident (uninit_error))
  (exprs (uninit_error) :type $exprseq))

(defstruct ($function (:constructor $function)
                      (:include $expr)
                      (:conc-name $function.)) 
  (typedvar (uninit_error) :type $typedvar)
  (typing (uninit_error) :type $type)
  (expr   (uninit_error) :type $expr))

(defstruct ($defunc   (:constructor $defunc)
                      (:conc-name $defunc.))
  (name "" :type string)
  (lambdaform (uninit_error) :type $function))




(defvar *primitive-operator-type* 
  (list 
    (list "+"   (list ($tint) ($tint)) ($tint))
    (list "-"   (list ($tint) ($tint)) ($tint))
    (list "*"   (list ($tint) ($tint)) ($tint))
    (list "mod" (list ($tint) ($tint)) ($tint))
    (list "="   (list ($tint) ($tint)) ($tbool))
    (list "or"  (list ($tbool) ($tbool)) ($tbool))
    (list "and" (list ($tbool) ($tbool)) ($tbool))
    (list "not" (list ($tbool)) ($tbool))))
 
(defmethod type= (($1 t) ($2 t))
  (type_error))

(defmethod type= (($1 $tint) ($2 $tint))
  t)

(defmethod type= (($1 $tbool) ($2 $tbool))
  t)

(defmethod type= (($1 $tfunc) ($2 $tfunc))
  (and 
    (type= ($tfunc.domain $1) ($tfunc.domain $2))
    (type= ($tfunc.range $1)  ($tfunc.range $2))))

(defmethod typecheck% (($ins $integer) env) 
  ($tint))

(defmethod typecheck% (($ins $boolean) env)
  ($tbool))

(defmethod typecheck% (($ins $var) env)
  (declare (special *global-env*))
  (let* ((env (append env *global-env*))
         (tmp (cdr (assoc ($var.value $ins) env :test #'string=))))
    (when (null tmp) 
      (undefvar_error))
    tmp))

(defmethod typecheck% (($ins $special) env)
  (declare (special *global-env*))
  (let ((env (append env *global-env*))
        (ident ($special.ident $ins)))
    (cond 
      ((string= ident "if")
       (destructuring-bind 
         (test then else) ($exprseq.container ($special.exprs $ins)) 
         (let ((then-type (typecheck% then env)))
           (unless 
             (and (type= (typecheck% test env) ($tbool))
                  (type= then-type (typecheck% else env)))
             (type_error))
           then-type)))
      ((string= ident "print")
       (destructuring-bind 
         (expr) ($exprseq.container ($special.exprs $ins))
         (typecheck% expr env)))
      (t (undefform_error)))))

(defmethod typecheck% (($ins $function) env)
  (declare (special *global-env*))
  (let* ((env (append env *global-env*))
         (tmp ($function.typedvar $ins))
         (var ($typedvar.var tmp))
         (vt ($typedvar.typing tmp))
         (rv ($function.typing $ins)))
    (unless 
      (type= 
        rv 
        (typecheck% 
          ($function.expr $ins) 
          (acons ($var.value var) vt env)))
      (type_error))
    ($tfunc :domain vt :range rv)))

(defmethod typecheck% (($ins $defunc) env)
  (declare (special *global-env*))
  (let* ((lambdaform ($defunc.lambdaform $ins))
         (tvar ($function.typedvar lambdaform))
         (domain  ($typedvar.typing tvar))
         (range   ($function.typing lambdaform)))
    (push 
      (cons ($defunc.name $ins) ($tfunc :domain domain :range range)) 
      *global-env*)
    (typecheck% lambdaform env)))

(defmethod typecheck% (($ins $primitive-call) env)
  (declare (special *global-env*))
  (let ((env (append env *global-env*))
        (poptype (cdr (assoc ($pcall.callee $ins) *primitive-operator-type* :test #'string=))))
    (when (null poptype)
      (undefcall-error))
    (destructuring-bind 
      (domain range) poptype
      (unless 
        (every #'type= 
             domain 
             (mapcar 
               (lambda (x) (typecheck% x env)) 
               ($exprseq.container ($pcall.exprs $ins))))
        (type_error))
      range)))

(defmethod typecheck% (($ins $lambda-call) env)
  (declare (special *global-env*))
  (let ((env (append env *global-env*))
        (functype (typecheck% ($lcall.callee $ins) env)))
    (unless 
      (and 
        (typep functype '$tfunc) 
        (type= 
          ($tfunc.domain functype)
          (typecheck% ($lcall.expr $ins) env)))
      (type_error))
    ($tfunc.range functype)))

(defmethod typecheck% (($ins $program) env)
  (declare (special *global-env*))
  (let ((env (append  env *global-env*)))
    (loop with lt = nil
        finally (Return lt)
        for each in ($program.container $ins)
        do (setf lt (typecheck% each env)))))

(defun typecheck (obj init)
  (let ((*global-env* init))
    (declare (special *global-env*))
    (values (typecheck% obj nil) *global-env* )))

(defmethod print-object (($ins $tint) stream)
  (format stream "Integer"))

(defmethod print-object (($ins $tbool) stream)
  (format stream "Boolean"))

(defmethod print-object (($ins $tfunc) stream)
  (format stream "(")
  (print-object ($tfunc.domain $ins) stream)
  (format stream " -> ")
  (print-object ($tfunc.range $ins) stream)
  (format stream ")"))

(defmethod ->sexpr (($ins $integer))
  (parse-integer ($integer.value $ins)))

(defmethod ->sexpr (($ins $boolean))
  (when (string= ($boolean.value $ins) "true")
    t))

(defmethod ->sexpr (($ins $var))
  (intern ($var.value $ins)))

(defmethod ->sexpr (($ins $primitive-call))
  (cons (intern (string-upcase ($pcall.callee $ins))) 
        (mapcar #'->sexpr ($exprseq.container ($pcall.exprs $ins)))))

(defmethod ->sexpr (($ins $lambda-call))
  (list 'funcall (->sexpr ($lcall.callee $ins)) (->sexpr ($lcall.expr $ins))))

(defmethod ->sexpr (($ins $special))
  (let ((ident ($special.ident $ins)))
   (cons (intern (string-upcase ident))  
         (mapcar #'->sexpr ($exprseq.container ($special.exprs $ins))))))

(defmethod ->sexpr (($ins $function))
  (list 'lambda 
        (list (->sexpr ($typedvar.var ($function.typedvar $ins))))
        (->sexpr ($function.expr $ins))))

(defmethod ->sexpr (($ins $defunc))
  `(setf ,(intern ($defunc.name $ins))
        ,(->sexpr ($defunc.lambdaform $ins))))

(defmethod ->sexpr (($ins $program))
  (cons 'progn (mapcar #'->sexpr ($program.container $ins))))
