;; ====================================
;; System Management Utilities
;; ====================================

(in-package :stdutils)

;; Some of this is borrowed from or inspired by
;; Foner's rehash of Genera system functions.
;; Makes certain code management functions easier
;; although it is sure to confuse the novice!

;; A macro definition function that exports its name from
;; the enclosing package in which it was defined.
(defmacro defmacro-exported (name pattern &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (defmacro ,name ,pattern ,@body)))

(eval-when (compile load eval)
  (export '(defmacro-exported)))


(defmacro defexport (defdef name &rest rest)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (,defdef ,name ,@rest)))

(eval-when (compile load eval)
  (export '(defexport)))


;; A defun that exports it's name
(defmacro-exported defun-exported (function-spec lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',function-spec))
     (defun ,function-spec ,lambda-list ,@body)))

;; A defsetf that exports it's name
(defmacro-exported defsetf-exported (name function)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(setf ,name)))
     (defsetf ,name ,function)))

;; A class generation and exported function
(defmacro-exported defclass-exported (classname superclasses slots &rest options)
  "Exports the class name"
  `(progn
     (defclass ,classname ,superclasses ,slots ,@options)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',classname))))

;; A structure generation and exporting facility
(defmacro-exported defstruct-exported (structname superclasses slots &rest options)
  "Exports the structure and related accessors using reflection"
  (let ((struct (gensym)))  
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (export ',structname))
       (let ((,struct (defstruct ,structname ,superclasses ,slots ,@options)))
	 ,struct))))

;; An exported generic function 
(defmacro-exported defgeneric-exported (name lambda-list &rest args)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (defgeneric ,name ,lambda-list ,@args)))
 
;; A generic function that exports its name
(defmacro-exported defmethod-exported (method-spec lambda-list &body body)
  `(progn
     (defmethod ,method-spec ,lambda-list ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',method-spec))))

;; A defvar that exports it's name
(defmacro-exported defvar-exported (name &rest args)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (defvar ,name ,@args)))

;; A defconstant that exports it's name
(defmacro-exported defconstant-exported (name &rest args)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name))
     (defconstant ,name ,@args)))

;; Used instead of defun to add an inline proclamation
(defmacro-exported defsubst (function lambda-list &body body)
  `(progn 
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:proclaim '(cl:inline ,function)))
     (defun ,function ,lambda-list ,@body)))

;; The name exporting form of this definition
(defmacro-exported defsubst-exported (function lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',function))
     (defsubst ,function ,lambda-list ,@body)))

;; -------------------------------------------------
;; Anaphoric control structures

(defmacro-exported aif (test-form then-form &optional else-form) 
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro-exported aif-ret (test-form &body else-form)
  `(let ((it ,test-form))
     (if it it (progn ,@else-form))))

(defmacro-exported aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro-exported aif2t (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if ,win ,then ,else))))

(defmacro-exported aif2-ret (test-form &rest else-form)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test-form
       (if ,win it (progn ,@else-form)))))

(defmacro-exported ifret (test-form &rest else-form)
  `(aif-ret ,test-form ,@else-form))

(defmacro-exported retset (variable form)
  `(aif-ret ,variable
     (setf ,variable
	   ,form)))

(defmacro-exported awhen (test-form &body body) 
  `(aif ,test-form
	(progn ,@body)))

(defmacro-exported awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro-exported awhen2t (test &body body)
  `(aif2t ,test
	  (progn ,@body)))

(defmacro-exported awhen-null (test-form default-value)
  `(aif ,test-form it ,default-value))

(defmacro-exported awhen0 (test-form when-zero-form)
  `(let ((it ,test-form))
     (if (= it 0)
	 ,when-zero-form
       it)))

(defmacro-exported aprog1 (result-form &body body)
  `(let ((it ,result-form))
     (prog1 it
       ,@body)))

(defmacro-exported awhile (expr &body body) 
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro-exported awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))

(defmacro-exported aand (&rest args) 
  "Makes the prior element in an and available to the next"
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro-exported acond (&rest clauses)
  "Anaphoric cond"
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym))
	  (it (intern "IT" *package*)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((,it ,sym))
	       (declare (ignorable ,it))
	       ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

(defmacro-exported acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym))
	    (it (intern "IT" *package*)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((,it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(defmacro-exported acond-mv (&rest clauses)
  "If the second value of a multiple-value return from a
   clause is true, it is bound to the primary value"
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
	 (if (or ,val ,win)
	     (let ((it ,val)) 
	       (declare (ignorable it))
	       ,@(cdr cl1))
	   (acond-mv ,@(cdr clauses)))))))

(defmacro-exported acond2-mv (&rest clauses)
  "If the primary value of a multiple-value return from a
   clause is true, 'it' is bound to the second value"
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      `(multiple-value-bind (,win ,val) ,(car cl1)
	 (if ,win
	     (let ((it ,val)) 
	       (declare (ignorable it))
	       ,@(cdr cl1))
	   (acond2-mv ,@(cdr clauses)))))))

(defvar-exported self nil)

(defmacro-exported alambda (args &body body)
  "Allow recursive calls using captured variable 'self'"
  `(labels ((self ,args ,@body))
     #'self))

;; ---------------------------------------------
;; Package and Symbol Utilities
;; ---------------------------------------------

;; NOTE: Make sensitive to current case
;;(defun symbolize (string &optional (package *package*))
;;  "Interns string in package assuring lowercase characters in string."
;;  (intern (string-downcase string) package))

(defmacro-exported intern-format (format-string &rest format-args)
  "This interns a string defined by format-string into the current package"
  `(intern (format nil ,format-string ,@format-args)))

(defmacro-exported intern-format-pkg (pkg format-string &rest format-args)
  "This takes an explicit package and interns a string defined by format-string"
  `(intern (format nil ,format-string ,@format-args) ,pkg))

(defun-exported symbol-name-equal (src target &key key (test #'equal))
  "Compare two symbols independant of package, can treat target as a complex
   structure using key"
  (funcall test (symbol-name src) (symbol-name (if key (funcall key target) target))))

(defun-exported localize-symbol (sym &key (package *package*) exceptions (ignore-keywords t))
  (if (and (symbolp sym) 
	   (if ignore-keywords (not (keywordp sym)) t)
	   (not (eq (symbol-package sym) package))
	   (not (find (symbol-package sym) exceptions)))
      (intern (symbol-name sym) package)
    sym))

(defun-exported rem-keywords (list keywords)
  "Remove keywords from a keylist"
  (loop for (value indicator) on list by #'cddr
	unless (member value keywords)
	  nconc (list value indicator)))

(defun-exported keep-keywords (list keywords)
  "Keep only specified keywords from a keylist"
  (loop for (value indicator) on list by #'cddr
	when (member value keywords)
	  nconc (list value indicator)))

(defun-exported select-keywords (list)
  (loop for (key) on list by #'cddr
    collecting key))

(defmacro-exported with-assocs (names alist &body body)
  "Associates a symbol with it's equivalent in the alist.  Also
   matches keyword versions of the symbol to make it simpler for
   parsing most static datastructures you might want to use this for
   Beware if this isn't what you want."
  (let ((bind-func (gensym)))
    `(flet ((,bind-func (name list)
	      (awhen (or (assoc name list)
			 (assoc (intern (symbol-name name) 'keyword) list))
		     (cdr it))))
       (let ,(mapcar #'(lambda (name) 
			 (list name `(,bind-func ',name ,alist)))
	       names)
	 ,@body))))
  
(defmacro-exported with-keywords (names list &body body)
  "Bind keyword names to values from a list of alternating keyword value pairs"
  (let ((bind-value (gensym)))
  `(progn 
     (when (neq (mod (length ,list) 2) 0)
       (error "Keyword list must be even"))
     (let ((pairs (group ,list 2)))
       (flet ((,bind-value (name list)
		(awhen (find name list :key #'first)
		       (second it))))
	 (let ,(mapcar #'(lambda (name) 
			   (list name 
				 `(,bind-value ,(intern (symbol-name name) 'keyword) pairs)))
		 names)
	   ,@body))))))

(defun-exported use-package-noshadow-exported (src-package &optional (target-package *package*))
  (let ((src (if (packagep src-package) src-package (find-package src-package)))
	(dst (if (packagep target-package) target-package (find-package target-package))))
    (do-external-symbols (sym src)
			 (if (not (find-symbol (symbol-name sym) dst))
			     (progn
			       (import sym target-package)
			       (export sym target-package))))))

;;
;; Help with macro construction (before we load macros.lisp)
;; 

(defmacro-exported with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

