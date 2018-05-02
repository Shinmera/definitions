#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defclass global-definition (definition)
  ((designator :initarg :designator :reader designator))
  (:default-initargs :designator (error "DESIGNATOR required.")))

(defmethod symbol ((definition global-definition))
  (let ((designator (designator definition)))
    (cond ((not (listp designator))
           designator)
          ((eql 'setf (first designator))
           (second designator))
          (T
           (first designator)))))

(defmethod name ((definition global-definition))
  (symbol-name (symbol definition)))

(defmethod package ((definition global-definition))
  (symbol-package (symbol definition)))

(defmethod type ((definition global-definition))
  (type-of definition))

(defmethod visibility ((definition global-definition))
  (nth-value 1 (find-symbol (name definition) (package definition))))

(defclass type (global-definition) ())

(defclass variable (global-definition) ())

(defclass package (global-definition) ())

(defclass function (global-definition callable) ())

(defclass macro (global-definition callable) ())

(defclass compiler-macro (global-definition callable) ())

(defclass setf-expander (global-definition callable) ())

(defclass generic-function (function) ())

(defclass method (callable)
  ((method :initarg :method :initform (error "METHOD required.") :reader object)))

(defgeneric qualifiers (method))

(defmethod qualifiers ((method method))
  (method-qualifiers (object method)))

(defclass method-combination (global-definition) ())

(defclass class (type) ())

(defclass condition (type) ())

(defclass structure (type) ())

(defclass type-definition (type) ())

(defclass special-variable (variable) ())

(defclass constant (variable) ())

(defclass symbol-macro (variable) ())

(defmacro define-simple-object-lookup (class lookup-function)
  `(defmethod object ((,class ,class))
     (,lookup-function (designator ,class))))

(define-simple-object-lookup package find-package)
(define-simple-object-lookup function fdefinition)
(define-simple-object-lookup macro macro-function)
(define-simple-object-lookup compiler-macro compiler-macro-function)
(define-simple-object-lookup class find-class)

(defmacro define-simple-documentation-lookup (class documentation-type)
  `(defmethod documentation ((,class ,class))
     (cl:documentation (symbol ,class) ,documentation-type)))

(define-simple-documentation-lookup type 'cl:type)
(define-simple-documentation-lookup variable 'cl:variable)
(define-simple-documentation-lookup package 'cl:package)
(define-simple-documentation-lookup function 'cl:function)
(define-simple-documentation-lookup macro 'cl:function)
(define-simple-documentation-lookup compiler-macro 'cl:compiler-macro)
(define-simple-documentation-lookup setf-expander 'cl:setf)
(define-simple-documentation-lookup method-combination 'cl:method-combination)
(define-simple-documentation-lookup structure 'cl:structure)

(defmacro define-simple-definition-resolver (class lookup-function)
  `(define-definition-resolver ,class (,class)
     (when (ignore-errors (,lookup-function ,class))
       (list (make-instance ',class :designator ,class)))))

(define-simple-definition-resolver package find-package)
(define-simple-definition-resolver function designator-function-p)
(define-simple-definition-resolver macro macro-function)
(define-simple-definition-resolver compiler-macro compiler-macro-function)
(define-simple-definition-resolver generic-function designator-generic-function-p)
(define-simple-definition-resolver class designator-class-p)
(define-simple-definition-resolver condition designator-condition-p)
(define-simple-definition-resolver structure designator-structure-p)
(define-simple-definition-resolver constant designator-constant-p)
;; IMPL (define-simple-definition-resolver setf-expander ..)
;; IMPL (define-definition-resolver method ..)
;; IMPL (define-simple-definition-resolver method-combination ..)
;; IMPL (define-simple-definition-resolver type-definition ..)
;; IMPL (define-simple-definition-resolver special-variable ..)
;; IMPL (define-simple-definition-resolver symbol-macro ..)

(defun designator-function-p (designator)
  (and (fdefinition designator)
       (or (listp designator)
           (not (macro-function designator)))
       (not (typep (fdefinition designator) 'standard-generic-function))))

(defun designator-generic-function-p (designator)
  (typep (fdefinition designator) 'standard-generic-function))

(defun designator-structure-p (designator)
  (unless (listp designator)
    (ignore-errors (subtypep designator 'structure-class))))

(defun designator-condition-p (designator)
  (unless (listp designator)
    (ignore-errors (subtypep designator 'condition))))

(defun designator-class-p (designator)
  (and (not (listp designator))
       (not (designator-structure-p designator))
       (not (designator-condition-p designator))
       (find-class designator NIL)))

(defun designator-constant-p (designator)
  (unless (listp designator)
    (constantp designator)))
