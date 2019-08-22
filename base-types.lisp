#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defclass special-operator (global-definition callable) ())
(defclass type (global-definition) ())
(defclass variable (global-definition) ())
(defclass package (global-definition)
  ((package :reader object)))
(defclass function (global-definition callable) ())
(defclass macro (global-definition callable) ())
(defclass compiler-macro (global-definition callable) ())
(defclass setf-expander (global-definition callable) ())
(defclass generic-function (function) ())
(defclass method (global-definition callable)
  ((method :initarg :method :initform (error "METHOD required.") :reader object)))
(defclass method-combination (global-definition) ())
(defclass class (type) ())
(defclass condition (type) ())
(defclass structure (type) ())
(defclass type-definition (type) ())
(defclass special-variable (variable) ())
(defclass constant (variable) ())
(defclass symbol-macro (variable) ())

;; FIXME: Add introspection for method combination, methods.

(defmethod initialize-instance :around ((def package) &rest args &key package designator)
  (macrolet ((c (&rest args)
               `(apply #'call-next-method def ,@args args)))
    (cond ((null package)
           (c :package (or (find-package designator)
                           (error "No such package ~s." designator))
              :designator designator))
          ((null designator)
           (c :package package :designator (package-name designator)))
          (T
           (c)))))

(defmethod designator ((package package))
  (symbol package))

(defmethod name ((package package))
  (package-name (package package)))

(defmethod symbol ((package package))
  (make-symbol (name package)))

(defmethod visibility ((package package))
  :external)

(defgeneric qualifiers (method))

(defmethod qualifiers ((method method))
  (method-qualifiers (object method)))

(defmethod documentation ((definition global-definition))
  (multiple-value-bind (object unknown-p) (object definition)
    (if (eql :unknown unknown-p)
        (cl:documentation (designator definition) (type definition))
        (cl:documentation object T))))

(define-simple-type-map special-operator :special-operator)
(define-simple-type-map type cl:type)
(define-simple-type-map variable cl:variable)
(define-simple-type-map package cl:package)
(define-simple-type-map function cl:function)
(define-simple-type-map macro cl:macro-function)
(define-simple-type-map compiler-macro cl:compiler-macro)
(define-simple-type-map setf-expander cl:setf)
(define-simple-type-map generic-function cl:generic-function)
(define-simple-type-map method cl:method)
(define-simple-type-map method-combination cl:method-combination)
(define-simple-type-map class cl:class)
(define-simple-type-map condition cl:condition)
(define-simple-type-map structure cl:structure)
(define-simple-type-map type-definition cl:type)
(define-simple-type-map special-variable cl:variable)
(define-simple-type-map constant cl:variable)
(define-simple-type-map symbol-macro cl:variable)

(define-simple-object-lookup package find-package)
(define-simple-object-lookup function fdefinition)
(define-simple-object-lookup macro macro-function)
(define-simple-object-lookup compiler-macro compiler-macro-function)
(define-simple-object-lookup class find-class)

;; It's not specified how to look up documentation for special
;; operators, since they're neither functions nor macros, but
;; we'll just assume as a default heuristic that the implementation
;; is going to store the docstring here.
(define-simple-documentation-lookup special-operator cl:function)
(define-simple-documentation-lookup type cl:type)
(define-simple-documentation-lookup variable cl:variable)
(define-simple-documentation-lookup package T)
(define-simple-documentation-lookup function cl:function)
(define-simple-documentation-lookup macro cl:function)
(define-simple-documentation-lookup compiler-macro cl:compiler-macro)
(define-simple-documentation-lookup setf-expander cl:setf)
(define-simple-documentation-lookup method T)
(define-simple-documentation-lookup method-combination cl:method-combination)
(define-simple-documentation-lookup structure cl:structure)

(define-simple-definition-resolver special-operator special-operator-p)
(define-simple-definition-resolver package find-package)
(define-simple-definition-resolver function designator-function-p)
(define-simple-definition-resolver macro macro-function)
(define-simple-definition-resolver compiler-macro compiler-macro-function)
(define-simple-definition-resolver generic-function designator-generic-function-p)
(define-simple-definition-resolver class designator-class-p)
(define-simple-definition-resolver condition designator-condition-p)
(define-simple-definition-resolver structure designator-structure-p)
(define-simple-definition-resolver constant designator-constant-p)
(define-simple-definition-resolver symbol-macro designator-symbol-macro-p)

(defun designator-function-p (designator)
  (and (fboundp designator)
       (or (listp designator)
           (not (macro-function designator)))
       (not (typep (fdefinition designator) 'cl:standard-generic-function))))

(defun designator-generic-function-p (designator)
  (and (fboundp designator)
       (typep (fdefinition designator) 'cl:standard-generic-function)))

(defun designator-structure-p (designator)
  (and (symbolp designator)
       (typep (find-class designator NIL)
              'cl:structure-class)))

(defun designator-condition-p (designator)
  (and (symbolp designator)
       (subtypep designator 'cl:condition)
       (not (subtypep designator 'cl:nil))
       (null (funcall (definition-resolver 'type-definition) designator))))

(defun designator-class-p (designator)
  (and (symbolp designator)
       (not (designator-structure-p designator))
       (not (designator-condition-p designator))
       (find-class designator NIL)))

(defun designator-constant-p (designator)
  (and (symbolp designator)
       (constantp designator)))

(defun designator-symbol-macro-p (designator)
  (and (symbolp designator)
       (nth-value 1 (macroexpand-1 designator))))

(defmethod bind (designator (type function) (object cl:function))
  (when (fboundp designator)
    (error 'binding-exists :type type :designator designator))
  (setf (fdefinition designator) object)
  designator)

(defmethod bind (designator (type macro) (object cl:function))
  (when (fboundp designator)
    (error 'binding-exists :type type :designator designator))
  (eval `(defmacro ,designator (&rest _)))
  (setf (macro-function designator) object)
  designator)

(defmethod bind (designator (type compiler-macro) (object cl:function))
  (when (or (macro-function designator)
            (compiler-macro-function designator))
    (error 'binding-exists :type type :designator designator))
  (setf (compiler-macro-function designator) object)
  designator)

(defmethod bind ((designator cl:symbol) (type class) (object cl:class))
  (when (find-class designator NIL)
    (error 'binding-exists :type type :designator designator))
  (setf (find-class designator) object)
  designator)

(defmethod bind ((designator cl:symbol) (type special-variable) object)
  (when (boundp designator)
    (error 'binding-exists :type type :designator designator))
  (proclaim `(special ,designator))
  (setf (symbol-value designator) object)
  designator)

(defmethod bind ((designator cl:symbol) (type constant) object)
  (when (boundp designator)
    (error 'binding-exists :type type :designator designator))
  (eval `(defconstant ,designator ,object))
  designator)

(defmethod bind ((designator cl:symbol) (type symbol-macro) object)
  (when (or (boundp designator)
            (designator-symbol-macro-p designator))
    (error 'binding-exists :type type :designator designator))
  (eval `(define-symbol-macro ,designator ,object))
  designator)

(defmethod unbind ((definition package))
  (delete-package (designator definition)))

(defmethod unbind ((definition function))
  (fmakunbound (designator definition)))

(defmethod unbind ((definition macro))
  (fmakunbound (designator definition)))

(defmethod unbind ((definition compiler-macro))
  (setf (compiler-macro-function (designator definition)) NIL))

(defmethod unbind ((definition method))
  (remove-method (fdefinition (designator definition)) (method definition)))

(defmethod unbind ((definition class))
  (setf (find-class (designator definition)) NIL))

(defmethod unbind ((definition special-variable))
  ;; FIXME: how to declare not special?
  (makunbound (designator definition)))

(defmethod (setf object) ((object cl:function) (definition function))
  (setf (fdefinition (designator definition)) object))

(defmethod (setf object) ((object cl:function) (definition macro))
  (setf (macro-function (designator definition)) object))

(defmethod (setf object) ((object cl:function) (definition compiler-macro))
  (setf (compiler-macro-function (designator definition)) object))

(defmethod (setf object) ((object cl:class) (definition class))
  (setf (find-class (designator definition)) object))

(defmethod (setf object) (object (definition symbol-macro))
  (eval `(define-symbol-macro ,(identifier definition) ,obejct))
  object)

(defmethod (setf object) (object (definition special-variable))
  (setf (symbol-value (designator definition)) object))

;; FIXME: add non-local types such as labels, blocks, tags, restarts, and lexical variables
;;        though I have no idea how discovery should be handled for those things.

;; IMPL (defmethod bind ((designator symbol) (type setf-expander) (object cl:function)))
;; IMPL (defmethod bind ((designator symbol) (type generic-function) (object cl:function)))
;; IMPL (defmethod bind (designator (type method) (object _)))
;; IMPL (defmethod bind ((designator symbol) (type method-combination) (object _)))
;; IMPL (defmethod bind ((designator symbol) (type condition) (object _)))
;; IMPL (defmethod bind ((designator symbol) (type structure) (object _)))
;; IMPL (defmethod bind ((designator symbol) (type type-definition) (object cl:function)))

;; IMPL (defmethod unbind ((definition setf-expander)))
;; IMPL (defmethod unbind ((definition method-combination)))
;; IMPL (defmethod unbind ((definition condition)))
;; IMPL (defmethod unbind ((definition structure)))
;; IMPL (defmethod unbind ((definition type-definition)))
;; IMPL (defmethod unbind ((definition constant)))
;; IMPL (defmethod unbind ((definition symbol-macro)))

;; IMPL (defmethod (setf object) ((object cl:function) (definition method)))
;; IMPL (defmethod (setf object ((object cl:function) (definition setf-expander)))
;; IMPL (defmethod (setf object (object (definition method-combination)))
;; IMPL (defmethod (setf object (object (definition condition)))
;; IMPL (defmethod (setf object (object (definition structure)))
;; IMPL (defmethod (setf object ((object cl:function) (definition type-definition)))
;; IMPL (defmethod (setf object (object (definition constant)))

;; IMPL (define-simple-definition-resolver setf-expander)
;; IMPL (define-definition-resolver method)
;; IMPL (define-simple-definition-resolver method-combination)
;; IMPL (define-simple-definition-resolver type-definition)
;; IMPL (define-simple-definition-resolver special-variable)
