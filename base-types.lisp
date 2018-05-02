#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defclass type (global-definition) ())
(defclass variable (global-definition) ())
(defclass package (global-definition) ())
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

(defgeneric qualifiers (method))

(defmethod qualifiers ((method method))
  (method-qualifiers (object method)))

(define-simple-type-map type cl:type)
(define-simple-type-map variable cl:variable)
(define-simple-type-map package cl:package)
(define-simple-type-map function cl:function)
(define-simple-type-map macro cl:function)
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
  (ignore-errors
   (and (fdefinition designator)
        (or (listp designator)
            (not (macro-function designator)))
        (not (typep (fdefinition designator) 'standard-generic-function)))))

(defun designator-generic-function-p (designator)
  (ignore-errors
   (typep (fdefinition designator) 'standard-generic-function)))

(defun designator-structure-p (designator)
  (when (symbolp designator)
    (ignore-errors (subtypep designator 'structure-class))))

(defun designator-condition-p (designator)
  (when (symbolp designator)
    (ignore-errors (subtypep designator 'condition))))

(defun designator-class-p (designator)
  (ignore-errors
   (and (not (listp designator))
        (not (designator-structure-p designator))
        (not (designator-condition-p designator))
        (find-class designator NIL))))

(defun designator-constant-p (designator)
  (when (symbolp designator)
    (constantp designator)))
