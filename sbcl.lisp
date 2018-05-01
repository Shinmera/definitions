#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defmethod object ((type-definition type-definition))
  (sb-int:info :type :expander (designator type-definition)))

(defmethod object ((setf-expander setf-expander))
  (sb-int:info :setf :expander (designator setf-expander)))

(defmethod object ((condition condition))
  (find-class (designator condition)))

(defmethod object ((structure structure))
  (sb-int:info :typed-structure :info (designator structure)))

(defmethod object ((method-combination method-combination))
  (find-method #'sb-mop:find-method-combination
               nil
               (list (find-class 'cl:generic-function) `(eql ,name) T)
               nil))

(defgeneric sb-introspect-type (definition))

(defmethod definition-source ((definition definition))
  (transform-definition-source
   (list
    (sb-introspect:find-definition-sources-by-name
     (designator definition)
     (sb-introspect-type definition)))))

(defmacro define-definition-object-source-lookup (class)
  `(defmethod definition-source ((,class ,class))
     (transform-definition-source
      (sb-introspect:find-definition-source (object ,class)))))

(defmacro define-definition-introspect-type (class type)
  `(defmethod sb-introspect-type ((,class ,class))
     ,type))

(define-definition-object-source-lookup function)
(define-definition-object-source-lookup condition)
(define-definition-object-source-lookup structure)
(define-definition-object-source-lookup package)
(define-definition-object-source-lookup class)
(define-definition-object-source-lookup method)
(define-definition-introspect-type class :class)
(define-definition-introspect-type compiler-macro :compiler-macro)
(define-definition-introspect-type condition :condition)
(define-definition-introspect-type constant :constant)
(define-definition-introspect-type function :function)
(define-definition-introspect-type generic-function :generic-function)
(define-definition-introspect-type macro :macro)
(define-definition-introspect-type method :method)
(define-definition-introspect-type method-combination :method-combination)
(define-definition-introspect-type package :package)
(define-definition-introspect-type structure :structure)
(define-definition-introspect-type symbol-macro :symbol-macro)
(define-definition-introspect-type type-definition :type)
(define-definition-introspect-type special-variable :variable)

(defun transform-definition-source (source)
  (let ((path (sb-introspect:definition-source-pathname source))))
  (list :file (if (typep path 'logical-pathname)
                  (translate-logical-pathname path)
                  path)
        :form (or (first (sb-introspect:definition-source-form-path source))
                  (sb-introspect:definition-source-form-number source))
        :offset (sb-introspect:definition-source-character-offset source)))

(defun designator-type-p (designator)
  (unless (listp designator)
    (handler-case
        (not (typep (sb-kernel:specifier-type symbol) 'sb-kernel:unknown-type))
      (error () T))))

(defun designator-special-p (designator)
  (unless (listp designator)
    (eq (sb-cltl2:variable-information designator) :special)))
