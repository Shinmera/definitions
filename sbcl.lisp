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

(defmethod arguments ((callable callable))
  (sb-introspect:function-lambda-list (object callable)))

(defmethod arguments ((method method))
  (sb-mop:method-lambda-list (object method)))

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
  (when source
    (list :file (sb-introspect:definition-source-pathname source)
          :form (or (first (sb-introspect:definition-source-form-path source))
                    (sb-introspect:definition-source-form-number source))
          :offset (sb-introspect:definition-source-character-offset source))))

(define-simple-definition-resolver setf-expander
    (lambda (designator)
      (sb-int:info :setf :expander designator)))

(define-definition-resolver method (designator)
  (when (designator-generic-function-p designator)
    (sb-mop:generic-function-methods (fdefinition designator))))

(define-simple-definition-resolver method-combination
    (lambda (designator)
      (find-method #'sb-mop:find-method-combination
                   nil
                   (list (find-class 'cl:generic-function) `(eql ,designator) T)
                   nil)))

(define-simple-definition-resolver type-definition
    (lambda (designator)
      (eql :defined (sb-int:info :type :kind designator))))

(define-simple-definition-resolver special-variable
    (lambda (designator)
      (eq :special (sb-cltl2:variable-information designator))))

(define-simple-definition-resolver symbol-macro
    (lambda (designator)
      (info :variable :macro-expansion designator)))
