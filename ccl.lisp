#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defmethod object ((setf-expander setf-expander))
  (ccl::%setf-method (designator setf-expander)))

(defmethod object ((type-definition type-definition))
  (ccl::%deftype-expander (designator type-definition)))

(defmethod object ((condition condition))
  (find-class (designator condition)))

(defmethod object ((symbol-macro symbol-macro))
  (gethash (designator symbol-macro) ccl::*symbol-macros*))

(defmethod arguments ((callable callable))
  (multiple-value-bind (list provided) (ccl:arglist (object callable))
    (if provided list :unknown)))

(defmethod arguments ((method method))
  (loop for rest on (ccl:method-lambda-list (object method))
        for spec in (ccl:method-specializers (object method))
        collect (etypecase spec
                  (ccl:eql-specializer `(eql ,(ccl:eql-specializer-object spec)))
                  (cl:class (class-name spec)))
        into arguments
        finally (return (append arguments rest))))

(defmethod definition-source ((definition definition))
  (let ((object (object definition)))
    (transform-definition-sources (ccl:find-definition-sources object) :object object)))

(defmacro define-definition-source-type-lookup (class type)
  `(defmethod definition-source ((,class ,class))
     (transform-definition-sources (source-definitions-for-name (symbol ,class)) :type ',type)))

(defun source-definitions-for-name (name)
  (append (ccl:find-definition-sources name)
          (let ((alpha (gethash name ccl::*nx1-alphatizers*)))
            (when alpha (ccl:find-definition-sources alpha)))
          (let ((nx1-op (gethash name ccl::*nx1-operators*)))
            (when nx1-op
              (let ((dispatch (ccl::backend-p2-dispatch ccl::*target-backend*)))
                (when (array-in-bounds-p dispatch nx1-op)
                  (let ((p2 (aref dispatch nx1-op)))
                    (when p2
                      (ccl:find-definition-sources p2)))))))))

(defun transform-definition-sources (defs &key object type)
  (transform-definition-source
   (find-if-not
    #'null
    (rest
     (or (cond (object
                (find (ccl:name-of object) defs :key #'cdar :test #'equal))
               (type
                (find type defs :key #'caar :test (lambda (tt d) (typep d tt)))))
         (first defs))))))

(defun transform-definition-source (source)
  (when source
    (list :file (ccl:source-note-filename source)
          :form NIL
          :offset (ccl:source-note-start-pos source))))

(define-definition-source-type-lookup function ccl::function-definition-type)
(define-definition-source-type-lookup macro ccl::function-definition-type)
(define-definition-source-type-lookup compiler-macro ccl::compiler-macro-definition-type)
(define-definition-source-type-lookup setf-expander ccl::setf-expander-definition-type)
(define-definition-source-type-lookup generic-function ccl::function-definition-type)
(define-definition-source-type-lookup method-combination ccl::method-combination-definition-type)
(define-definition-source-type-lookup class ccl::class-definition-type)
(define-definition-source-type-lookup condition ccl::class-definition-type)
(define-definition-source-type-lookup structure ccl::structure-definition-type)
(define-definition-source-type-lookup type-definition ccl::type-definition-type)
(define-definition-source-type-lookup special-variable ccl::variable-definition-type)
(define-definition-source-type-lookup constant ccl::constant-definition-type)
;; symbol-macros are not recorded by CCL

(define-simple-definition-resolver setf-expander ccl::%setf-method)
(define-definition-resolver method (designator)
  (when (designator-generic-function-p designator)
    (loop for method in (ccl:generic-function-methods (fdefinition designator))
          collect (make-instance 'method :designator designator :method method))))
(define-simple-definition-resolver method-combination ccl::method-combination-info)
(define-simple-definition-resolver type-definition ccl::%deftype-expander)
(define-simple-definition-resolver special-variable ccl:proclaimed-special-p)
(define-simple-definition-resolver symbol-macro (designator)
  (gethash designator ccl::*symbol-macros*))
