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
               (list (find-class 'cl:generic-function) `(eql ,(designator method-combination)) T)
               nil))

(defmethod arguments ((callable callable))
  (sb-introspect:function-lambda-list (object callable)))

(defmethod arguments ((method method))
  (sb-mop:method-lambda-list (object method)))

(defmethod definition-source ((definition definition))
  (transform-definition-source
   (sb-introspect:find-definition-source (object definition))))

(defmacro define-definition-introspect-type (class type)
  `(defmethod definition-source ((,class ,class))
     (transform-definition-source
      (first (sb-introspect:find-definition-sources-by-name (designator ,class) ,type)))))

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

(define-simple-definition-resolver setf-expander (designator)
  (sb-int:info :setf :expander designator))

(define-definition-resolver method (designator)
  (when (designator-generic-function-p designator)
    (loop for method in (sb-mop:generic-function-methods (fdefinition designator))
          collect (make-instance 'method :designator designator :method method))))

(define-simple-definition-resolver method-combination (designator)
  (find-method #'sb-mop:find-method-combination
               nil
               (list (find-class 'cl:generic-function) `(eql ,designator) T)
               nil))

(define-simple-definition-resolver type-definition (designator)
  (eql :defined (sb-int:info :type :kind designator)))

(define-simple-definition-resolver special-variable (designator)
  (eq :special (sb-cltl2:variable-information designator)))

(define-simple-definition-resolver symbol-macro (designator)
  (sb-int:info :variable :macro-expansion designator))

;;; Extra SBCL definitions

(defclass alien-type (global-definition) ())

(define-simple-type-map alien-type sb-alien-internals:alien-type)

(define-simple-definition-resolver alien-type (designator)
  (sb-int:info :alien-type :definition designator))

(define-definition-introspect-type alien-type :alien-type)

(defclass optimizer (global-definition)
  ((optimizer :initarg :optimizer :reader object)))

(define-simple-type-map optimizer sb-c:optimizer)

(define-definition-resolver optimizer (designator)
  (let ((fun-info (when (symbolp designator)
                    (sb-int:info :function :info designator))))
    (when fun-info
      (let ((otypes '(sb-c:fun-info-derive-type
                      sb-c:fun-info-ltn-annotate
                      sb-c:fun-info-optimizer
                      sb-c:fun-info-ir2-convert
                      sb-c::fun-info-stack-allocate-result
                      sb-c::fun-info-constraint-propagate
                      sb-c::fun-info-constraint-propagate-if
                      sb-c::fun-info-call-type-deriver)))
        (loop for (reader . name) in otypes
              for fn = (funcall reader fun-info)
              when fn collect (make-instance 'optimizer :designator designator :optimizer fn))))))

(define-definition-introspect-type optimizer :optimizer)

(defclass source-transform (global-definition) ())

(define-simple-type-map source-transform :source-transform)

(define-simple-definition-resolver source-transform (designator)
  (cond ((and (listp designator) (eql 'cl:setf (car designator)))
         (sb-int:info :function :source-transform (second designator)))
        ((symbolp designator)
         (sb-int:info :function :source-transform designator))))

(define-definition-introspect-type source-transform :source-transform)

(defclass transform (global-definition)
  ((transform :initarg :transform :reader object)))

(define-simple-type-map transform sb-c::transform)

(define-definition-resolver transform (designator)
  (let ((fun-info (when (symbolp designator)
                    (sb-int:info :function :info designator))))
    (when fun-info
      (loop for transform in (sb-c::fun-info-transforms fun-info)
            collect (make-instance 'transform :designator designator :transform transform)))))

(define-definition-introspect-type transform :transform)

(defclass vop (global-definition) ())

(define-simple-type-map vop sb-c::vop)

(define-simple-definition-resolver vop (designator)
  (sb-c::vop-parse-or-lose designator))

(define-definition-introspect-type vop :vop)

(defclass ir1-convert (global-definition) ())

(define-simple-type-map ir1-convert sb-c::ir1-convert)

(define-simple-definition-resolver ir1-convert (designator)
  (sb-int:info :function :ir1-convert designator))

(define-definition-introspect-type ir1-convert :ir1-convert)

(defclass declaration (global-definition) ())

(define-simple-type-map declaration cl:declaration)

(define-simple-definition-resolver declaration (designator)
  (sb-int:info :source-location :declaration designator))
