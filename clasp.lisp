(in-package #:org.shirakumo.definitions)

(define-simple-definition-resolver setf-expander (designator)
  (nth-value 1 (ext:setf-expander designator)))

(define-simple-definition-resolver special-variable ext:specialp)

(define-simple-definition-resolver method-combination clos::search-method-combination)

(define-definition-resolver method (designator package)
  (when (designator-generic-function-p designator)
    (loop for method in (clos:generic-function-methods (fdefinition designator))
          collect (make-instance 'method :designator designator :package package :method method))))

(define-simple-definition-resolver type-definition (designator)
  (nth-value 1 (ext:type-expander designator)))

(define-simple-definition-resolver symbol-macro ext:symbol-macro)

(defmethod object ((type-definition type-definition))
  (ext:type-expander (designator type-definition)))

(defmethod object ((setf-expander setf-expander))
  (ext:setf-expander (designator setf-expander)))

(defmethod object ((condition condition))
  (find-class (designator condition)))

(defmethod object ((structure structure))
  (find-class (designator structure)))

(defmethod object ((method-combination method-combination))
  (ignore-errors (clos::search-method-combination (designator method-combination))))

(defmethod arguments ((function function))
  (ext:function-lambda-list (designator function)))

(defmethod arguments ((function generic-function))
  (ext:function-lambda-list (designator function)))

(defmethod arguments ((method method))
  (loop for rest on (clos:method-lambda-list (object method))
        for spec in (clos:method-specializers (object method))
        collect (etypecase spec
                  (clos:eql-specializer `(eql ,(clos:eql-specializer-object spec)))
                  (cl:class (class-name spec)))
        into arguments
        finally (return (append arguments rest))))

#+(or)
(defmethod arguments ((type type))
  ...)

(defmacro define-definition-source-location-type (class type)
  `(defmethod source-location ((,class ,class))
     (transform-source-location
      (first (ext:source-location (designator ,class) ,type)))))

(define-definition-source-location-type function :function)
(define-definition-source-location-type generic-function :function)

(defun transform-source-location (source-location)
  (when source-location
    (list :file (ext:source-location-pathname source-location)
          :offset (ext:source-location-offset source-location))))
