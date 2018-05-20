#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defgeneric find-definitions (designator &optional local-package))

(defclass definition ()
  ())

(defmethod print-object ((definition definition) stream)
  (print-unreadable-object (definition stream :type T)
    (prin1 (designator definition) stream)))

(defgeneric designator (definition))
(defgeneric object (definition))
(defgeneric symbol (definition))
(defgeneric name (definition))
(defgeneric package (definition))
(defgeneric type (definition))
(defgeneric visibility (definition))
(defgeneric documentation (definition))
(defgeneric source-location (definition))

(defclass callable (definition)
  ())

(defgeneric arguments (callable))

(defmethod arguments ((callable callable))
  (values NIL :unknown))

(defclass global-definition (definition)
  ((designator :initarg :designator :reader designator)
   (package :initarg :package :reader package))
  (:default-initargs :designator (error "DESIGNATOR required.")))

(defmethod initialize-instance :after ((definition global-definition) &key package)
  (unless package
    (setf (slot-value definition 'package) (symbol-package (symbol definition)))))

(defmethod object ((definition global-definition))
  (values NIL :unknown))

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

(defmethod visibility ((definition global-definition))
  (nth-value 1 (find-symbol (name definition) (package definition))))

(defmethod documentation ((definition global-definition))
  (values NIL :unknown))

(defmethod source-location ((definition global-definition))
  (values NIL :unknown))

(defvar *definition-resolvers* (make-hash-table :test 'eql))

(defun definition-resolver (name &optional (errorp T))
  (or (gethash name *definition-resolvers*)
      (when errorp (error "No resolver function named ~s." name))))

(defun (setf definition-resolver) (function name)
  (setf (gethash name *definition-resolvers*) function))

(defun remove-definition-resolver (name)
  (remhash name *definition-resolvers*))

(defmacro define-definition-resolver (name args &body body)
  `(progn (setf (definition-resolver ',name)
                (lambda ,args ,@body))
          ',name))

(defmethod find-definitions (designator &optional package)
  (loop for resolver being the hash-values of *definition-resolvers*
        append (funcall resolver designator package)))

;; FIXME: Generify the expansion of symbols to designators.

(defmethod find-definitions ((package cl:package) &optional (local NIL local-p))
  (loop for symbol being the symbols of package
        append (append (find-definitions symbol (if local-p local package))
                       (find-definitions `(setf ,symbol) (if local-p local package)))))

(defmethod find-definitions ((string string) &optional (local NIL local-p))
  (let ((package (or (find-package string)
                     (error "No package named ~s available." string))))
    (find-definitions package (if local-p local package))))

(defun apropos-definitions (string)
  (loop for package in (list-all-packages)
        append (loop for symbol being the symbols of package
                     when (search string (symbol-name symbol) :test #'char-equal)
                     append (append (find-definitions symbol package)
                                    (find-definitions `(setf ,symbol) package)))))

(defmacro define-simple-definition-resolver (class lookup-function &body body)
  (let ((package (gensym "PACKAGE")))
    `(define-definition-resolver ,class (,class ,package)
       (when (ignore-errors ,(if body
                                 `(destructuring-bind ,lookup-function ,class
                                    ,@body)
                                 `(,lookup-function ,class)))
         (list (make-instance ',class :designator ,class :package ,package))))))

(defmacro define-simple-object-lookup (class lookup-function &body body)
  `(defmethod object ((,class ,class))
     ,(if body
          `(destructuring-bind ,lookup-function ,class
             ,@body)
          `(,lookup-function (designator ,class)))))

(defmacro define-simple-documentation-lookup (class documentation-type)
  `(defmethod documentation ((,class ,class))
     ,(if (eql documentation-type T)
          `(cl:documentation (object ,class) 'T)
          `(cl:documentation (designator ,class) ',documentation-type))))

(defmacro define-simple-type-map (class type)
  `(defmethod type ((,class ,class))
     ',type))
