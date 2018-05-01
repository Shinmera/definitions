#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.definitions)

(defgeneric find-definitions (designator))

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

(defmethod find-definitions (designator)
  (loop for resolver being the hash-values of *definition-resolvers*
        append (funcall resolver designator)))

(defmethod find-definitions ((package cl:package))
  (loop for symbol being the symbols of package
        append (append (find-definitions symbol)
                       (find-definitions `(setf ,symbol)))))

(defmethod find-definitions ((string string))
  (loop for symbol being the symbols of *package*
        when (string-equal symbol string)
        append (append (find-definitions symbol)
                       (find-definitions `(setf ,symbol)))))
