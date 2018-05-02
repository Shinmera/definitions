#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(cl:defpackage #:definitions
  (:nicknames #:org.shirakumo.definitions)
  (:shadow
   #:symbol
   #:package
   #:type
   #:documentation
   #:variable
   #:package
   #:function
   #:macro
   #:compiler-macro
   #:generic-function
   #:method
   #:method-combination
   #:class
   #:condition
   #:structure
   #:constant
   #:symbol-macro
   #:declaration)
  (:use #:cl)
  ;; base-types.lisp
  (:export
   #:global-definition
   #:type
   #:variable
   #:package
   #:function
   #:macro
   #:compiler-macro
   #:setf-expander
   #:generic-function
   #:method
   #:qualifiers
   #:method-combination
   #:class
   #:condition
   #:structure
   #:type-definition
   #:special-variable
   #:constant
   #:symbol-macro)
  ;; protocol.lisp
  (:export
   #:find-definitions
   #:definition
   #:designator
   #:object
   #:symbol
   #:name
   #:package
   #:type
   #:visibility
   #:documentation
   #:source-location
   #:callable
   #:arguments
   #:definition-resolver
   #:remove-definition-resolver
   #:define-definition-resolver)
  ;;; Extra
  ;; sbcl.lisp
  (:export
   #:alien-type
   #:optimizer
   #:source-transform
   #:transform
   #:vop
   #:ir1-convert
   #:declaration))
