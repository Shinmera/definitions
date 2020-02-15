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
   #:special-operator
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
   #:definition-p
   #:who-defines
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
   #:binding-exists
   #:bind
   #:unbind
   #:callable
   #:arguments
   #:global-definition
   #:definition-resolver
   #:remove-definition-resolver
   #:define-definition-resolver
   #:apropos-definitions
   #:define-simple-definition-resolver
   #:define-simple-object-lookup
   #:define-simple-documentation-lookup
   #:define-simple-type-map)
  ;;; Extra
  ;; sbcl.lisp
  (:export
   #:global
   #:alien-type
   #:optimizer
   #:source-transform
   #:transform
   #:vop
   #:ir1-convert
   #:declaration))
