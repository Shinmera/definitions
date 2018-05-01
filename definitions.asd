#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem definitions
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "General definitions reflection library."
  :homepage "https://github.com/Shinmera/definitions"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "base-types")
               (:file "sbcl" :if-feature :sbcl)
               (:file "documentation"))
  :depends-on (:documentation-utils
               (:feature :sbcl :sb-introspect :sb-cltl2)))

