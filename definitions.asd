#|
 This file is a part of Definitions
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem definitions
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "General definitions reflection library."
  :homepage "https://Shinmera.github.io/definitions/"
  :bug-tracker "https://github.com/Shinmera/definitions/issues"
  :source-control (:git "https://github.com/Shinmera/definitions.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "base-types")
               (:file "ccl" :if-feature :ccl)
               (:file "sbcl" :if-feature :sbcl)
               (:file "clasp" :if-feature :clasp)
               (:file "documentation"))
  :depends-on (:documentation-utils
               (:feature :sbcl (:require :sb-introspect))
               (:feature :sbcl (:require :sb-cltl2))))

