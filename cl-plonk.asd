;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; PLONK zk-SNARK system definition

(asdf:defsystem #:cl-plonk
  :description "Pure Common Lisp implementation of the PLONK zk-SNARK proving system"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :homepage "https://github.com/parkianco/cl-plonk"
  :bug-tracker "https://github.com/parkianco/cl-plonk/issues"
  :source-control (:git "https://github.com/parkianco/cl-plonk.git")
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "field")
     (:file "curve")
     (:file "polynomial")
     (:file "kzg")
     (:file "plonk")
     (:file "serialization")))))
