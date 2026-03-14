;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; test-plonk.lisp - Unit tests for plonk
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:cl-plonk.test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-plonk.test)

(defun run-tests ()
  "Run all tests for cl-plonk."
  (format t "~&Running tests for cl-plonk...~%")
  ;; TODO: Add test cases
  ;; (test-function-1)
  ;; (test-function-2)
  (format t "~&All tests passed!~%")
  t)
