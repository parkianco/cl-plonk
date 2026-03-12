;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Field arithmetic for PLONK

(in-package #:cl-plonk)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconstant +plonk-protocol-version+ 1
  "Current PLONK protocol version.")

(defconstant +plonk-default-curve+ :bn254
  "Default elliptic curve for PLONK operations.")

;;; ============================================================================
;;; Conditions
;;; ============================================================================

(define-condition plonk-error (error)
  ((message :initarg :message
            :reader plonk-error-message
            :initform "PLONK error"))
  (:report (lambda (c s)
             (format s "PLONK Error: ~A" (plonk-error-message c)))))

;;; ============================================================================
;;; Curve Parameters
;;; ============================================================================

(defun field-prime (curve)
  "Return the field prime for the given curve."
  (ecase curve
    (:bn254
     21888242871839275222246405745257275088696311157297823662689037894645226208583)
    (:bls12-381
     52435875175126190479447740508185965837690552500527637822603658699938581184513)))

(defun scalar-order (curve)
  "Return the scalar field order for the given curve."
  (ecase curve
    (:bn254
     21888242871839275222246405745257275088548364400416034343698204186575808495617)
    (:bls12-381
     52435875175126190479447740508185965837690552500527637822603658699938581184513)))

;;; ============================================================================
;;; Field Element Structure
;;; ============================================================================

(defstruct (field-element (:constructor %make-field-element))
  "An element of a prime field."
  (value 0 :type integer)
  (curve +plonk-default-curve+ :type keyword))

(defun make-field-element (value &optional (curve +plonk-default-curve+))
  "Create a field element, reducing modulo the field prime."
  (let ((p (scalar-order curve)))
    (%make-field-element :value (mod value p)
                         :curve curve)))

;;; ============================================================================
;;; Field Arithmetic
;;; ============================================================================

(defun field-add (a b)
  "Add two field elements."
  (let ((curve (field-element-curve a))
        (p (scalar-order (field-element-curve a))))
    (make-field-element (mod (+ (field-element-value a)
                                (field-element-value b))
                             p)
                        curve)))

(defun field-sub (a b)
  "Subtract field element B from A."
  (let ((curve (field-element-curve a))
        (p (scalar-order (field-element-curve a))))
    (make-field-element (mod (- (field-element-value a)
                                (field-element-value b))
                             p)
                        curve)))

(defun field-mul (a b)
  "Multiply two field elements."
  (let ((curve (field-element-curve a))
        (p (scalar-order (field-element-curve a))))
    (make-field-element (mod (* (field-element-value a)
                                (field-element-value b))
                             p)
                        curve)))

(defun field-neg (a)
  "Negate a field element."
  (let ((curve (field-element-curve a))
        (p (scalar-order (field-element-curve a))))
    (make-field-element (mod (- p (field-element-value a)) p)
                        curve)))

(defun field-exp (base exp &optional (curve +plonk-default-curve+))
  "Compute BASE^EXP in the scalar field using square-and-multiply."
  (let* ((p (scalar-order curve))
         (b (if (field-element-p base)
                (field-element-value base)
                base))
         (e (if (field-element-p exp)
                (field-element-value exp)
                exp)))
    (cond
      ((zerop e) (make-field-element 1 curve))
      ((= e 1) (make-field-element b curve))
      (t (let ((result 1)
               (base-val (mod b p)))
           (loop while (plusp e) do
             (when (oddp e)
               (setf result (mod (* result base-val) p)))
             (setf base-val (mod (* base-val base-val) p))
             (setf e (ash e -1)))
           (make-field-element result curve))))))

(defun field-inv (a)
  "Compute the multiplicative inverse of a field element.
   Uses Fermat's little theorem: a^(-1) = a^(p-2) mod p."
  (let* ((curve (field-element-curve a))
         (p (scalar-order curve))
         (val (field-element-value a)))
    (when (zerop val)
      (error 'plonk-error :message "Cannot invert zero"))
    (field-exp val (- p 2) curve)))

(defun field-div (a b)
  "Divide field element A by B."
  (field-mul a (field-inv b)))
