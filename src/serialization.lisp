;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; PLONK proof serialization

(in-package #:cl-plonk)

;;; ============================================================================
;;; Curve ID Mapping
;;; ============================================================================

(defun curve-id (curve)
  "Convert curve keyword to numeric ID."
  (case curve
    (:bn254 1)
    (:bls12-381 2)
    (otherwise 0)))

(defun id-to-curve (id)
  "Convert numeric ID to curve keyword."
  (case id
    (1 :bn254)
    (2 :bls12-381)
    (otherwise :unknown)))

;;; ============================================================================
;;; Integer Serialization
;;; ============================================================================

(defun write-u32-le (value buffer offset)
  "Write 32-bit unsigned integer in little-endian."
  (setf (aref buffer offset) (ldb (byte 8 0) value))
  (setf (aref buffer (+ offset 1)) (ldb (byte 8 8) value))
  (setf (aref buffer (+ offset 2)) (ldb (byte 8 16) value))
  (setf (aref buffer (+ offset 3)) (ldb (byte 8 24) value))
  (+ offset 4))

(defun read-u32-le (bytes offset)
  "Read 32-bit unsigned integer in little-endian."
  (values (+ (aref bytes offset)
             (ash (aref bytes (+ offset 1)) 8)
             (ash (aref bytes (+ offset 2)) 16)
             (ash (aref bytes (+ offset 3)) 24))
          (+ offset 4)))

;;; ============================================================================
;;; Point Serialization
;;; ============================================================================

(defun serialize-g1-point-to-buffer (point buffer offset)
  "Serialize G1 point to buffer, return new offset."
  (let ((x (if (g1-point-p point) (g1-point-x point) 0))
        (y (if (g1-point-p point) (g1-point-y point) 0)))
    ;; Write x coordinate (32 bytes for BN254)
    (loop for i from 0 below 32 do
      (setf (aref buffer (+ offset i))
            (ldb (byte 8 (* i 8)) x)))
    ;; Write y coordinate
    (loop for i from 0 below 32 do
      (setf (aref buffer (+ offset 32 i))
            (ldb (byte 8 (* i 8)) y)))
    (+ offset 64)))

(defun serialize-field-element-to-buffer (elem buffer offset)
  "Serialize field element to buffer, return new offset."
  (let ((val (if (field-element-p elem) (field-element-value elem) elem)))
    (loop for i from 0 below 32 do
      (setf (aref buffer (+ offset i))
            (ldb (byte 8 (* i 8)) val)))
    (+ offset 32)))

(defun deserialize-g1-point-from-buffer (bytes offset curve)
  "Deserialize G1 point from bytes."
  (let ((x 0) (y 0))
    (loop for i from 0 below 32 do
      (setf x (dpb (aref bytes (+ offset i)) (byte 8 (* i 8)) x)))
    (loop for i from 0 below 32 do
      (setf y (dpb (aref bytes (+ offset 32 i)) (byte 8 (* i 8)) y)))
    (values (make-g1-point :x x :y y :curve curve)
            (+ offset 64))))

(defun deserialize-field-element-from-buffer (bytes offset curve)
  "Deserialize field element from bytes."
  (let ((val 0))
    (loop for i from 0 below 32 do
      (setf val (dpb (aref bytes (+ offset i)) (byte 8 (* i 8)) val)))
    (values (make-field-element val curve) (+ offset 32))))

;;; ============================================================================
;;; PLONK Proof Serialization
;;; ============================================================================

(defun serialize-plonk-proof (proof)
  "Serialize a PLONK proof to bytes.

   FORMAT:
   - Version (1 byte)
   - Curve ID (1 byte)
   - A point (64 bytes)
   - B point (64 bytes)
   - C point (64 bytes)
   - Z point (64 bytes)
   - t_lo point (64 bytes)
   - t_mid point (64 bytes)
   - t_hi point (64 bytes)
   - W_zeta point (64 bytes)
   - W_zeta_omega point (64 bytes)
   - a_eval (32 bytes)
   - b_eval (32 bytes)
   - c_eval (32 bytes)
   - s1_eval (32 bytes)
   - s2_eval (32 bytes)
   - z_omega_eval (32 bytes)

   Total: 2 + 9*64 + 6*32 = 770 bytes

   RETURNS: Byte vector of serialized proof"
  (let* ((curve (plonk-proof-curve proof))
         (total-size (+ 2             ; version + curve
                        (* 9 64)      ; 9 G1 points
                        (* 6 32)))    ; 6 field elements
         (buffer (make-array total-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (offset 0))
    ;; Version
    (setf (aref buffer offset) +plonk-protocol-version+)
    (incf offset)
    ;; Curve
    (setf (aref buffer offset) (curve-id curve))
    (incf offset)
    ;; G1 points
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-a proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-b proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-c proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-z proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-t-lo proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-t-mid proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-t-hi proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-w-zeta proof) buffer offset))
    (setf offset (serialize-g1-point-to-buffer (plonk-proof-w-zeta-omega proof) buffer offset))
    ;; Field elements
    (setf offset (serialize-field-element-to-buffer (plonk-proof-a-eval proof) buffer offset))
    (setf offset (serialize-field-element-to-buffer (plonk-proof-b-eval proof) buffer offset))
    (setf offset (serialize-field-element-to-buffer (plonk-proof-c-eval proof) buffer offset))
    (setf offset (serialize-field-element-to-buffer (plonk-proof-s1-eval proof) buffer offset))
    (setf offset (serialize-field-element-to-buffer (plonk-proof-s2-eval proof) buffer offset))
    (setf offset (serialize-field-element-to-buffer (plonk-proof-z-omega-eval proof) buffer offset))
    buffer))

(defun deserialize-plonk-proof (bytes)
  "Deserialize a PLONK proof from bytes.

   RETURNS: PLONK-PROOF structure"
  (let ((offset 0)
        a b c z t-lo t-mid t-hi w-zeta w-zeta-omega
        a-eval b-eval c-eval s1-eval s2-eval z-omega-eval
        curve)
    ;; Version check
    (let ((version (aref bytes offset)))
      (unless (= version +plonk-protocol-version+)
        (error 'plonk-error
               :message (format nil "Unsupported proof version: ~D" version))))
    (incf offset)
    ;; Curve
    (setf curve (id-to-curve (aref bytes offset)))
    (incf offset)
    ;; Deserialize G1 points
    (multiple-value-setq (a offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (b offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (c offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (z offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (t-lo offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (t-mid offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (t-hi offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (w-zeta offset) (deserialize-g1-point-from-buffer bytes offset curve))
    (multiple-value-setq (w-zeta-omega offset) (deserialize-g1-point-from-buffer bytes offset curve))
    ;; Deserialize field elements
    (multiple-value-setq (a-eval offset) (deserialize-field-element-from-buffer bytes offset curve))
    (multiple-value-setq (b-eval offset) (deserialize-field-element-from-buffer bytes offset curve))
    (multiple-value-setq (c-eval offset) (deserialize-field-element-from-buffer bytes offset curve))
    (multiple-value-setq (s1-eval offset) (deserialize-field-element-from-buffer bytes offset curve))
    (multiple-value-setq (s2-eval offset) (deserialize-field-element-from-buffer bytes offset curve))
    (multiple-value-setq (z-omega-eval offset) (deserialize-field-element-from-buffer bytes offset curve))
    ;; Build proof
    (%make-plonk-proof
     :a a :b b :c c :z z
     :t-lo t-lo :t-mid t-mid :t-hi t-hi
     :w-zeta w-zeta :w-zeta-omega w-zeta-omega
     :a-eval a-eval :b-eval b-eval :c-eval c-eval
     :s1-eval s1-eval :s2-eval s2-eval
     :z-omega-eval z-omega-eval
     :curve curve)))

;;; ============================================================================
;;; Verification Key Serialization
;;; ============================================================================

(defun serialize-verification-key (vk)
  "Serialize verification key to plist format."
  (list :q-l-commit (plonk-verification-key-q-l-commit vk)
        :q-r-commit (plonk-verification-key-q-r-commit vk)
        :q-o-commit (plonk-verification-key-q-o-commit vk)
        :q-m-commit (plonk-verification-key-q-m-commit vk)
        :q-c-commit (plonk-verification-key-q-c-commit vk)
        :sigma-1-commit (plonk-verification-key-sigma-1-commit vk)
        :sigma-2-commit (plonk-verification-key-sigma-2-commit vk)
        :sigma-3-commit (plonk-verification-key-sigma-3-commit vk)
        :num-gates (plonk-verification-key-num-gates vk)
        :num-public (plonk-verification-key-num-public vk)
        :curve (plonk-verification-key-curve vk)))

(defun deserialize-verification-key (plist)
  "Deserialize verification key from plist format."
  (%make-plonk-verification-key
   :srs nil
   :q-l-commit (getf plist :q-l-commit)
   :q-r-commit (getf plist :q-r-commit)
   :q-o-commit (getf plist :q-o-commit)
   :q-m-commit (getf plist :q-m-commit)
   :q-c-commit (getf plist :q-c-commit)
   :sigma-1-commit (getf plist :sigma-1-commit)
   :sigma-2-commit (getf plist :sigma-2-commit)
   :sigma-3-commit (getf plist :sigma-3-commit)
   :num-gates (or (getf plist :num-gates) 0)
   :num-public (or (getf plist :num-public) 0)
   :curve (or (getf plist :curve) +plonk-default-curve+)))
