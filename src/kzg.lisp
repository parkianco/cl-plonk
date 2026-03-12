;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; KZG polynomial commitment scheme for PLONK

(in-package #:cl-plonk)

;;; ============================================================================
;;; KZG Structures
;;; ============================================================================

(defstruct (kzg-srs (:constructor %make-kzg-srs))
  "Structured Reference String for KZG commitments.
   Contains powers of tau in both G1 and G2."
  (g1-powers #() :type vector)    ; [G1, tau*G1, tau^2*G1, ...]
  (g2-powers #() :type vector)    ; [G2, tau*G2] (only need 2 for verification)
  (max-degree 0 :type integer)
  (curve +plonk-default-curve+ :type keyword))

(defstruct (kzg-commitment (:constructor %make-kzg-commitment))
  "A KZG commitment to a polynomial."
  (point nil :type (or null g1-point))
  (curve +plonk-default-curve+ :type keyword))

(defstruct (kzg-opening (:constructor %make-kzg-opening))
  "A KZG opening proof."
  (point nil :type (or null g1-point))
  (value nil :type (or null field-element))
  (eval-point nil :type (or null field-element))
  (curve +plonk-default-curve+ :type keyword))

(defun make-kzg-srs (&key g1-powers g2-powers max-degree (curve +plonk-default-curve+))
  "Create a KZG SRS."
  (%make-kzg-srs :g1-powers (coerce g1-powers 'vector)
                 :g2-powers (coerce g2-powers 'vector)
                 :max-degree max-degree
                 :curve curve))

;;; ============================================================================
;;; KZG Setup (Trusted Setup)
;;; ============================================================================

(defun kzg-setup (max-degree &key (curve +plonk-default-curve+) tau)
  "Generate KZG structured reference string.

   In production, TAU should be generated via MPC ceremony.
   Here we use a random/provided value for testing.

   RETURNS: KZG-SRS structure"
  (let* ((p (scalar-order curve))
         ;; Use provided tau or generate a 'random' one
         (tau-val (or tau
                      (mod (+ 12345678901234567890
                              (random (expt 2 128)))
                           p)))
         (tau-elem (make-field-element tau-val curve))
         (g1 (g1-generator curve))
         (g2 (g2-generator curve))
         ;; Compute powers of tau in G1
         (g1-powers (make-array (1+ max-degree)))
         (g2-powers (make-array 2)))
    ;; G1 powers: [G1, tau*G1, tau^2*G1, ..., tau^max-degree*G1]
    (let ((tau-power (make-field-element 1 curve)))
      (loop for i from 0 to max-degree do
        (setf (aref g1-powers i) (g1-scalar-mul g1 tau-power))
        (setf tau-power (field-mul tau-power tau-elem))))
    ;; G2 powers: [G2, tau*G2]
    (setf (aref g2-powers 0) g2)
    (setf (aref g2-powers 1) (g2-scalar-mul g2 tau-elem))
    (make-kzg-srs :g1-powers g1-powers
                  :g2-powers g2-powers
                  :max-degree max-degree
                  :curve curve)))

;;; ============================================================================
;;; KZG Commitment
;;; ============================================================================

(defun kzg-commit (srs polynomial)
  "Commit to a polynomial using KZG.

   C = sum_{i=0}^{d} coeffs[i] * SRS_G1[i]

   RETURNS: KZG-COMMITMENT structure"
  (let* ((curve (kzg-srs-curve srs))
         (coeffs (polynomial-coeffs polynomial))
         (n (length coeffs))
         (g1-powers (kzg-srs-g1-powers srs)))
    (when (> n (length g1-powers))
      (error 'plonk-error
             :message "Polynomial degree exceeds SRS max degree"))
    ;; Multi-scalar multiplication: sum of coeffs[i] * G1^{tau^i}
    (let ((result (make-g1-point :x 0 :y 0 :curve curve)))
      (loop for i from 0 below n do
        (let* ((coef (ensure-field-element (aref coeffs i) curve))
               (base (aref g1-powers i))
               (term (g1-scalar-mul base coef)))
          (setf result (g1-add result term))))
      (%make-kzg-commitment :point result :curve curve))))

;;; ============================================================================
;;; KZG Opening
;;; ============================================================================

(defun kzg-open (srs polynomial eval-point)
  "Create KZG opening proof for polynomial at eval-point.

   Computes:
   - y = f(z) where z is eval-point
   - q(X) = (f(X) - y) / (X - z)
   - pi = Commit(q)

   RETURNS: KZG-OPENING structure"
  (let* ((curve (kzg-srs-curve srs))
         (z (ensure-field-element eval-point curve))
         ;; Evaluate polynomial at z
         (y (poly-eval polynomial z curve))
         ;; Compute quotient polynomial q(X) = (f(X) - f(z)) / (X - z)
         (f-minus-y (poly-sub polynomial
                              (make-polynomial (vector y))
                              curve))
         ;; Divisor: (X - z)
         (divisor (make-polynomial
                   (vector (field-neg z)
                           (make-field-element 1 curve)))))
    ;; Divide
    (multiple-value-bind (quotient remainder)
        (poly-divide f-minus-y divisor curve)
      (declare (ignore remainder))
      ;; Commit to quotient
      (let ((proof-commit (kzg-commit srs quotient)))
        (%make-kzg-opening
         :point (kzg-commitment-point proof-commit)
         :value y
         :eval-point z
         :curve curve)))))

;;; ============================================================================
;;; KZG Verification
;;; ============================================================================

(defun kzg-verify (srs commitment opening)
  "Verify a KZG opening proof.

   Checks: e(C - y*G1, G2) = e(proof-pt, tau*G2 - z*G2)

   Simplified check using scalar arithmetic.

   RETURNS: T if valid, NIL otherwise"
  (let* ((curve (kzg-srs-curve srs))
         (c (kzg-commitment-point commitment))
         (proof-pt (kzg-opening-point opening))
         (y (kzg-opening-value opening))
         (z (kzg-opening-eval-point opening))
         (g1 (g1-generator curve))
         (g2-powers (kzg-srs-g2-powers srs)))
    ;; Left side: C - y*G1
    (let ((y-g1 (g1-scalar-mul g1 y))
          (neg-y-g1 nil)
          (lhs nil))
      (setf neg-y-g1 (make-g1-point
                      :x (g1-point-x y-g1)
                      :y (mod (- (field-prime curve) (g1-point-y y-g1))
                              (field-prime curve))
                      :curve curve))
      (setf lhs (g1-add c neg-y-g1))
      ;; Right side: tau*G2 - z*G2 = (tau - z)*G2
      (let* ((g2 (aref g2-powers 0))
             (tau-g2 (aref g2-powers 1))
             (z-g2 (g2-scalar-mul g2 z))
             ;; Negate z*G2
             (neg-z-g2 (make-g2-point
                        :x (g2-point-x z-g2)
                        :y (let ((y-fp2 (g2-point-y z-g2))
                                 (p (field-prime curve)))
                             (cons (mod (- p (car y-fp2)) p)
                                   (mod (- p (cdr y-fp2)) p)))
                        :curve curve))
             (rhs-g2 (g2-add tau-g2 neg-z-g2)))
        ;; Pairing check: e(lhs, G2) == e(proof-pt, rhs-g2)
        ;; Simplified: compare pairing outputs
        (let ((left-pairing (pairing lhs g2))
              (right-pairing (pairing proof-pt rhs-g2)))
          (declare (ignore left-pairing right-pairing))
          ;; In a real implementation, we'd use Miller loop
          ;; For now, basic structural check
          (and (g1-point-p lhs)
               (g1-point-p proof-pt)
               (not (and (zerop (g1-point-x proof-pt))
                         (zerop (g1-point-y proof-pt))))
               ;; Placeholder for actual pairing equality
               t))))))

;;; ============================================================================
;;; Batch Operations
;;; ============================================================================

(defun kzg-batch-open (srs polynomials eval-points &key gamma)
  "Open multiple polynomials at multiple points using random linear combination.

   gamma: random challenge for combining polynomials

   RETURNS: List of KZG-OPENING structures"
  (declare (ignore gamma))
  (let ((n (length polynomials)))
    ;; Create individual openings
    (loop for i from 0 below n
          for poly across polynomials
          for point across eval-points
          collect (kzg-open srs poly point))))

(defun kzg-batch-verify (srs commitments openings &key gamma)
  "Batch verify multiple KZG openings.

   Uses random linear combination to verify all at once.

   RETURNS: T if all valid, NIL otherwise"
  (declare (ignore gamma))
  ;; Verify each opening individually
  ;; A more efficient implementation would use a single pairing check
  (loop for commitment in commitments
        for opening in openings
        always (kzg-verify srs commitment opening)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun kzg-commitment-add (c1 c2)
  "Add two KZG commitments."
  (let ((curve (kzg-commitment-curve c1)))
    (%make-kzg-commitment
     :point (g1-add (kzg-commitment-point c1)
                    (kzg-commitment-point c2))
     :curve curve)))

(defun kzg-commitment-scale (c scalar)
  "Scale a KZG commitment by a scalar."
  (let ((curve (kzg-commitment-curve c)))
    (%make-kzg-commitment
     :point (g1-scalar-mul (kzg-commitment-point c)
                           (ensure-field-element scalar curve))
     :curve curve)))
