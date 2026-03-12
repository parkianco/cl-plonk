;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Polynomial operations for PLONK

(in-package #:cl-plonk)

;;; ============================================================================
;;; Polynomial Structure
;;; ============================================================================

(defstruct (polynomial (:constructor %make-polynomial))
  "A polynomial represented by its coefficients (lowest degree first)."
  (coeffs #() :type vector))

(defun make-polynomial (coeffs)
  "Create a polynomial from coefficients."
  (let ((vec (if (vectorp coeffs) coeffs (coerce coeffs 'vector))))
    ;; Trim trailing zeros
    (let ((len (length vec)))
      (loop while (and (> len 1)
                       (let ((c (aref vec (1- len))))
                         (or (and (field-element-p c)
                                  (zerop (field-element-value c)))
                             (and (integerp c) (zerop c)))))
            do (decf len))
      (%make-polynomial :coeffs (subseq vec 0 len)))))

;;; ============================================================================
;;; Basic Polynomial Operations
;;; ============================================================================

(defun poly-degree (p)
  "Return the degree of polynomial P."
  (max 0 (1- (length (polynomial-coeffs p)))))

(defun poly-zero-p (p)
  "Check if polynomial is zero."
  (let ((coeffs (polynomial-coeffs p)))
    (or (zerop (length coeffs))
        (and (= 1 (length coeffs))
             (let ((c (aref coeffs 0)))
               (or (and (field-element-p c) (zerop (field-element-value c)))
                   (and (integerp c) (zerop c))))))))

(defun ensure-field-element (x &optional (curve +plonk-default-curve+))
  "Convert X to field element if needed."
  (if (field-element-p x)
      x
      (make-field-element x curve)))

(defun poly-add (a b &optional (curve +plonk-default-curve+))
  "Add two polynomials."
  (let* ((ca (polynomial-coeffs a))
         (cb (polynomial-coeffs b))
         (la (length ca))
         (lb (length cb))
         (result (make-array (max la lb))))
    (loop for i from 0 below (max la lb) do
      (let ((va (if (< i la) (ensure-field-element (aref ca i) curve)
                    (make-field-element 0 curve)))
            (vb (if (< i lb) (ensure-field-element (aref cb i) curve)
                    (make-field-element 0 curve))))
        (setf (aref result i) (field-add va vb))))
    (make-polynomial result)))

(defun poly-sub (a b &optional (curve +plonk-default-curve+))
  "Subtract polynomial B from A."
  (let* ((ca (polynomial-coeffs a))
         (cb (polynomial-coeffs b))
         (la (length ca))
         (lb (length cb))
         (result (make-array (max la lb))))
    (loop for i from 0 below (max la lb) do
      (let ((va (if (< i la) (ensure-field-element (aref ca i) curve)
                    (make-field-element 0 curve)))
            (vb (if (< i lb) (ensure-field-element (aref cb i) curve)
                    (make-field-element 0 curve))))
        (setf (aref result i) (field-sub va vb))))
    (make-polynomial result)))

(defun poly-mul (a b &optional (curve +plonk-default-curve+))
  "Multiply two polynomials using naive algorithm."
  (let* ((ca (polynomial-coeffs a))
         (cb (polynomial-coeffs b))
         (la (length ca))
         (lb (length cb)))
    (when (or (zerop la) (zerop lb))
      (return-from poly-mul (make-polynomial #())))
    (let ((result (make-array (+ la lb -1)
                              :initial-element (make-field-element 0 curve))))
      (loop for i from 0 below la do
        (loop for j from 0 below lb do
          (let ((prod (field-mul (ensure-field-element (aref ca i) curve)
                                 (ensure-field-element (aref cb j) curve))))
            (setf (aref result (+ i j))
                  (field-add (aref result (+ i j)) prod)))))
      (make-polynomial result))))

(defun poly-scale (p scalar &optional (curve +plonk-default-curve+))
  "Multiply polynomial by scalar."
  (let* ((coeffs (polynomial-coeffs p))
         (s (ensure-field-element scalar curve))
         (result (make-array (length coeffs))))
    (loop for i from 0 below (length coeffs) do
      (setf (aref result i)
            (field-mul (ensure-field-element (aref coeffs i) curve) s)))
    (make-polynomial result)))

(defun poly-eval (p x &optional (curve +plonk-default-curve+))
  "Evaluate polynomial at point X using Horner's method."
  (let* ((coeffs (polynomial-coeffs p))
         (n (length coeffs))
         (xf (ensure-field-element x curve)))
    (if (zerop n)
        (make-field-element 0 curve)
        (let ((result (ensure-field-element (aref coeffs (1- n)) curve)))
          (loop for i from (- n 2) downto 0 do
            (setf result (field-add (field-mul result xf)
                                    (ensure-field-element (aref coeffs i) curve))))
          result))))

;;; ============================================================================
;;; Polynomial Division
;;; ============================================================================

(defun poly-divide (dividend divisor &optional (curve +plonk-default-curve+))
  "Divide polynomials, returning (VALUES quotient remainder)."
  (let* ((num (map 'vector
                   (lambda (c) (ensure-field-element c curve))
                   (polynomial-coeffs dividend)))
         (den (map 'vector
                   (lambda (c) (ensure-field-element c curve))
                   (polynomial-coeffs divisor)))
         (num-deg (1- (length num)))
         (den-deg (1- (length den))))
    (when (< num-deg den-deg)
      (return-from poly-divide
        (values (make-polynomial (vector (make-field-element 0 curve)))
                dividend)))
    (let* ((quotient-deg (- num-deg den-deg))
           (quotient (make-array (1+ quotient-deg)))
           (remainder (copy-seq num))
           (lead-inv (field-inv (aref den den-deg))))
      (loop for i from quotient-deg downto 0 do
        (let ((coef (field-mul (aref remainder (+ i den-deg)) lead-inv)))
          (setf (aref quotient i) coef)
          (loop for j from 0 to den-deg do
            (setf (aref remainder (+ i j))
                  (field-sub (aref remainder (+ i j))
                             (field-mul coef (aref den j)))))))
      (values (make-polynomial quotient)
              (make-polynomial (subseq remainder 0 den-deg))))))

(defun poly-mod (dividend divisor &optional (curve +plonk-default-curve+))
  "Return remainder of polynomial division."
  (multiple-value-bind (q r) (poly-divide dividend divisor curve)
    (declare (ignore q))
    r))

;;; ============================================================================
;;; Special Polynomials
;;; ============================================================================

(defun vanishing-polynomial (domain &optional (curve +plonk-default-curve+))
  "Compute the vanishing polynomial for a domain.
   Z_H(X) = (X - omega^0)(X - omega^1)...(X - omega^(n-1))
   Simplified: Z_H(X) = X^n - 1"
  (let ((n (length domain)))
    (make-polynomial
     (let ((coeffs (make-array (1+ n) :initial-element (make-field-element 0 curve))))
       (setf (aref coeffs 0) (make-field-element -1 curve))
       (setf (aref coeffs n) (make-field-element 1 curve))
       coeffs))))

(defun lagrange-basis (domain i &optional (curve +plonk-default-curve+))
  "Compute the i-th Lagrange basis polynomial for the domain."
  (let ((n (length domain))
        (xi (aref domain i)))
    (let ((result (make-polynomial (vector (make-field-element 1 curve)))))
      (loop for j from 0 below n
            when (/= i j) do
              (let* ((xj (aref domain j))
                     (denom (field-sub (ensure-field-element xi curve)
                                       (ensure-field-element xj curve)))
                     (denom-inv (field-inv denom))
                     ;; (X - xj) / (xi - xj)
                     (linear (make-polynomial
                              (vector (field-mul (field-neg (ensure-field-element xj curve))
                                                 denom-inv)
                                      denom-inv))))
                (setf result (poly-mul result linear curve))))
      result)))

(defun lagrange-interpolate (points values &optional (curve +plonk-default-curve+))
  "Interpolate a polynomial through (points, values) pairs."
  (let ((n (length points))
        (result (make-polynomial (vector (make-field-element 0 curve)))))
    (loop for i from 0 below n do
      (let* ((li (lagrange-basis points i curve))
             (yi (ensure-field-element (aref values i) curve))
             (term (poly-scale li yi curve)))
        (setf result (poly-add result term curve))))
    result))

;;; ============================================================================
;;; Roots of Unity
;;; ============================================================================

(defun primitive-root-of-unity (n &optional (curve +plonk-default-curve+))
  "Find a primitive n-th root of unity in the scalar field."
  (let* ((p (scalar-order curve))
         ;; Find generator of multiplicative group
         (g 5)
         ;; Compute omega = g^((p-1)/n)
         (exp (/ (1- p) n)))
    (when (/= (mod (1- p) n) 0)
      (error 'plonk-error :message "N does not divide p-1"))
    (field-exp g exp curve)))

(defun roots-of-unity (n &optional (curve +plonk-default-curve+))
  "Generate the n-th roots of unity."
  (let ((omega (primitive-root-of-unity n curve))
        (roots (make-array n)))
    (loop for i from 0 below n
          with curr = (make-field-element 1 curve) do
            (setf (aref roots i) curr)
            (setf curr (field-mul curr omega)))
    roots))

;;; ============================================================================
;;; FFT Operations
;;; ============================================================================

(defun fft (coeffs &optional (curve +plonk-default-curve+))
  "Compute FFT of polynomial coefficients.
   Evaluates polynomial at all n-th roots of unity."
  (let* ((n (length coeffs))
         (omega (primitive-root-of-unity n curve)))
    (fft-internal coeffs omega n curve)))

(defun ifft (evals &optional (curve +plonk-default-curve+))
  "Compute inverse FFT to recover coefficients from evaluations."
  (let* ((n (length evals))
         (omega (primitive-root-of-unity n curve))
         (omega-inv (field-inv omega))
         (n-inv (field-inv (make-field-element n curve)))
         (result (fft-internal evals omega-inv n curve)))
    ;; Scale by 1/n
    (map 'vector (lambda (x) (field-mul x n-inv)) result)))

(defun fft-internal (vals omega n curve)
  "Internal recursive FFT implementation."
  (if (<= n 1)
      (map 'vector (lambda (x) (ensure-field-element x curve)) vals)
      (let* ((half (/ n 2))
             (even (make-array half))
             (odd (make-array half)))
        ;; Split into even and odd coefficients
        (loop for i from 0 below half do
          (setf (aref even i) (aref vals (* 2 i)))
          (setf (aref odd i) (aref vals (1+ (* 2 i)))))
        ;; Recursive FFT
        (let* ((omega-sq (field-mul omega omega))
               (y-even (fft-internal even omega-sq half curve))
               (y-odd (fft-internal odd omega-sq half curve))
               (result (make-array n))
               (curr (make-field-element 1 curve)))
          ;; Combine
          (loop for i from 0 below half do
            (let ((t-val (field-mul curr (aref y-odd i))))
              (setf (aref result i)
                    (field-add (aref y-even i) t-val))
              (setf (aref result (+ i half))
                    (field-sub (aref y-even i) t-val)))
            (setf curr (field-mul curr omega)))
          result))))

(defun coset-fft (coeffs shift &optional (curve +plonk-default-curve+))
  "Compute FFT over a coset k*H where H is the subgroup of roots of unity."
  (let* ((n (length coeffs))
         (shifted (make-array n))
         (k (ensure-field-element shift curve))
         (k-power (make-field-element 1 curve)))
    ;; Multiply coefficients by powers of k
    (loop for i from 0 below n do
      (setf (aref shifted i)
            (field-mul (ensure-field-element (aref coeffs i) curve) k-power))
      (setf k-power (field-mul k-power k)))
    (fft shifted curve)))

(defun coset-ifft (evals shift &optional (curve +plonk-default-curve+))
  "Compute inverse FFT over a coset."
  (let* ((n (length evals))
         (result (ifft evals curve))
         (k-inv (field-inv (ensure-field-element shift curve)))
         (k-power (make-field-element 1 curve)))
    ;; Divide coefficients by powers of k
    (loop for i from 0 below n do
      (setf (aref result i) (field-mul (aref result i) k-power))
      (setf k-power (field-mul k-power k-inv)))
    result))
