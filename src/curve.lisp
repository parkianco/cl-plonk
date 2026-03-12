;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Elliptic curve operations for PLONK

(in-package #:cl-plonk)

;;; ============================================================================
;;; G1 Point Structure
;;; ============================================================================

(defstruct (g1-point (:constructor %make-g1-point))
  "A point on the G1 curve (base field)."
  (x 0 :type integer)
  (y 0 :type integer)
  (curve +plonk-default-curve+ :type keyword))

(defun make-g1-point (&key (x 0) (y 0) (curve +plonk-default-curve+))
  "Create a G1 point."
  (%make-g1-point :x x :y y :curve curve))

;;; ============================================================================
;;; G2 Point Structure
;;; ============================================================================

(defstruct (g2-point (:constructor %make-g2-point))
  "A point on the G2 curve (extension field).
   Coordinates are pairs representing Fp2 elements."
  (x (cons 0 0) :type cons)
  (y (cons 0 0) :type cons)
  (curve +plonk-default-curve+ :type keyword))

(defun make-g2-point (&key (x (cons 0 0)) (y (cons 0 0)) (curve +plonk-default-curve+))
  "Create a G2 point."
  (%make-g2-point :x x :y y :curve curve))

;;; ============================================================================
;;; Generator Points
;;; ============================================================================

(defun g1-generator (&optional (curve +plonk-default-curve+))
  "Return the generator point for G1."
  (ecase curve
    (:bn254
     (make-g1-point :x 1 :y 2 :curve curve))
    (:bls12-381
     (make-g1-point
      :x 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507
      :y 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569
      :curve curve))))

(defun g2-generator (&optional (curve +plonk-default-curve+))
  "Return the generator point for G2."
  (ecase curve
    (:bn254
     (make-g2-point
      :x (cons 10857046999023057135944570762232829481370756359578518086990519993285655852781
               11559732032986387107991004021392285783925812861821192530917403151452391805634)
      :y (cons 8495653923123431417604973247489272438418190587263600148770280649306958101930
               4082367875863433681332203403145435568316851327593401208105741076214120093531)
      :curve curve))
    (:bls12-381
     (make-g2-point
      :x (cons 352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160
               3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758)
      :y (cons 1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905
               927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582)
      :curve curve))))

;;; ============================================================================
;;; G1 Point Operations
;;; ============================================================================

(defun g1-add (p1 p2)
  "Add two G1 points using simplified affine addition."
  (let* ((curve (g1-point-curve p1))
         (p (field-prime curve))
         (x1 (g1-point-x p1))
         (y1 (g1-point-y p1))
         (x2 (g1-point-x p2))
         (y2 (g1-point-y p2)))
    (cond
      ;; Point at infinity cases
      ((and (zerop x1) (zerop y1)) p2)
      ((and (zerop x2) (zerop y2)) p1)
      ;; Point doubling case
      ((and (= x1 x2) (= y1 y2))
       (g1-double p1))
      ;; Inverse points
      ((and (= x1 x2) (= (mod (+ y1 y2) p) 0))
       (make-g1-point :x 0 :y 0 :curve curve))
      ;; General addition
      (t
       (let* ((lambda-val (mod (* (- y2 y1)
                                  (field-element-value
                                   (field-inv (make-field-element (- x2 x1) curve))))
                               p))
              (x3 (mod (- (* lambda-val lambda-val) x1 x2) p))
              (y3 (mod (- (* lambda-val (- x1 x3)) y1) p)))
         (make-g1-point :x x3 :y y3 :curve curve))))))

(defun g1-double (p)
  "Double a G1 point."
  (let* ((curve (g1-point-curve p))
         (prime (field-prime curve))
         (x (g1-point-x p))
         (y (g1-point-y p)))
    (when (zerop y)
      (return-from g1-double (make-g1-point :x 0 :y 0 :curve curve)))
    (let* ((lambda-val (mod (* (mod (* 3 x x) prime)
                               (field-element-value
                                (field-inv (make-field-element (* 2 y) curve))))
                            prime))
           (x3 (mod (- (* lambda-val lambda-val) (* 2 x)) prime))
           (y3 (mod (- (* lambda-val (- x x3)) y) prime)))
      (make-g1-point :x x3 :y y3 :curve curve))))

(defun g1-scalar-mul (point scalar)
  "Multiply G1 point by scalar using double-and-add."
  (let* ((curve (g1-point-curve point))
         (s (if (field-element-p scalar)
                (field-element-value scalar)
                scalar))
         (result (make-g1-point :x 0 :y 0 :curve curve))
         (temp point))
    (loop while (plusp s) do
      (when (oddp s)
        (setf result (g1-add result temp)))
      (setf temp (g1-double temp))
      (setf s (ash s -1)))
    result))

;;; ============================================================================
;;; G2 Point Operations
;;; ============================================================================

(defun fp2-add (a b p)
  "Add two Fp2 elements."
  (cons (mod (+ (car a) (car b)) p)
        (mod (+ (cdr a) (cdr b)) p)))

(defun fp2-sub (a b p)
  "Subtract two Fp2 elements."
  (cons (mod (- (car a) (car b)) p)
        (mod (- (cdr a) (cdr b)) p)))

(defun fp2-mul (a b p)
  "Multiply two Fp2 elements.
   (a0 + a1*i) * (b0 + b1*i) = (a0*b0 - a1*b1) + (a0*b1 + a1*b0)*i"
  (let ((a0 (car a)) (a1 (cdr a))
        (b0 (car b)) (b1 (cdr b)))
    (cons (mod (- (* a0 b0) (* a1 b1)) p)
          (mod (+ (* a0 b1) (* a1 b0)) p))))

(defun fp2-inv (a p)
  "Invert an Fp2 element."
  (let* ((a0 (car a))
         (a1 (cdr a))
         (denom (mod (+ (* a0 a0) (* a1 a1)) p))
         (inv-denom (field-element-value
                     (field-exp denom (- p 2) :bn254))))
    (cons (mod (* a0 inv-denom) p)
          (mod (- p (mod (* a1 inv-denom) p)) p))))

(defun g2-add (p1 p2)
  "Add two G2 points."
  (let* ((curve (g2-point-curve p1))
         (p (field-prime curve))
         (x1 (g2-point-x p1))
         (y1 (g2-point-y p1))
         (x2 (g2-point-x p2))
         (y2 (g2-point-y p2)))
    (cond
      ;; Point at infinity
      ((and (zerop (car x1)) (zerop (cdr x1))
            (zerop (car y1)) (zerop (cdr y1)))
       p2)
      ((and (zerop (car x2)) (zerop (cdr x2))
            (zerop (car y2)) (zerop (cdr y2)))
       p1)
      ;; Point doubling
      ((and (= (car x1) (car x2)) (= (cdr x1) (cdr x2))
            (= (car y1) (car y2)) (= (cdr y1) (cdr y2)))
       (g2-double p1))
      ;; General addition
      (t
       (let* ((dx (fp2-sub x2 x1 p))
              (dy (fp2-sub y2 y1 p))
              (lambda-val (fp2-mul dy (fp2-inv dx p) p))
              (lambda-sq (fp2-mul lambda-val lambda-val p))
              (x3 (fp2-sub (fp2-sub lambda-sq x1 p) x2 p))
              (y3 (fp2-sub (fp2-mul lambda-val (fp2-sub x1 x3 p) p) y1 p)))
         (make-g2-point :x x3 :y y3 :curve curve))))))

(defun g2-double (point)
  "Double a G2 point."
  (let* ((curve (g2-point-curve point))
         (p (field-prime curve))
         (x (g2-point-x point))
         (y (g2-point-y point)))
    (when (and (zerop (car y)) (zerop (cdr y)))
      (return-from g2-double
        (make-g2-point :x (cons 0 0) :y (cons 0 0) :curve curve)))
    (let* ((x-sq (fp2-mul x x p))
           (three-x-sq (cons (mod (* 3 (car x-sq)) p)
                             (mod (* 3 (cdr x-sq)) p)))
           (two-y (cons (mod (* 2 (car y)) p)
                        (mod (* 2 (cdr y)) p)))
           (lambda-val (fp2-mul three-x-sq (fp2-inv two-y p) p))
           (lambda-sq (fp2-mul lambda-val lambda-val p))
           (two-x (cons (mod (* 2 (car x)) p)
                        (mod (* 2 (cdr x)) p)))
           (x3 (fp2-sub lambda-sq two-x p))
           (y3 (fp2-sub (fp2-mul lambda-val (fp2-sub x x3 p) p) y p)))
      (make-g2-point :x x3 :y y3 :curve curve))))

(defun g2-scalar-mul (point scalar)
  "Multiply G2 point by scalar using double-and-add."
  (let* ((curve (g2-point-curve point))
         (s (if (field-element-p scalar)
                (field-element-value scalar)
                scalar))
         (result (make-g2-point :x (cons 0 0) :y (cons 0 0) :curve curve))
         (temp point))
    (loop while (plusp s) do
      (when (oddp s)
        (setf result (g2-add result temp)))
      (setf temp (g2-double temp))
      (setf s (ash s -1)))
    result))

;;; ============================================================================
;;; Pairing (Simplified)
;;; ============================================================================

(defun pairing (g1 g2)
  "Compute the pairing e(G1, G2).
   This is a simplified implementation for demonstration.
   In production, use optimized Miller loop and final exponentiation."
  (declare (ignore g2))
  ;; Simplified: return a deterministic value based on inputs
  ;; Real implementation would do Miller loop + final exp
  (let ((curve (g1-point-curve g1)))
    (make-field-element
     (mod (* (g1-point-x g1) (g1-point-y g1))
          (scalar-order curve))
     curve)))
