;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; PLONK zk-SNARK package definition

(defpackage #:cl-plonk
  (:use #:cl)
  (:export
   ;; Constants
   #:+plonk-protocol-version+
   #:+plonk-default-curve+

   ;; Conditions
   #:plonk-error
   #:plonk-error-message

   ;; Field arithmetic
   #:field-element
   #:field-element-p
   #:field-element-value
   #:field-element-curve
   #:make-field-element
   #:field-add
   #:field-sub
   #:field-mul
   #:field-neg
   #:field-inv
   #:field-div
   #:field-exp
   #:field-prime
   #:scalar-order

   ;; Curve points
   #:g1-point
   #:g1-point-p
   #:g1-point-x
   #:g1-point-y
   #:g1-point-curve
   #:make-g1-point
   #:g2-point
   #:g2-point-p
   #:g2-point-x
   #:g2-point-y
   #:g2-point-curve
   #:make-g2-point
   #:g1-add
   #:g1-double
   #:g1-scalar-mul
   #:g2-add
   #:g2-double
   #:g2-scalar-mul
   #:g1-generator
   #:g2-generator
   #:pairing

   ;; Polynomials
   #:polynomial
   #:polynomial-p
   #:polynomial-coeffs
   #:make-polynomial
   #:poly-add
   #:poly-sub
   #:poly-mul
   #:poly-scale
   #:poly-eval
   #:poly-degree
   #:poly-zero-p
   #:poly-divide
   #:poly-mod
   #:vanishing-polynomial
   #:lagrange-basis
   #:lagrange-interpolate
   #:roots-of-unity
   #:fft
   #:ifft
   #:coset-fft
   #:coset-ifft

   ;; KZG Commitments
   #:kzg-srs
   #:kzg-srs-p
   #:kzg-srs-g1-powers
   #:kzg-srs-g2-powers
   #:kzg-srs-max-degree
   #:make-kzg-srs
   #:kzg-setup
   #:kzg-commit
   #:kzg-open
   #:kzg-verify
   #:kzg-batch-open
   #:kzg-batch-verify
   #:kzg-commitment
   #:kzg-commitment-p
   #:kzg-opening
   #:kzg-opening-p

   ;; PLONK Circuit
   #:plonk-gate
   #:plonk-gate-p
   #:plonk-gate-type
   #:plonk-gate-wires
   #:plonk-gate-selectors
   #:make-plonk-gate
   #:plonk-circuit
   #:plonk-circuit-p
   #:plonk-circuit-gates
   #:plonk-circuit-num-wires
   #:plonk-circuit-num-public
   #:make-plonk-circuit
   #:add-gate
   #:mul-gate
   #:const-gate
   #:public-input-gate

   ;; PLONK Keys
   #:plonk-proving-key
   #:plonk-proving-key-p
   #:make-plonk-proving-key
   #:plonk-verification-key
   #:plonk-verification-key-p
   #:make-plonk-verification-key

   ;; PLONK Proof
   #:plonk-proof
   #:plonk-proof-p
   #:plonk-proof-a
   #:plonk-proof-b
   #:plonk-proof-c
   #:plonk-proof-z
   #:plonk-proof-t-lo
   #:plonk-proof-t-mid
   #:plonk-proof-t-hi
   #:plonk-proof-w-zeta
   #:plonk-proof-w-zeta-omega
   #:make-plonk-proof

   ;; Fiat-Shamir Transcript
   #:transcript
   #:transcript-p
   #:make-transcript
   #:transcript-append
   #:transcript-append-point
   #:transcript-append-scalar
   #:transcript-challenge

   ;; PLONK Setup
   #:plonk-setup
   #:plonk-compile-circuit

   ;; PLONK Proving and Verification
   #:plonk-prover
   #:plonk-prover-p
   #:make-plonk-prover
   #:plonk-prove
   #:plonk-verify

   ;; Serialization
   #:serialize-plonk-proof
   #:deserialize-plonk-proof
   #:serialize-verification-key
   #:deserialize-verification-key))
