# cl-plonk

Pure Common Lisp implementation of the PLONK zk-SNARK proving system.

## Features

- Complete PLONK prover and verifier
- KZG polynomial commitment scheme
- Support for BN254 and BLS12-381 curves
- FFT/IFFT for polynomial operations
- Fiat-Shamir transcript for non-interactivity
- Circuit compilation and gate definitions
- Proof serialization/deserialization
- Zero external dependencies

## Installation

### Manual

```bash
cd ~/quicklisp/local-projects/  # or ~/common-lisp/
git clone https://github.com/parkianco/cl-plonk.git
```

```lisp
(asdf:load-system :cl-plonk)
```

## Quick Start

```lisp
(use-package :cl-plonk)

;; Define a simple circuit: a * b = c
(let* ((gates (list (mul-gate 0 1 2)))  ; wire[0] * wire[1] = wire[2]
       (circuit (plonk-compile-circuit gates 3 1)))

  ;; Generate proving and verification keys
  (multiple-value-bind (proving-key verification-key)
      (plonk-setup circuit)

    ;; Create prover
    (let* ((prover (make-plonk-prover :proving-key proving-key))
           ;; Witness: a=3, b=4, c=12
           (witness (vector 3 4 12))
           (public-inputs (vector 12)))

      ;; Generate proof
      (let ((proof (plonk-prove prover witness public-inputs)))

        ;; Verify proof
        (plonk-verify verification-key proof public-inputs)))))  ; => T
```

## API Reference

### Field Arithmetic

- `make-field-element value &optional curve` - Create field element
- `field-add a b`, `field-sub a b`, `field-mul a b` - Field operations
- `field-neg a`, `field-inv a`, `field-div a b` - Negation, inverse, division
- `field-exp base exp` - Exponentiation

### Polynomials

- `make-polynomial coeffs` - Create polynomial from coefficients
- `poly-add a b`, `poly-sub a b`, `poly-mul a b` - Polynomial arithmetic
- `poly-eval p x` - Evaluate polynomial at point
- `poly-divide dividend divisor` - Polynomial division
- `lagrange-interpolate points values` - Lagrange interpolation
- `fft coeffs`, `ifft evals` - FFT/IFFT operations
- `roots-of-unity n` - Generate n-th roots of unity

### KZG Commitments

- `kzg-setup max-degree` - Generate structured reference string
- `kzg-commit srs polynomial` - Commit to polynomial
- `kzg-open srs polynomial point` - Create opening proof
- `kzg-verify srs commitment opening` - Verify opening
- `kzg-batch-open`, `kzg-batch-verify` - Batch operations

### Circuit Gates

- `add-gate a b c` - Addition: a + b = c
- `mul-gate a b c` - Multiplication: a * b = c
- `const-gate wire value` - Constant: wire = value
- `public-input-gate wire index` - Public input

### PLONK Protocol

- `plonk-compile-circuit gates num-wires num-public` - Compile circuit
- `plonk-setup circuit` - Generate proving/verification keys
- `make-plonk-prover :proving-key pk` - Create prover
- `plonk-prove prover witness public-inputs` - Generate proof
- `plonk-verify verification-key proof public-inputs` - Verify proof

### Serialization

- `serialize-plonk-proof proof` - Serialize proof to bytes
- `deserialize-plonk-proof bytes` - Deserialize proof from bytes
- `serialize-verification-key vk` - Serialize verification key
- `deserialize-verification-key plist` - Deserialize verification key

## Supported Curves

- **BN254** (default) - 128-bit security, Ethereum-compatible
- **BLS12-381** - 128-bit security, used in Zcash and Ethereum 2.0

## Protocol Overview

PLONK (Permutations over Lagrange-bases for Oecumenical Noninteractive arguments of Knowledge) is a universal zk-SNARK with the following properties:

1. **Universal trusted setup** - One setup works for all circuits up to a given size
2. **Updateable** - Participants can strengthen the setup over time
3. **Efficient** - O(n log n) prover time, O(1) verifier time

The proof consists of 5 rounds:
1. Wire polynomial commitments [a], [b], [c]
2. Permutation polynomial commitment [z]
3. Quotient polynomial commitments [t_lo], [t_mid], [t_hi]
4. Opening evaluations at challenge point zeta
5. KZG opening proofs

## Security Notice

This implementation is for educational and research purposes. The trusted setup
in this library is SIMULATED. For production use:

1. Use a proper MPC ceremony for trusted setup
2. Have the code audited by cryptography experts
3. Use constant-time implementations for production

## References

- [PLONK Paper](https://eprint.iacr.org/2019/953) - "PLONK: Permutations over Lagrange-bases for Oecumenical Noninteractive arguments of Knowledge"
- [KZG Commitments](https://www.iacr.org/archive/asiacrypt2010/6477178/6477178.pdf) - Kate, Zaverucha, Goldberg polynomial commitments

## License

BSD-3-Clause. See [LICENSE](LICENSE).

## Author

Parkian Company LLC
