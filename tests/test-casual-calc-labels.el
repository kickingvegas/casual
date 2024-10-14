;;; test-casual-calc-labels.el --- Casual Label Tests     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ert)
(require 'casual-calc-test-utils)
(require 'casual-calc-labels)

;;; Labels
(ert-deftest test-casual-calc-cmplx-or-polar-label ()
  (casualt-calc-setup)
  (setq calc-complex-mode 'polar)
  (should (equal (casual-calc-cmplx-or-polar-label)
                 "Change to Complex Mode (now Polar)"))
  (setq calc-complex-mode 'cmplx)
  (should (equal (casual-calc-cmplx-or-polar-label)
                 "Change to Polar Mode (now Complex)"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-symbolic-mode-label ()
  (casualt-calc-setup)
  (setq calc-symbolic-mode t)
  (should (equal (casual-calc-symbolic-mode-label)
                 "Change to Numeric Mode (now Symbolic)"))
  (setq calc-symbolic-mode nil)
  (should (equal (casual-calc-symbolic-mode-label)
                 "Change to Symbolic Mode (now Numeric)"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-prefer-frac-label ()
  (casualt-calc-setup)
  (setq calc-prefer-frac t)
  (should (equal (casual-calc-prefer-frac-label)
      "Change to Floating Point Results (now Fractional)"))
  (setq calc-prefer-frac nil)
  (should (equal (casual-calc-prefer-frac-label)
    "Change to Fractional Results (now Floating Point)"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-number-radix-label ()
  (casualt-calc-setup)
  (setq calc-number-radix '2)
  (should (equal (casual-calc-number-radix-label) "Binary"))
  (setq calc-number-radix '8)
  (should (equal (casual-calc-number-radix-label) "Octal"))
  (setq calc-number-radix '16)
  (should (equal (casual-calc-number-radix-label) "Hexadecimal"))
  (setq calc-number-radix '7)
  (should (equal (casual-calc-number-radix-label) "7"))
  (setq calc-number-radix '10)
  (should (equal (casual-calc-number-radix-label) "Decimal"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-matrix-mode-label ()
  (casualt-calc-setup)
  (setq calc-matrix-mode 'matrix)
  (should (equal (casual-calc-matrix-mode-label) "Matrix"))
  (setq calc-matrix-mode 'sqmatrix)
  (should (equal (casual-calc-matrix-mode-label) "Square Matrix"))
  (setq calc-matrix-mode 'scalar)
  (should (equal (casual-calc-matrix-mode-label) "Scalar"))
  (setq calc-matrix-mode 7)
  (should (equal (casual-calc-matrix-mode-label) "7x7"))
  (setq calc-matrix-mode nil)
  (should (equal (casual-calc-matrix-mode-label) "No assumptions"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-angle-mode-label ()
  (casualt-calc-setup)
  (setq calc-angle-mode 'deg)
  (should (equal (casual-calc-angle-mode-label) "Degrees"))
  (setq calc-angle-mode 'rad)
  (should (equal (casual-calc-angle-mode-label) "Radians"))
  (setq calc-angle-mode 'hms)
  (should (equal (casual-calc-angle-mode-label) "hms"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-complex-format-label ()
  (casualt-calc-setup)
  (setq calc-complex-format 'i)
  (should (equal (casual-calc-complex-format-label) "x + yi"))
  (setq calc-complex-format 'j)
  (should (equal (casual-calc-complex-format-label) "x + yj"))
  (setq calc-complex-format nil)
  (should (equal (casual-calc-complex-format-label) "(x, y)"))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-float-format-label ()
  (casualt-calc-setup)
  (setq calc-float-format (list 'sci 0))
  (should (equal (casual-calc-float-format-label) "Scientific"))
  (setq calc-float-format (list 'eng 0))
  (should (equal (casual-calc-float-format-label) "Engineering"))
  (setq calc-float-format (list 'fix 0))
  (should (equal (casual-calc-float-format-label) "Fixed Point"))

  (setq calc-float-format (list 'sci 4))
  (should (equal (casual-calc-float-format-label t) "Scientific 4"))
  (setq calc-float-format (list 'eng 5))
  (should (equal (casual-calc-float-format-label t) "Engineering 5"))
  (setq calc-float-format (list 'fix 7))
  (should (equal (casual-calc-float-format-label t) "Fixed Point 7"))

  (setq calc-float-format (list 'float 0))
  (should (equal (casual-calc-float-format-label) "Normal"))
  (casualt-calc-breakdown t))

(provide 'test-casual-calc-labels)
;;; test-casual-calc-labels.el ends here
