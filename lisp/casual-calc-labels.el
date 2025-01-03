;;; casual-calc-labels.el --- Casual Labels               -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'calc)
(require 'calc-math)
(require 'casual-calc-utils)

;; !!! While not a label function, it is called from modules that call labels.
(defun casual-calc-open-settings-file ()
  "Open Calc settings file in other window.
\nThis function opens in another window the file defined in
`calc-settings-file'.  Persisted variables stored in this file
can be subsequently edited or removed."
  (interactive)
  (find-file-other-window calc-settings-file))

;; Labels
(defun casual-calc-cmplx-or-polar-label ()
  "Label for either complex or polar mode."
  (if (eq calc-complex-mode 'polar)
      "Change to Complex Mode (now Polar)"
    "Change to Polar Mode (now Complex)"))

(defun casual-calc-symbolic-mode-label ()
  "Label for symbolic mode."
  (if calc-symbolic-mode
      "Change to Numeric Mode (now Symbolic)"
    "Change to Symbolic Mode (now Numeric)"))

(defun casual-calc-prefer-frac-label ()
  "Label for fractional or floating point mode."
  (if calc-prefer-frac
      "Change to Floating Point Results (now Fractional)"
    "Change to Fractional Results (now Floating Point)"))

(defun casual-calc-number-radix-label ()
  "Label for number radix."
  (cond
   ((= calc-number-radix 10) "Decimal")
   ((= calc-number-radix 2) "Binary")
   ((= calc-number-radix 8) "Octal")
   ((= calc-number-radix 16) "Hexadecimal")
   (t (format "%d" calc-number-radix))))

(defun casual-calc-matrix-mode-label ()
  "Label for matrix mode."
  (cond
   ((eq calc-matrix-mode 'matrix) "Matrix")
   ((eq calc-matrix-mode 'sqmatrix) "Square Matrix")
   ((eq calc-matrix-mode 'scalar) "Scalar")
   ((eq calc-matrix-mode 'nil) "No assumptions")
   ((integerp calc-matrix-mode) (format "%dx%d"
                                        calc-matrix-mode
                                        calc-matrix-mode))))

(defun casual-calc-angle-mode-label ()
  "Label for angle mode."
  (cond
   ((eq calc-angle-mode 'deg) (casual-calc-unicode-get :degrees))
   ((eq calc-angle-mode 'rad) (casual-calc-unicode-get :radians))
   ((eq calc-angle-mode 'hms) "hms")))

(defun casual-calc-complex-format-label ()
  "Label for complex format mode."
  (cond
   ((eq calc-complex-format 'i) "x + yi")
   ((eq calc-complex-format 'j) "x + yj")
   ((not calc-complex-format) "(x, y)")))

(defun casual-calc-float-format-label (&optional include-precision)
  "Label for Calc float mode.
If INCLUDE-PRECISION is non-nil, then add precision to label."
  (let ((mode (pcase (car calc-float-format)
                ('float "Normal")
                ('fix "Fixed Point")
                ('sci "Scientific")
                ('eng "Engineering")))
        (precision (nth 1 calc-float-format)))

    (if include-precision
        (format "%s %d" mode precision)
      (format "%s" mode))))

(provide 'casual-calc-labels)
;;; casual-calc-labels.el ends here
