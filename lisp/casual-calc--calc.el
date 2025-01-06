;;; casual-calc--calc.el --- Casual Wrapped Calc Functions  -*- lexical-binding: t; -*-

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

;; Wrapped Functions
;; Shortdoc
;;(define-short-documentation-group casual "Casual Shortdoc")
;;(shortdoc-add-function 'casual "Constants" '(casual-calc--pi :no-manual))

(defun casual-calc--round ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-round'.

* References
- info node `(calc) Integer Truncation'
- `calc-round'"
 (interactive)
 (call-interactively #'calc-round))

(defun casual-calc--floor ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-floor'.

* References
- info node `(calc) Integer Truncation'
- `calc-floor'"
 (interactive)
 (call-interactively #'calc-floor))

(defun casual-calc--ceiling ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-ceiling'.

* References
- info node `(calc) Integer Truncation'
- `calc-ceiling'"
 (interactive)
 (call-interactively #'calc-ceiling))

(defun casual-calc--trunc ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-trunc'.

* References
- info node `(calc) Integer Truncation'
- `calc-trunc'"
 (interactive)
 (call-interactively #'calc-trunc))


(defun casual-calc--plus ()
  "Add the top two numbers on the stack and leave the result at the top (a+b).

Stack Arguments:
2: a
1: b

This function is a wrapper over `calc-plus'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-plus'"
 (interactive)
 (call-interactively #'calc-plus))

(defun casual-calc--minus ()
  "Subtract the top two numbers on the stack and leave the result at the top (a-b).

Stack Arguments:
2: a
1: b

This function is a wrapper over `calc-minus'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-minus'"
 (interactive)
 (call-interactively #'calc-minus))

(defun casual-calc--times ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-times'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-times'"
 (interactive)
 (call-interactively #'calc-times))

(defun casual-calc--divide ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-divide'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-divide'"
 (interactive)
 (call-interactively #'calc-divide))

(defun casual-calc--mod ()
    "TODO: This function does not yet have a docstring.
\nStack Arguments:
1: n

This function is a wrapper over `calc-mod'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-mod'"
 (interactive)
 (call-interactively #'calc-mod))

(defun casual-calc--pi ()
  "Push the value of 𝜋 at the current precision onto the stack.
\nIf in symbolic mode, this command instead pushes the actual
variable \\='pi\\='.

This function is a wrapper over `calc-pi'.

* References
- info node `(calc) Scientific Functions'
- `calc-pi'"
  (interactive)
  (call-interactively #'calc-pi))

(defun casual-calc--e-constant ()
  "Constant 𝑒.

Depending on value of `calc-symbolic-mode', either enter numeric
or symbolic value of e."
  (interactive)
  (if calc-symbolic-mode
      (calc-pop-push-record 0 "e" '(var e var-e))
    (calc-pop-push-record 0 "e" (math-e))))

(defun casual-calc--inv ()
  "Invert the value n at the top of the stack.
\nIf n is a number, then compute its reciprocal. If n is a square
matrix, then compute the inverse of the matrix.
\nStack Arguments:
1: n

This function is a wrapper over `calc-inv'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-inv'"
  (interactive)
  (call-interactively #'calc-inv))

(defun casual-calc--sqrt ()
  "Compute the square root of a number.
\nFor a negative real argument, the result will be a complex
number whose form is determined by the current Polar mode.
\nStack Arguments:
1: n

This function is a wrapper over `calc-sqrt'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-sqrt'"
  (interactive)
  (call-interactively #'calc-sqrt))

(defun casual-calc--change-sign ()
  "Negate the number on the top of the stack.
\nThis works on numbers, vectors and matrices, HMS forms, date
forms, error forms, intervals, and modulo forms.

\nStack Arguments:
1: n

This function is a wrapper over `calc-change-sign'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-change-sign'"
  (interactive)
  (call-interactively #'calc-change-sign))

(defun casual-calc--power ()
  "Raise a number 𝑦 to a power 𝑥.
\nIf the power is an integer, an exact result is computed using
repeated multiplications. For non-integer powers, Calc uses
Newton's method or logarithms and exponentials. Square matrices
can be raised to integer powers. If either argument is an
error (or interval or modulo) form, the result is also an
error (or interval or modulo) form.
\nStack Arguments:
2: 𝑦
1: 𝑥

This function is a wrapper over `calc-power'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-power'"
  (interactive)
  (call-interactively #'calc-power))

(defun casual-calc--abs ()
  "Compute the absolute value of a number.
\nThe result is always a nonnegative real number: With a complex
argument, it computes the complex magnitude. With a vector or
matrix argument, it computes the Frobenius norm, i.e., the square
root of the sum of the squares of the absolute values of the
elements. The absolute value of an error form is defined by
replacing the mean part with its absolute value and leaving the
error part the same. The absolute value of a modulo form is
undefined. The absolute value of an interval is defined in the
obvious way.
\nStack Arguments: 1: n

This function is a wrapper over `calc-abs'.

* References
- info node `(calc) Basic Arithmetic'
- `calc-abs'"
  (interactive)
  (call-interactively #'calc-abs))


(defun casual-calc--factorial ()
  "Compute the factorial of the number at the top of the stack.
\nIf the number is an integer, the result is an exact integer. If
the number is an integer-valued float, the result is a
floating-point approximation. If the number is a non-integral
real number, the generalized factorial is used, as defined by the
Euler Gamma function. Please note that computation of large
factorials can be slow; using floating-point format will help
since fewer digits must be maintained.

\nStack Arguments:
1: n

This function is a wrapper over `calc-factorial'.

* References
- info node `(calc) Combinatorial Functions'
- `calc-factorial'"
  (interactive)
  (call-interactively #'calc-factorial))

(defun casual-calc--percent ()
  "Convert top of the stack into a percentage formula where the '%' is appended.
\nThe '%' operator means “the preceding value divided by 100”.

\nStack Arguments:
1: n

This function is a wrapper over `calc-percent'.

* References
- info node `(calc) Percentages'
- `calc-percent'"
  (interactive)
  (call-interactively #'calc-percent))

(defun casual-calc--percent-change ()
  "Calculate the percentage change from one number to another.
\nCalculates the percentage change from a previous value (p) to a
current value (c).
\nStack Arguments:
2: p
1: c

This function is a wrapper over `calc-percent-change'.

* References
- info node `(calc) Percentages'
- `calc-percent-change'"
  (interactive)
  (call-interactively #'calc-percent-change))

(defun casual-calc--evaluate ()
  "Evaluate a formula by replacing all variables with their stored values.
\nIf a variable in the formula does not have a stored value, it is left alone.
\nStack Arguments:
1: n

This function is a wrapper over `calc-evaluate'.

* References
- info node `(calc) Variables'
- `calc-evaluate'"
  (interactive)
  (call-interactively #'calc-evaluate))

(defun casual-calc--store ()
  "Store the value at the top of the stack into a specified variable.
\nStack Arguments:
1: n

This function is a wrapper over `calc-store'.

* References
- info node `(calc) Storing Variables'
- `calc-store'"
  (interactive)
  (call-interactively #'calc-store))

(defun casual-calc--recall ()
  "Recall a stored variable.
\nThis command prompts for a variable name that was stored
using `casual-calc--store'.
\nThis function is a wrapper over `calc-recall'.

* References
- info node `(calc) Recalling Variables'
- `calc-recall'"
  (interactive)
  (call-interactively #'calc-recall))

(defun casual-calc--unstore ()
  "Unstore (clears) a stored variable.
\nComplimentary command to `casual-calc--store'.
\nThis function is a wrapper over `calc-unstore'.

* References
- info node `(calc) Storing Variables'
- `calc-unstore'"
  (interactive)
  (call-interactively #'calc-unstore))

(defun casual-calc--edit-variable ()
  "Edits the stored value of a variable.
\nThe value of the stored variable is modified without putting
the value on the stack or simplifying or evaluating the value.
This command will prompt for the name of the variable to edit.
\nThis function is a wrapper over `calc-edit-variable'.

* References
- info node `(calc) Operations on Variables'
- `calc-edit-variable'"
  (interactive)
  (call-interactively #'calc-edit-variable))

(defun casual-calc--copy-variable ()
  "Copies the stored value of one variable to another.
\nThis function is a wrapper over `calc-copy-variable'.

* References
- info node `(calc) Storing Variables'
- `calc-copy-variable'"
  (interactive)
  (call-interactively #'calc-copy-variable))

(defun casual-calc--store-exchange ()
  "Exchanges the value of a stored variable with the top of the stack.
\nStack Arguments:
1: n

This function is a wrapper over `calc-store-exchange'.

* References
- info node `(calc) Storing Variables'
- `calc-store-exchange'"
  (interactive)
  (call-interactively #'calc-store-exchange))

(defun casual-calc--permanent-variable ()
  "Persist a stored variable so that it is available across restarts of Emacs.
\nTypically file stored variables are persisted to is
‘~/<emacs config path>/calc.el’.
To remove a persisted stored variable, one must edit this file.
\nThis function is a wrapper over `calc-permanent-variable'.

* References
- info node `(calc) Operations on Variables'
- `calc-permanent-variable'"
  (interactive)
  (call-interactively #'calc-permanent-variable))

(defun casual-calc--insert-variables ()
  "Write all stored variables into a specified buffer.
\nThe variables are written with the prefix ‘var-’ in the form of
Lisp ‘setq’ commands which store the values in string form.
\nThis function is a wrapper over `calc-insert-variables'.

* References
- info node `(calc) Operations on Variables'
- `calc-insert-variables'"
  (interactive)
  (call-interactively #'calc-insert-variables))

(provide 'casual-calc--calc)
;;; casual-calc--calc.el ends here
