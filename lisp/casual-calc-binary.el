;;; casual-calc-binary.el --- Casual Binary Menu          -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-labels)
(require 'casual-calc-radix)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-binary-tmenu ()
  "Casual binary functions menu."
  ["Binary Functions"
   ["Operators"
    ("&" "and" casual-calc--and :transient t)
    ("|" "or" casual-calc--or :transient t)
    ("^" "xor" casual-calc--xor :transient t)
    ("d" "diff" casual-calc--diff :transient t)
    ("!" "not" casual-calc--not :transient t)]
   ["Shift"
    :pad-keys t
    ("l" "binary left" casual-calc--lshift-binary :transient t)
    ("r" "binary right" casual-calc--rshift-binary :transient t)
    ("M-l" "arithmetic left" casual-calc--lshift-arith :transient t)
    ("M-r" "arithmetic right" casual-calc--rshift-arith :transient t)
    ("C-r" "rotate binary" casual-calc--rotate-binary :transient t)]

   casual-calc-operators-group

   ["Utils"
    ("R" casual-calc-radix-tmenu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-calc-number-radix-label)))
     :transient t)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual-lib-checkbox-label calc-leading-zeros "Leading Zeroes"))
     :transient t)
    ("," "Thousands Separator…" calc-group-char
     :description (lambda ()
                    (format "Set Thousands Separator (now %s)…" calc-group-char))
     :transient t)
    ("w" "Set Word Size…" casual-calc--word-size :transient t)
    ("u" "Unpack Bits" casual-calc--unpack-bits :transient t)
    ("p" "Pack Bits" casual-calc--pack-bits :transient t)]]

  casual-calc-navigation-group)


;; Wrapped Calc Functions

(defun casual-calc--and ()
  "Computes the bitwise AND of the two numbers on the top of the stack.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-and'.

* References
- info node `(calc) Binary Functions'
- `calc-and'"
 (interactive)
 (call-interactively #'calc-and))

(defun casual-calc--or ()
  "Computes the bitwise inclusive OR of two numbers.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-or'.

* References
- info node `(calc) Binary Functions'
- `calc-or'"
 (interactive)
 (call-interactively #'calc-or))

(defun casual-calc--xor ()
  "Computes the bitwise exclusive OR of two numbers.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-xor'.

* References
- info node `(calc) Binary Functions'
- `calc-xor'"
 (interactive)
 (call-interactively #'calc-xor))

(defun casual-calc--diff ()
  "Computes the bitwise difference of two numbers.
\nStack Arguments:
2: a
1: b

This is defined by ‘diff(a,b) = and(a,not(b))’, so that
‘diff(2#1100, 2#1010) = 2#0100’.

This function is a wrapper over `calc-diff'.

* References
- info node `(calc) Binary Functions'
- `calc-diff'"
 (interactive)
 (call-interactively #'calc-diff))

(defun casual-calc--not ()
  "Computes the bitwise NOT of a number.
\nStack Arguments:
1: n

A bit is 1 if the input bit is 0 and vice-versa.

This function is a wrapper over `calc-not'.

* References
- info node `(calc) Binary Functions'
- `calc-not'"
 (interactive)
 (call-interactively #'calc-not))

(defun casual-calc--lshift-binary ()
  "Shifts a number left by one bit.
\nStack Arguments:
1: n

This function is a wrapper over `calc-lshift-binary'.

* References
- info node `(calc) Binary Functions'
- `calc-lshift-binary'"
 (interactive)
 (call-interactively #'calc-lshift-binary))

(defun casual-calc--rshift-binary ()
    "Shifts a number right by one bit.
\nStack Arguments:
1: n

This function is a wrapper over `calc-rshift-binary'.

* References
- info node `(calc) Binary Functions'
- `calc-rshift-binary'"
 (interactive)
 (call-interactively #'calc-rshift-binary))

(defun casual-calc--lshift-arith ()
  "Arithmetic shift left by one bit.
\nStack Arguments:
1: n

This function is a wrapper over `calc-lshift-arith'.

* References
- info node `(calc) Binary Functions'
- `calc-lshift-arith'"
 (interactive)
 (call-interactively #'calc-lshift-arith))

(defun casual-calc--rshift-arith ()
  "Arithmetic shift right by one bit.
\nStack Arguments:
1: n

This command performs an “arithmetic” shift to the right, in
which the leftmost bit (according to the current word size) is
duplicated rather than shifting in zeros. This corresponds to
dividing by a power of two where the input is interpreted as a
signed, twos-complement number. (The distinction between the
‘binary’ and ‘arithmetic’ operations is totally independent from
whether the word size is positive or negative.)

This function is a wrapper over `calc-rshift-arith'.

* References
- info node `(calc) Binary Functions'
- `calc-rshift-arith'"
 (interactive)
 (call-interactively #'calc-rshift-arith))

(defun casual-calc--rotate-binary ()
  "Rotates a number one bit to the left.
\nStack Arguments:
1: n

The leftmost bit (according to the current word size) is dropped
off the left and shifted in on the right.

This function is a wrapper over `calc-rotate-binary'.

* References
- info node `(calc) Binary Functions'
- `calc-rotate-binary'"
 (interactive)
 (call-interactively #'calc-rotate-binary))

(defun casual-calc--word-size ()
    "Set the word size from a prompt.

This command displays a prompt with the current word size; press
<RET> immediately to keep this word size, or type a new word size
at the prompt. Default word size is 32.

This function is a wrapper over function `calc-word-size'.

* References
- info node `(calc) Binary Functions'
- function `calc-word-size'"
 (interactive)
 (call-interactively #'calc-word-size))

(defun casual-calc--unpack-bits ()
    "Unpack into set of bit indexes.
\nStack Arguments:
1: n

This function is a wrapper over `calc-unpack-bits'.

* References
- info node `(calc) Binary Functions'
- `calc-unpack-bits'"
 (interactive)
 (call-interactively #'calc-unpack-bits))

(defun casual-calc--pack-bits ()
    "Pack set of bit indexes into a number.
\nStack Arguments:
1: n

This function is a wrapper over `calc-pack-bits'.

* References
- info node `(calc) Binary Functions'
- `calc-pack-bits'"
 (interactive)
 (call-interactively #'calc-pack-bits))

(provide 'casual-calc-binary)
;;; casual-calc-binary.el ends here
