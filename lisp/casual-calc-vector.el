;;; casual-calc-vector.el --- Casual Vector Menu          -*- lexical-binding: t; -*-

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
(require 'casual-calc-predicates)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-vector-tmenu ()
  "Casual vector and matrix functions top-level menu."
  ["Vector & Matrix Functions (index is 1-offset)\n"
   ["Categories"
   ("b" "Building›" casual-calc-vector-building-tmenu)
   ("a" "Arithmetic›" casual-calc-vector-arithmetic-tmenu)
   ("s" "Statistics›" casual-calc-statistics-tmenu)
   ("S" "Set Operations›" casual-calc-set-operations-tmenu)
   ("m" "Map, Reduce, Apply›" casual-calc-map-and-reduce-tmenu)]

   ["Manipulate"
    :pad-keys t
    ("l" "Length" calc-vlength :transient t)
    ("t" "Transpose" calc-transpose :transient t)
    ("v" "Reverse" calc-reverse-vector :transient t)
    ("o" "Sort" calc-sort :transient t)
    ("d" "Deduplicate" calc-remove-duplicates :transient t)]

   ["Extract and Pack"
    ("r" "Extract Row…" calc-mrow :transient t)
    ("c" "Extract Column…" calc-mcol :transient t)
    ("p" "Pack (𝑛)" calc-pack :transient t)
    ("u" "Unpack" calc-unpack :transient t)]

   casual-calc-operators-group]
  casual-calc-navigation-group)

(transient-define-prefix casual-calc-vector-building-tmenu ()
  "Casual vector building functions menu."
  ["Vector Building (index is 1-offset, 𝑛 is a prompt value)\n"
   ["Build"
    ("|" "Concat" calc-concat :transient t)
    ("i" "index (1..𝑛)…" calc-index :transient t)
    ("e" "Enumerate Interval" calc-set-enumerate :transient t)
    ("I" "Identity 𝑛…" calc-ident :transient t)
    ("d" "Diagonal (𝟣:)" calc-diag :transient t)
    ("b" "Build Vector 𝑛…" calc-build-vector :transient t)]

   ["Manipulate"
    ("t" "Transpose" calc-transpose :transient t)
    ("r" "Reverse" calc-reverse-vector :transient t)
    ("a" "Vector Arrange" calc-arrange-vector :transient t)
    ("s" "Sort" calc-sort :transient t)
    ("p" "Deduplicate" calc-remove-duplicates :transient t)]

   ["Miscellaneous"
    ("l" "Length" calc-vlength :transient t)
    ("c" "Vector Count" calc-vector-count :transient t)
    ("f" "Vector Find (𝟣:)" calc-vector-find :transient t)
    ("h" "Histogram" calc-histogram :transient t)]

   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-vector-arithmetic-tmenu ()
  "Casual vector arithmetic functions menu."
  [["Arithmetic (index is 1-offset)\n"
    ("t" "Conjugate Transpose" calc-conj-transpose :transient t)
    ("A" "Frobenius Norm (|𝑛|)" calc-abs :transient t)
    ("r" "Row Norm" calc-rnorm :transient t)
    ("c" "Column Norm" calc-cnorm :transient t)
    ("p" "RH Cross Product" calc-cross :inapt-if-not casual-calc-crossp :transient t)
    ("k" "Kronecker Product" calc-kron :inapt-if-not casual-calc-matrixmultp :transient t)]
   ["Square Matrix"
    ("&" "Inverse" calc-inv :inapt-if-not casual-calc-square-matrixp :transient t)
    ("d" "Determinant" calc-mdet :inapt-if-not casual-calc-square-matrixp  :transient t)
    ("l" "LU Decomposition" calc-mlud :inapt-if-not casual-calc-square-matrixp :transient t)
    ("T" "Trace" calc-mtrace :inapt-if-not casual-calc-square-matrixp :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

;; TODO: add Transient prefix arguments n
(transient-define-prefix casual-calc-statistics-tmenu ()
  "Casual statistic functions menu."
  ["Statistics (index is 1-offset, 𝑛 is 𝟣: on stack)\n"
   ["Mean and Error"
    ("c" "Vector Count" calc-vector-count :transient t)
    ("s" "Sum" calc-vector-sum :transient t)
    ("x" "Max" calc-vector-max :transient t)
    ("m" "Mean" calc-vector-mean :transient t)
    ("h" "Histogram…" casual-calc--histogram :transient t)
    ("e" "Mean Error" calc-vector-mean-error :transient t)
    ("M" "Median" calc-vector-median :transient t)
    ("H" "Harmonic Mean" calc-vector-harmonic-mean :transient t)
    ("g" "Geometric Mean" calc-vector-geometric-mean :transient t)]

   ["Deviation and Variance"
    ("r" "Root Mean Square" calc-vector-rms :transient t)
    ("1" "Standard Deviation" calc-vector-sdev :transient t)
    ("2" "Population Standard Deviation" calc-vector-pop-sdev :transient t)
    ("3" "Variance" calc-vector-variance :transient t)
    ("4" "Population Variance" calc-vector-pop-variance :transient t)]

   ["Paired-Sample Statistics" ; predicate for two vectors of the same size
    ("5" "Covariance" calc-vector-covariance :transient t)
    ("6" "Population Covariance" calc-vector-pop-covariance :transient t)
    ("7" "Correlation" calc-vector-correlation :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-set-operations-tmenu ()
  "Casual set functions menu."
  [["Set Operations"
    ("d" "Deduplicate" calc-remove-duplicates :transient t)
    ("u" "Union" calc-set-union :transient t)
    ("i" "Intersect" calc-set-intersect :transient t)
    ("D" "Difference" calc-set-difference :transient t)
    ("x" "xor" calc-set-xor :transient t)
    ("~" "Complement" calc-set-complement :transient t)
    ("#" "Cardinality" calc-set-cardinality :transient t)]
   casual-calc-operators-group]
  casual-calc-navigation-group)

(transient-define-prefix casual-calc-map-and-reduce-tmenu ()
  "Casual functional operations (map, reduce, apply) menu."
  [["Functional Operators"
   ("m" "map" calc-map :transient t)
   ("r" "reduce" calc-reduce :transient t)
   ("a" "apply" calc-apply :transient t)
   ("A" "accumulate" calc-accumulate :transient t)]
   casual-calc-operators-group]
  casual-calc-navigation-group)

;;; Wrapped Functions

(defun casual-calc--histogram ()
  "Build histogram of (1:).
\nGiven a vector data set in (1:), this command will prompt the
user for a bin specification vector, where each element of the
vector is a center point of a bin. For example, if the entered
bin vector is '[a, b, c, …]' then the bin ranges will be computed
as (-inf, (a+b)/2], ((a+b)/2, (b+c)/2], …

* Example

Start with the following data set of integer numbers from 1 to 100.

1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66,
    67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
    83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
    99, 100]

A histogram of the above data set where each bin is every 10
units requires this bin vector. Enter this when prompted:

[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

The result on the stack is:

1: [5, 10, 10, 10, 10, 10, 10, 10, 10, 10, 5]

* References - info node `(calc) Manipulating Vectors' -
`calc-histogram'"
  (interactive)
  (call-interactively #'calc-histogram))

(provide 'casual-calc-vector)
;;; casual-calc-vector.el ends here
