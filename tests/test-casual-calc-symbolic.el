;;; test-casual-calc-symbolic.el --- Casual Symbolic Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-symbolic)

(ert-deftest test-casual-calc-symbolic-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("E" . casual-calc--alg-evaluate)
                        ("=" . casual-calc--evaluate)
                        ("m" . casual-calc-symbolic-manipulation-tmenu)
                        ("F" . casual-calc-subformula-tmenu)
                        ("f" . casual-calc--factor)
                        ("e" . casual-calc--expand)
                        ("p" . casual-calc-polynomial-tmenu)
                        ("d" . casual-calc--derivative)
                        ("i" . casual-calc--integral)
                        ("c" . casual-calc--calculus-tmenu)
                        ("s" . casual-calc-solve-symbolic-tmenu)
                        ("n" . casual-calc-solve-numeric-tmenu)
                        ("C" . casual-calc-curve-fit-tmenu)
                        ("S" . casual-calc-summations-tmenu)
                        ("l" . casual-calc-symbolic-logic-tmenu)
                        ("g" . casual-calc-plot-tmenu)
                        ;;("A" . calc-algebraic-mode)
                        ;;("M" . calc-symbolic-mode)
                        ("a" . casual-calc-angle-measure-tmenu))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-symbolic-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc-subformula-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("s" . casual-calc--select-here)
                        ("o" . casual-calc--select-once)
                        ("m" . casual-calc--select-more)
                        ("p" . casual-calc--select-previous)
                        ("u" . casual-calc--unselect)
                        ("c" . casual-calc--clear-selections)
                        ("l" . casual-calc--select-less)
                        ("n" . casual-calc--select-next)
                        ("b" . casual-calc--commute-left)
                        ("d" . casual-calc--sel-distribute)
                        ("i" . casual-calc--sel-isolate)
                        ("N" . casual-calc--sel-negate)
                        ("e" . casual-calc--sel-jump-equals)
                        ("f" . casual-calc--commute-right)
                        ("M" . casual-calc--sel-merge)
                        ("&" . casual-calc--sel-invert)
                        ("=" . casual-calc--sel-evaluate)
                        ;; ("`" . casual-calc--edit-selection)
                        ("C" . casual-calc--copy-selection)
                        ;; ("'" . casual-calc--enter-selection)
                        ("D" . casual-calc--del-selection))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-subformula-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-symbolic-manipulation-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("E" . casual-calc--alg-evaluate)
                        ("=" . casual-calc--evaluate)
                        ("e" . casual-calc--expand-formula)
                        ("m" . casual-calc--map-equation)
                        ("s" . casual-calc--substitute))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-symbolic-manipulation-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-polynomial-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("f" . casual-calc--factor)
                        ("e" . casual-calc--expand)
                        ("c" . casual-calc--collect)
                        ("a" . casual-calc--apart)
                        ("n" . casual-calc--normalize-rat)
                        ("\\" . casual-calc--poly-div)
                        ("%" . casual-calc--poly-rem)
                        ("/" . casual-calc--poly-div-rem)
                        ("g" . casual-calc--poly-gcd))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-polynomial-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc--calculus-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("n" . casual-calc--num-integral)
                        ("t" . casual-calc--taylor)
                        ("d" . casual-calc--derivative)
                        ("i" . casual-calc--integral))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc--calculus-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-solve-symbolic-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("s" . casual-calc--solve-for)
                        ("p" . casual-calc--poly-roots))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-solve-symbolic-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-solve-numeric-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("r" . casual-calc--find-root)
                        ("m" . casual-calc--find-minimum)
                        ("x" . casual-calc--find-maximum)
                        ("h" . casual-calc--head)
                        ("w" . casual-calc--why))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-solve-numeric-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-curve-fit-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("c" . casual-calc--curve-fit)
                        ("p" . casual-calc--poly-interp))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-curve-fit-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-summations-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("s" . casual-calc--summation)
                        ("a" . casual-calc--alt-summation)
                        ("p" . casual-calc--product)
                        ("t" . casual-calc--tabulate))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-summations-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-symbolic-logic-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("=" . casual-calc--equal-to)
                        ("l" . casual-calc--less-than)
                        ("g" . casual-calc--greater-than)
                        ("n" . casual-calc--not-equal-to)
                        ("L" . casual-calc--less-equal)
                        ("G" . casual-calc--greater-equal)
                        ("x" . casual-calc--remove-equal)
                        ("!" . casual-calc--logical-not)
                        ("&" . casual-calc--logical-and)
                        ("|" . casual-calc--logical-or)
                        ("e" . casual-calc--in-set)
                        ("i" . casual-calc--logical-if))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-symbolic-logic-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(provide 'test-casual-calc-symbolic)
