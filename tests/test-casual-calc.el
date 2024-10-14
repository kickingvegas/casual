;;; test-casual-calc.el --- Tests for Casual Calc  -*- lexical-binding: t; -*-

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

;; Test suite for Casual package.

;; Note that tests are focused on verifying UI behavior and not intended to test
;; the `calc' package.

;; * Running the test suite

;; ** Command Line
;; Casual uses `ert' to manage its test suite. The test suite is intended to be
;; run in batch mode via a Makefile in the same directory as this file.
;; Invoke the following command in a shell to run the test suite.

;;     $ make tests

;; ** Interactive
;; Tests can be interactively run via `ert' by loading the following two files:
;; - casual-calc-test-utils.el
;; - test-casual.el

;; Refer to `ert' documentation for more detail on how use it.

;;; Code:
(require 'ert)
(require 'casual-calc)
(require 'casual-calc-test-utils)

;;; Tests

(ert-deftest test-casual-calc-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("&" . casual-calc--inv)
                        ("Q" . casual-calc--sqrt)
                        ("n" . casual-calc--change-sign)
                        ("^" . casual-calc--power)
                        ("=" . casual-calc--evaluate)
                        ("A" . casual-calc--abs)
                        ("!" . casual-calc--factorial)
                        ("%" . casual-calc--percent)
                        ("D" . casual-calc--percent-change)
                        ("p" . casual-calc--pi)
                        ("e" . casual-calc--e-constant)
                        ("m" . casual-calc-modes-tmenu)
                        ("S" . casual-calc-stack-display-tmenu)
                        ("ô" . casual-calc-trail-tmenu)
                        ("o" . casual-calc-rounding-tmenu)
                        ("c" . casual-calc-conversions-tmenu)
                        ("T" . casual-calc-time-tmenu)
                        ("i" . casual-calc-complex-number-tmenu)
                        ("R" . casual-calc-random-number-tmenu)
                        ("t" . casual-calc-trig-tmenu)
                        ("l" . casual-calc-logarithmic-tmenu)
                        ("b" . casual-calc-binary-tmenu)
                        ("v" . casual-calc-vector-tmenu)
                        ("u" . casual-calc-units-tmenu)
                        ("f" . casual-calc-financial-tmenu)
                        ("g" . casual-calc-plot-tmenu)
                        ("s" . casual-calc--stack-swap)
                        ("r" . casual-calc--stack-roll-all)
                        ("d" . casual-calc--stack-drop)
                        ("C" . casual-calc--stack-clear)
                        ("L" . casual-calc--stack-last)
                        ("w" . casual-calc--copy-as-kill)
                        ("z" . casual-calc-variable-crud-tmenu))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-tmenu
                                     '(lambda () (random 5000))))
  (execute-kbd-macro "q")
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-tmenu-last ()
  (casualt-calc-setup)
  (calc-push-list '(2 3))
  (funcall 'casual-calc-tmenu)
  (execute-kbd-macro "^")
  (funcall 'casual-calc-tmenu)
  (execute-kbd-macro "L")
  (should (and (= (calc-top) 3)
               (= (calc-top-n 2) 2)))
  (casualt-calc-breakdown t))

(provide 'test-casual-calc)
;;; test-casual-calc.el ends here
