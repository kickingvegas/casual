;;; test-casual-calc--calc.el --- Casual Wrapped Casual Tests  -*- lexical-binding: t; -*-

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
(require 'ert)
;;(require 'casual)
(require 'casual-calc-test-utils)
(require 'casual-calc--calc)

;;; Tests

(ert-deftest test-casual-calc--inv ()
  (casualt-calc-setup)
  (calc-push 10)
  (casualt-testbench-calc-fn #'casual-calc--inv
                             '()
                             '(float 1 -1))
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc--sqrt ()
  (casualt-calc-setup)
  (calc-push 25)
  (casualt-testbench-calc-fn #'casual-calc--sqrt
                             '()
                             5)
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--change-sign ()
  (casualt-calc-setup)
  (calc-push 10)
  (casualt-testbench-calc-fn #'casual-calc--change-sign
                             '()
                             -10)
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc--power ()
  (casualt-calc-setup)
  (calc-push 2)
  (calc-push 3)
  (casualt-testbench-calc-fn #'casual-calc--power
                             '()
                             8)
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc--abs ()
  (casualt-calc-setup)
  (calc-push -10)
  (casualt-testbench-calc-fn #'casual-calc--abs
                             '()
                             10)
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--factorial ()
  (casualt-calc-setup)
  (calc-push 10)
  (casualt-testbench-calc-fn #'casual-calc--factorial
                             '()
                             3628800)
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--percent ()
  (casualt-calc-setup)
  (calc-push 10)
  (casualt-testbench-calc-fn #'casual-calc--percent
                             '()
                             '(calcFunc-percent 10))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--percent-change ()
  (casualt-calc-setup)
  (calc-push 40)
  (calc-push 50)
  (casualt-testbench-calc-fn #'casual-calc--percent-change
                             '()
                             '(calcFunc-percent 25))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--pi ()
  (casualt-calc-setup)
  (casualt-testbench-calc-fn #'casual-calc--pi
                             '()
                             '(float 314159265359 -11))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--evaluate ()
  (casualt-calc-setup)
  (calc-push '(* 10 (var x var-x)))
  (casualt-testbench-calc-fn #'casual-calc--evaluate
                             '()
                             '(* 10 (var x var-x)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--stack-swap ()
  (casualt-calc-setup)
  (calc-push 10)
  (calc-push 20)
  (casualt-testbench-calc-fn #'casual-calc--stack-swap
                             '()
                             10)
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--stack-drop ()
  (casualt-calc-setup)
  (calc-push 19)
  (calc-push 20)
  (casualt-testbench-calc-fn #'casual-calc--stack-drop
                             '()
                             19)
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--stack-last ()
  (casualt-calc-setup)
  (calc-push 225)
  (call-interactively #'calc-pop)
  (casualt-testbench-calc-fn #'casual-calc--stack-last
                             '()
                             225)
  (casualt-calc-breakdown t))

(provide 'test-casual-calc--calc)
;;; test-casual-calc--calc.el ends here
