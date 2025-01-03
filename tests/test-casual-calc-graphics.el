;;; test-casual-calc-graphics.el --- Casual Graphics Tests  -*- lexical-binding: t; -*-

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

;; Note that these are UI tests. Testing wrapped functions means effectively
;; testing Calc behavior which is out of scope for testing here.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'casual-calc-test-utils)
(require 'casual-calc-graphics)

(ert-deftest test-casual-calc--push-natural-interval-0-100 ()
  (casualt-calc-setup)
  (casual-calc--push-natural-interval-0-100)
  (should (equal '(intv 3 0 100) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-natural-interval-0-360 ()
  (casualt-calc-setup)
  (casual-calc--push-natural-interval-0-360)
  (should (equal '(intv 3 0 360) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-float-interval-0-100 ()
  (casualt-calc-setup)
  (casual-calc--push-float-interval-0-100)
  (should (equal '(intv 3 (float 0 0) (float 1 2)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-float-interval-1-symmetric ()
  (casualt-calc-setup)
  (casual-calc--push-float-interval-1-symmetric)
  (should (equal '(intv 3 (float -1 0) (float 1 0)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-sin ()
  (casualt-calc-setup)
  (casual-calc--push-sin)
  (should (equal '(calcFunc-sin (var x var-x)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-cos ()
  (casualt-calc-setup)
  (casual-calc--push-cos)
  (should (equal '(calcFunc-cos (var x var-x)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-tan ()
  (casualt-calc-setup)
  (casual-calc--push-tan)
  (should (equal '(calcFunc-tan (var x var-x)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-ln ()
  (casualt-calc-setup)
  (casual-calc--push-ln)
  (should (equal '(calcFunc-ln (var x var-x)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-e-raised-to-x ()
  (casualt-calc-setup)
  (casual-calc--push-e-raised-to-x)
  (should (equal '(^ (var e var-e) (var x var-x)) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-polynomial-order-2 ()
  (casualt-calc-setup)
  (casual-calc--push-polynomial-order-2)
  (should (equal '(+ (^ (var x var-x) 2) 1) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--push-polynomial-order-3 ()
  (casualt-calc-setup)
  (casual-calc--push-polynomial-order-3)
  (should (equal '(+ (+ (^ (var x var-x) 3) (^ (var x var-x) 2)) 1) (calc-top)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-plot-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-graph-plot) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-clear) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-print) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-command) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'casual-calc-read-plot-data) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("a" . casual-calc--graph-add)
                           ("e" . casual-calc--graph-add-equation)
                           ("A" . casual-calc--graph-add-3d)
                           ("d" . casual-calc--graph-delete)
                           ("N" . casual-calc--graph-name)
                           ("j" . casual-calc--graph-juggle)
                           ("s" . casual-calc-curve-style-tmenu)
                           ("o" . casual-calc-read-plot-data)
                           ("c" . calc-graph-clear)
                           ("r" . calc-graph-plot) ; can't get this to work, dunno why
                           ("n" . casual-calc--graph-num-points)
                           ("S" . casual-calc-plot-options-tmenu)
                           ("g" . casual-calc-graph-settings-tmenu)
                           ("p" . calc-graph-print)
                           ("C" . calc-graph-command)
                           ("E" . casual-calc-graph-examples-tmenu)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-plot-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc-graph-examples-tmenu ()
  (casualt-calc-setup)
  (let* ((test-vectors '(("a" . casual-calc--push-natural-interval-0-100)
                         ("b" . casual-calc--push-natural-interval-0-360)
                         ("c" . casual-calc--push-float-interval-0-100)
                         ("d" . casual-calc--push-float-interval-1-symmetric)
                         ("1" . casual-calc--push-sin)
                         ("2" . casual-calc--push-cos)
                         ("3" . casual-calc--push-tan)
                         ("4" . casual-calc--push-ln)
                         ("5" . casual-calc--push-e-raised-to-x)
                         ("6" . casual-calc--push-polynomial-order-2)
                         ("7" . casual-calc--push-polynomial-order-3)))
         (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-graph-examples-tmenu
                                       '(lambda () (random 5000))))

  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc-graph-settings-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-graph-device) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-output) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-quit) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("d" . calc-graph-device)
                           ("o" . calc-graph-output)
                           ("Q" . calc-graph-quit))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-graph-settings-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc-curve-style-tmenu ()
  (casualt-calc-setup)
  (let* ((test-vectors '(("l" . casual-calc--graph-toggle-line-style)
                         ("p" . casual-calc--graph-toggle-point-style))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-curve-style-tmenu
                                       '(lambda () (random 5000))))
  (casualt-calc-breakdown t))



(provide 'test-casual-calc-graphics)
;;; test-casual-calc-graphics.el ends here
