;;; test-casual-calc-settings.el --- Casual Settings Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-test-utils)
(require 'casual-calc-settings)

(ert-deftest test-casual-calc-complex-format-tmenu-integration ()
  (casualt-calc-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-calc-complex-format-tmenu
   '(("i" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'i)))))
     ("j" () (lambda () (calc-slow-wrapper (should (eq calc-complex-format 'j)))))
     ("c" () (lambda () (calc-slow-wrapper (should (not calc-complex-format)))))))
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc-float-format-tmenu-integration ()
  (casualt-calc-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-calc-float-format-tmenu
   '(("s" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'sci)))))
     ("f3" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'fix)))))
     ("e" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'eng)))))
     ("n" () (lambda () (calc-slow-wrapper (should (eq (car calc-float-format) 'float)))))
     ))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-modes-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-algebraic-mode) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-leading-zeros) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-frac-mode) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-symbolic-mode) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-polar-mode) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-precision) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-group-digits) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-group-char) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-point-char) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-hms-notation) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-save-modes) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-reset) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("A" . calc-algebraic-mode)
                           ("z" . calc-leading-zeros)
                           ("F" . calc-frac-mode)
                           ("s" . calc-symbolic-mode)
                           ("p" . calc-polar-mode)
                           ("c" . casual-calc-complex-format-tmenu)
                           ("P" . calc-precision)
                           ("I" . casual-calc--infinite-mode)
                           ("a" . casual-calc-angle-measure-tmenu)
                           ("R" . casual-calc-radix-tmenu)
                           ("f" . casual-calc-float-format-tmenu)
                           ("g" . calc-group-digits)
                           ("," . calc-group-char)
                           ("." . calc-point-char)
                           ("H" . calc-hms-notation)
                           ("C" . casual-calc--customize-group)
                           ("S" . calc-save-modes)
                           ("O" . casual-calc-open-settings-file)
                           ("" . calc-reset)

                           ("á" . casual-calc-about))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-modes-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc-complex-format-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-complex-notation) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-i-notation) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-j-notation) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("c" . calc-complex-notation)
                           ("i" . calc-i-notation)
                           ("j" . calc-j-notation))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-complex-format-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-float-format-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-normal-notation) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-fix-notation) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-sci-notation) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-eng-notation) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(
                           ("n" . calc-normal-notation)
                           ("f" . calc-fix-notation)
                           ("s" . calc-sci-notation)
                           ("e" . calc-eng-notation))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-float-format-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t))



(provide 'test-casual-calc-settings)
;;; test-casual-calc-settings.el ends here
