;;; test-casual-calc-random.el --- Casual Random Menu Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords:

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
(require 'casual-calc-random)

(ert-deftest test-casual-calc-random-number-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-assert-testcases
   'casual-calc-random-number-tmenu
   '(
     ;; default m=10
     ("r" ()
      (lambda ()
        (message (format "m=10, n: %d" (calc-top)))
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (< (calc-top) 10)))))
     ("mr" ((intv 3 0 100))
      (lambda ()
        (message (format "stack [0..100], n: %d" (calc-top)))
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (<= (calc-top) 100)))))
     ("a" ()
      (lambda ()
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (<= (calc-top) 100)))))
     ("a" ()
      (lambda ()
        (should (and (math-integerp (calc-top))
                     (>= (calc-top) 0)
                     (<= (calc-top) 100)))))

     ("c" ()
      (lambda () ; TODO: Figure out bounds check for xfloat
        (should (math-floatp (calc-top)))))))
  (casualt-calc-breakdown t))


(ert-deftest test-casual-calc-random-number-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-rrandom) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-random-again) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("r" . casual-calc--random-interval-0-to-m)
                           ("c" . calc-rrandom)
                           ("a" . calc-random-again)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-random-number-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(provide 'test-casual-calc-random)
;;; test-casual-calc-random.el ends here
