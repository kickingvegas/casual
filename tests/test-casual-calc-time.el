;;; test-casual-calc-time.el --- Casual Time Menu Tests   -*- lexical-binding: t; -*-

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
(require 'casual-calc-time)

(ert-deftest test-casual-calc-time-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-now) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-inc-month) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-unix-time) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-business-days-plus) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-business-days-minus) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("n" . calc-now)
                           ("f" . casual-calc-first-day-tmenu)
                           ("i" . calc-inc-month)
                           ("u" . calc-unix-time)
                           ("a" . calc-business-days-plus)
                           ("s" . calc-business-days-minus)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-time-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-first-day-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-new-week) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-new-month) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-new-year) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("w" . calc-new-week)
                           ("m" . calc-new-month)
                           ("y" . calc-new-year)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-first-day-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(provide 'test-casual-calc-time)
;;; test-casual-calc-time.el ends here
