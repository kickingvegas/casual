;;; test-casual-calc-conversion.el --- Casual Conversion Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-conversion)

(ert-deftest test-casual-calc-conversions-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-to-degrees) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-to-radians) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-to-hms) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-from-hms) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-float) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-fraction) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("d" . calc-to-degrees)
                           ("r" . calc-to-radians)
                           ("h" . calc-to-hms)
                           ("H" . calc-from-hms)
                           ("f" . calc-float)
                           ("F" . calc-fraction)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-conversions-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(provide 'test-casual-calc-conversion)
;;; test-casual-calc-conversion.el ends here
