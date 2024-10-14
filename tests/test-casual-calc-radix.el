;;; test-casual-calc-radix.el --- Casual Radix Tests      -*- lexical-binding: t; -*-

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
(require 'casual-calc-radix)

(ert-deftest test-casual-calc-radix-tmenu ()
  (casualt-calc-setup) ;; calc-number-radix
  (casualt-run-menu-assert-testcases
   'casual-calc-radix-tmenu
   '(("2" () (lambda () (should (= calc-number-radix 2))))
     ("8" () (lambda () (should (= calc-number-radix 8))))
     ("6" () (lambda () (should (= calc-number-radix 16))))
     ;; TODO "n" case
     ("0" () (lambda () (should (= calc-number-radix 10))))))
  (casualt-calc-breakdown t))


(provide 'test-casual-calc-radix)
;;; test-casual-calc-radix.el ends here
