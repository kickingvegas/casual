;;; test-casual-calc-predicates.el --- Test Casual Predicates  -*- lexical-binding: t; -*-

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
(require 'casual-calc-predicates)

(ert-deftest test-casual-calc-matrixp ()
  (casualt-calc-setup)
  (calc-push-list '((vec (vec 2 3) (vec 5 7))))
  (should (casual-calc-matrixp))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-square-matrixp ()
  (casualt-calc-setup)
  (calc-push-list '((vec (vec 2 3) (vec 5 7))))
  (should (casual-calc-square-matrixp))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-vectorp ()
  (casualt-calc-setup)
  (calc-push-list '((vec 8 2 9)))
  (should (casual-calc-vectorp))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-crossp ()
  (casualt-calc-setup)
  (calc-push-list '((vec 8 2 9) (vec 1 5 3)))
  (should (casual-calc-crossp))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-matrixmultp ()
  (casualt-calc-setup)
  (calc-push-list '((vec (vec 8 2)) (vec (vec 7) (vec 4))))
  (should (casual-calc-matrixmultp))
  (casualt-calc-breakdown t))

(provide 'test-casual-calc-predicates)
;;; test-casual-calc-predicates.el ends here
