;;; casual-calc-predicates.el --- Casual Predicates       -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.

(defun casual-calc-matrixp ()
  "Predicate if top of stack is a matrix."
  (if (> (calc-stack-size) 0)
      (math-matrixp (calc-top-n 1))))

(defun casual-calc-square-matrixp ()
  "Predicate if top of stack is a square matrix."
  (if (> (calc-stack-size) 0)
      (math-square-matrixp (calc-top-n 1))))

(defun casual-calc-vectorp ()
  "Predicate if top of stack is a vector."
  (if (> (calc-stack-size) 0)
      (math-vectorp (calc-top-n 1))))

(defun casual-calc-crossp ()
  "Predicate if top two stack items supports a cross product."
  (if (> (calc-stack-size) 1)
      (let ((arg1 (calc-top-n 1))
            (arg2 (calc-top-n 2)))
        (and (math-vectorp arg1)
             (math-vectorp arg2)
             (not (math-matrixp arg1))
             (not (math-matrixp arg2))
             (eq 3 (calcFunc-vlen arg1))
             (eq 3 (calcFunc-vlen arg2))))
    nil))

(defun casual-calc-matrixmultp ()
  "Predicate if top two stack items support matrix multiplication."
  (if (> (calc-stack-size) 1)
      (let ((arg1 (calc-top-n 1))
            (arg2 (calc-top-n 2)))
        (and (math-matrixp arg1)
             (math-matrixp arg2)))
    nil))

(provide 'casual-calc-predicates)
;;; casual-calc-predicates.el ends here
