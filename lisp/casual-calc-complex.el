;;; casual-calc-complex.el --- Casual Complex Menu        -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-complex-number-tmenu ()
  "Casual complex number functions menu."
  [["Complex Number"
    ("r" "Real Part" calc-re :transient t)
    ("i" "Imaginary Part" calc-im :transient t)
    ("c" "Complex Conjugate" calc-conj :transient t)
    ("a" "Argument" calc-argument :transient t)]
   casual-calc-operators-group]
  casual-calc-navigation-group)

(provide 'casual-calc-complex)
;;; casual-calc-complex.el ends here
