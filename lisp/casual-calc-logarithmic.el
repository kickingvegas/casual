;;; casual-calc-logarithmic.el --- Casual Logarithmic Menu  -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-calc-logarithmic-tmenu ()
  "Casual logarithmic functions."
  ["Logarithmic Functions"
   ["Logarithm"
    ("l" "𝑙𝑛" calc-ln
     :description (lambda () (casual-calc-unicode-get :ln))
     :transient t)
    ("p" "𝑙𝑛(𝑥+𝟣)" calc-lnp1
     :description (lambda () (casual-calc-unicode-get :lnp1))
     :transient t)
    ("1" "𝑙𝑜𝑔₁₀" calc-log10
     :description (lambda () (casual-calc-unicode-get :log10))
     :transient t)
    ("L" "𝑙𝑜𝑔ₐ(𝑥)" calc-log
     :description (lambda () (casual-calc-unicode-get :log))
     :transient t)]

   ["Exponential"
    ("^" "𝑒ˣ" calc-exp
     :description (lambda () (casual-calc-unicode-get :exp))
     :transient t)
    ("m" "𝑒ˣ-𝟣" calc-expm1
     :description (lambda () (casual-calc-unicode-get :expm1))
     :transient t)]

   ["Constant"
    ("e" "𝑒" casual-calc--e-constant
     :description (lambda () (casual-calc-unicode-get :e))
     :transient t)]]

  casual-calc-operators-group-row
  casual-calc-navigation-group)

(provide 'casual-calc-logarithmic)
;;; casual-calc-logarithmic.el ends here
