;;; casual-calc-utils.el --- Casual Calc Utils       -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'casual-calc--calc)
(require 'transient)
(require 'casual-lib)

(defconst casual-calc-unicode-db
  '((:inv . '("1/𝑥" "1/x"))
    (:sqrt . '("√" "sqrt"))
    (:change-sign . '("∓" "+/-"))
    (:power . '("𝑦ˣ" "y^x"))
    (:abs . '("|𝑥|" "|x|"))
    (:factorial . '(" !" "!"))
    (:percent . '("%" "%"))
    (:percent-change . '(" Δ%" "% change"))
    (:pi . '("𝜋" "pi"))
    (:e . '("𝑒" "e"))
    (:ln . '("𝑙𝑛" "ln"))
    (:lnp1 . '("𝑙𝑛(𝑥+𝟣)" "ln(x+1)"))
    (:log10 . '("𝑙𝑜𝑔₁₀" "log10"))
    (:log . '("𝑙𝑜𝑔ₐ(𝑥)" "log"))
    (:exp . '("𝑒ˣ" "e^x"))
    (:expm1 . '("𝑒ˣ-𝟣" "e^x - 1"))
    (:sin . '("𝑠𝑖𝑛" "sin"))
    (:cos . '("𝑐𝑜𝑠" "cos"))
    (:tan . '("𝑡𝑎𝑛" "tan"))
    (:stack . '("≣" "Stack"))
    (:arcsin . '("𝑎𝑟𝑐𝑠𝑖𝑛" "arcsin"))
    (:arccos . '("𝑎𝑟𝑐𝑐𝑜𝑠" "arccos"))
    (:arctan . '("𝑎𝑟𝑐𝑡𝑎𝑛" "arctan"))
    (:degrees . '("°" "degrees"))
    (:radians . '("𝑟𝑎𝑑" "radians"))
    (:hms . '("ℎ𝑚𝑠" "HMS"))
    (:float . '("𝑓𝑙𝑜𝑎𝑡" "float"))
    (:fraction . '("𝑓𝑟𝑎𝑐𝑡𝑖𝑜𝑛" "fraction"))
    (:to . '("→" "to")))
  "Unicode symbol DB to use for Calc Transient menus.")

(defun casual-calc-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-calc-unicode-db))

;; Transient Navigation
(transient-define-suffix casual-calc-undo-suffix ()
  "Undo stack."
  :transient t
  :key "U"
  :description (lambda () (format "Undo %s"
                                  (casual-calc-unicode-get :stack)))
  (interactive)
  (call-interactively #'calc-undo))

(transient-define-suffix casual-calc-algebraic-entry ()
  "Algebraic entry."
  :transient t
  :key "'"
  :description "Entry"
  (interactive)
  (call-interactively #'calc-algebraic-entry))

(transient-define-suffix casual-calc-pop ()
  "Pop."
  :transient t
  :key "DEL"
  :description "Pop"
  (interactive)
  (call-interactively #'calc-pop))

(transient-define-suffix casual-calc-enter ()
  "Enter/Return."
  :transient t
  :key "RET"
  :description "Enter"
  (interactive)
  (call-interactively #'calc-enter))

(transient-define-suffix casual-calc-edit ()
  "Enter/Return."
  :transient nil
  :key "`"
  :description "Edit"
  (interactive)
  (call-interactively #'calc-edit))

(transient-define-suffix casual-calc-roll-down ()
  "Roll stack down.

Invokes command `calc-roll-down'."
  :transient t
  :key "TAB"
  :description "Roll↓"
  (interactive)
  (call-interactively #'calc-roll-down))

(defconst casual-calc-operators-group
  ["Operators"
    ("+" "add" casual-calc--plus :transient t)
    ("-" "sub" casual-calc--minus :transient t)
    ("*" "mul" casual-calc--times :transient t)
    ("/" "div" casual-calc--divide :transient t)
    ("%" "mod" casual-calc--mod :transient t)])

(defconst casual-calc-operators-group-row
  ["Operators"
   :class transient-row
    ("+" "add" casual-calc--plus :transient t)
    ("-" "sub" casual-calc--minus :transient t)
    ("*" "mul" casual-calc--times :transient t)
    ("/" "div" casual-calc--divide :transient t)
    ("%" "mod" casual-calc--mod :transient t)])

(defconst casual-calc-basic-operators-group
  ["Operators"
    ("+" "add" casual-calc--plus :transient t)
    ("-" "sub" casual-calc--minus :transient t)
    ("*" "mul" casual-calc--times :transient t)
    ("/" "div" casual-calc--divide :transient t)])

(defconst casual-calc-navigation-group
  [:class transient-row
          (casual-lib-quit-one)
          (casual-calc-algebraic-entry)
          (casual-calc-enter)
          (casual-calc-roll-down)
          (casual-calc-pop)
          (casual-calc-undo-suffix)
          (casual-lib-quit-all)])

(defun casual-calc--percent-of ()
  "Apply percentage at top of stack (1:) to value above it (2:).

Given an example stack:

    2: a
    1: b

Executing this function will leave the resultant values on the
stack:

    2: a
    1: a * b%

The result in (1:) can then be added or subtracted from (2:)."
  (interactive)
  (calc-percent)
  (calc-roll-up 2)
  (calc-enter 1)
  (calc-roll-up 3)
  (calc-times 2))

(provide 'casual-calc-utils)
;;; casual-calc-utils.el ends here
