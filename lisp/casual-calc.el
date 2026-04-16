;;; casual-calc.el --- Transient UI for Calc -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

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

;; Casual Calc is an opinionated Transient-based user interface for Emacs Calc.

;; INSTALLATION
;; (require 'casual-calc) ; optional if using autoloaded menu
;; (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
;; (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:

(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'casual-calc--calc)
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-utils)

(require 'casual-calc-binary)
(require 'casual-calc-complex)
(require 'casual-calc-conversion)
(require 'casual-calc-logarithmic)
(require 'casual-calc-random)
(require 'casual-calc-rounding)
(require 'casual-calc-settings)
(require 'casual-calc-time)
(require 'casual-calc-trigonometric)
(require 'casual-calc-units)
(require 'casual-calc-vector)
(require 'casual-calc-graphics)
(require 'casual-calc-trail)
(require 'casual-calc-stack)
(require 'casual-calc-financial)
(require 'casual-calc-symbolic)
(require 'casual-calc-variables)

;; Menus
;;;###autoload (autoload 'casual-calc-tmenu "casual-calc" nil t)
(transient-define-prefix casual-calc-tmenu ()
  "Casual Calc main menu."
  [["Calc"
    ("&" "1/x" casual-calc--inv
     :description (lambda () (casual-calc-unicode-get :inv))
     :transient t)
    ("Q" "√" casual-calc--sqrt
     :description (lambda () (casual-calc-unicode-get :sqrt))
     :transient t)
    ("n" "∓" casual-calc--change-sign
     :description (lambda () (casual-calc-unicode-get :change-sign))
     :transient t)
    ("^" "𝑦ˣ" casual-calc--power
     :description (lambda () (casual-calc-unicode-get :power))
     :transient t)
    ("=" "=" casual-calc--evaluate :transient t)]
   [""
    ("A" "|𝑥|" casual-calc--abs
     :description (lambda () (casual-calc-unicode-get :abs))
     :transient t)
    ("!" " !" casual-calc--factorial
     :description (lambda () (casual-calc-unicode-get :factorial))
     :transient t)
    ("%" "𝑎𝑏%" casual-calc--percent-of :transient t)
    ("D" " Δ%" casual-calc--percent-change
     :description (lambda () (casual-calc-unicode-get :percent-change))
     :transient t)]
   ["Constants"
    ("p" "𝜋" casual-calc--pi
     :description (lambda () (casual-calc-unicode-get :pi))
     :transient t)
    ("e" "𝑒" casual-calc--e-constant
     :description (lambda () (casual-calc-unicode-get :e))
     :transient t)]

   casual-calc-basic-operators-group

   ["Stack"
    ("s" "Swap" casual-calc--stack-swap :transient t)
    ("r" "Roll" casual-calc--stack-roll-all :transient t)
    ("d" "Drop" casual-calc--stack-drop :transient t)
    ("C" "Clear" casual-calc--stack-clear :transient t)]

   [""
    ("L" "Last" casual-calc--stack-last :transient t)
    ("w" "Copy" casual-calc--copy-as-kill :transient nil)
    ("`" "Edit" calc-edit)
    ("z" "Variables›" casual-calc-variable-crud-tmenu)]]

  ["Arithmetic"
   :class transient-row
   ("o" "Rounding›" casual-calc-rounding-tmenu)
   ("c" "Conversion›" casual-calc-conversions-tmenu)
   ("T" "Time›" casual-calc-time-tmenu)
   ("i" "Complex›" casual-calc-complex-number-tmenu)
   ("R" "Random›" casual-calc-random-number-tmenu)]

  ["Functions"
   [("t" "Trigonometric›" casual-calc-trig-tmenu)
    ("l" "Logarithmic›" casual-calc-logarithmic-tmenu)]

   [("b" "Binary›" casual-calc-binary-tmenu)
    ("v" "Vector/Matrix›" casual-calc-vector-tmenu)]

   [("u" "Units›" casual-calc-units-tmenu)
    ("f" "Financial›" casual-calc-financial-tmenu)]

   [("g" "Graphics›" casual-calc-plot-tmenu)
    ("a" "Algebra›" casual-calc-symbolic-tmenu)]]

  ["Settings"
   :class transient-row
   ("m" "Modes, Displays, Angles›" casual-calc-modes-tmenu)
   ("S" "Stack›" casual-calc-stack-display-tmenu)
   ("M-t" "Trail›" casual-calc-trail-tmenu)]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-calc-algebraic-entry)
   (casual-calc-enter)
   (casual-calc-roll-down)
   (casual-calc-pop)
   (casual-calc-undo-suffix)
   ("q" "Quit" calc-quit)])

(provide 'casual-calc)
;;; casual-calc.el ends here
