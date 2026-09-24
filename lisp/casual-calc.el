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

;; Casual Calc is a Transient user interface for the Calc library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Calc by running the hook
;; function `casual-calc-init'.

;; Ensure that `casual-calc-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Calc Install' for more detail on
;; installation.

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

;;;###autoload (autoload 'casual-calc-init "casual-calc" nil t)
(defun casual-calc-init ()
  "Initialize and configure Casual Calc.

This hook binds `casual-calc-tmenu' to `casual-keybinding-primary'.

If `casual-calc-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-calc-setup' will be set."
  (add-hook 'calc-mode-hook #'casual-calc-setup))

(defun casual-calc-setup ()
  "Setup `calc-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set calc-mode-map casual-keybinding-primary #'casual-calc-tmenu)
  ;; (keymap-set calc-alg-map casual-keybinding-primary #'casual-calc-tmenu)
  ;; (when casual-calc-add-extra-keybindings
  ;;   )
  )

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
    ("n" "±" casual-calc--change-sign
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
    ("s" "SWAP" casual-calc--stack-swap :transient t)
    ("r" "ROLL" casual-calc--stack-roll-all :transient t)
    ("d" "DROP" casual-calc--stack-drop :transient t)
    ("C" "CLEAR" casual-calc--stack-clear :transient t)]

   [""
    ("L" "LAST" casual-calc--stack-last :transient t)
    ("w" "COPY" casual-calc--copy-as-kill :transient nil)
    ("`" "EDIT" calc-edit)
    ("z" "VAR›" casual-calc-variable-crud-tmenu)]

   ["Settings"
    (",m" "⚙︎›" casual-calc-modes-tmenu
     :description (lambda () (format "%s›" (casual-calc-unicode-get :settings))))
    (",s" "Stack›" casual-calc-stack-display-tmenu
     :description (lambda () (format "%s›" (casual-calc-unicode-get :stack))))
    (",t" "🐾›" casual-calc-trail-tmenu
     :description (lambda () (format "%s›" (casual-calc-unicode-get :trail))))]]

  [
   ["Arithmetic"
    ("o" "ROUND›" casual-calc-rounding-tmenu)
    ("c" "CONV›" casual-calc-conversions-tmenu)]

   [""
    ("T" "TIME›" casual-calc-time-tmenu)
    ("i" "CPLX›" casual-calc-complex-number-tmenu)]

   [""
    ("R" "RAND›" casual-calc-random-number-tmenu)]

   ["Functions"
    ("t" "TRIG›" casual-calc-trig-tmenu)
    ("l" "LOG›" casual-calc-logarithmic-tmenu)]

   [""
    ("b" "BIN›" casual-calc-binary-tmenu)
    ("v" "VEC›" casual-calc-vector-tmenu)]

   [""
    ("u" "UNITS›" casual-calc-units-tmenu)
    ("f" "FIN›" casual-calc-financial-tmenu)]

   [""
    ("g" "GRAPH›" casual-calc-plot-tmenu)
    ("a" "ALG›" casual-calc-symbolic-tmenu)]]

  [:class transient-row
   (casual-calc-algebraic-entry)
   (casual-calc-enter)
   (casual-calc-roll-down)
   (casual-calc-pop)
   (casual-calc-undo-suffix)
   ("q" "Quit" calc-quit)])

(provide 'casual-calc)
;;; casual-calc.el ends here
