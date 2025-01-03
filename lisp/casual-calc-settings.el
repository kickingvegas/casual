;;; casual-calc-settings.el --- Casual Settings Menu      -*- lexical-binding: t; -*-

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
(require 'casual-calc-labels)

(require 'casual-calc-angle-measure)
(require 'casual-calc-utils)

;; = Menus =
(transient-define-prefix casual-calc-modes-tmenu ()
  "Casual modes menu."
  [["Modes"
    :pad-keys t
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual-lib-checkbox-label calc-algebraic-mode
                                               "Algebraic Mode"))
     :transient t)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (casual-lib-checkbox-label calc-leading-zeros
                                            "Leading Zeroes"))
     :transient t)

    ("F" calc-frac-mode :description casual-calc-prefer-frac-label :transient t)
    ("s" calc-symbolic-mode :description casual-calc-symbolic-mode-label :transient t)
    ("p" calc-polar-mode :description casual-calc-cmplx-or-polar-label :transient t)
    ("c" "Complex Number Format›" casual-calc-complex-format-tmenu
     :description (lambda ()
                    (format "Complex Number Format (now %s)›"
                            (casual-calc-complex-format-label)))
     :transient t)
    ;; ("m" calc-matrix-mode :description casual-calc-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("P" calc-precision
     :description (lambda ()
                    (format "Precision (now %d)" calc-internal-prec))
     :transient t)
    ("I" "Infinite Mode" casual-calc--infinite-mode
     :description (lambda ()
                    (casual-lib-checkbox-label calc-infinite-mode
                                            "Infinite Mode"))
     :transient t)]

   ["Angular Measure"
    ("a" casual-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-calc-angle-mode-label)))
     :transient t)]]

  [["Display"
    ("R" casual-calc-radix-tmenu
     :description (lambda ()
                    (format "Radix (now %s)›" (casual-calc-number-radix-label)))
     :transient t)
    ("f" casual-calc-float-format-tmenu
     :description (lambda ()
                    (format "Float Formats (now %s)›"
                            (casual-calc-float-format-label)))
     :transient t)
    ("g" calc-group-digits
     ;; TODO calc-group-digits can actually be an int 😦
     :description (lambda ()
                    (casual-lib-checkbox-label calc-group-digits
                                            "Show Thousands Separators"))
     :transient t)
    ("," "Thousands Separator…" calc-group-char
     :description (lambda ()
                    (format "Set Thousands Separator (now %s)…" calc-group-char))
     :transient t)
    ("." "Decimal Separator…" calc-point-char
     :description (lambda ()
                    (format "Set Decimal Separator (now %s)…" calc-point-char))
     :transient t)
    ("H" "ℎ𝑚𝑠 Format" calc-hms-notation
     :description (lambda ()
                    (format
                     "Set %s Format (now %s)"
                     (casual-calc-unicode-get :hms)
                     (format calc-hms-format "" "" "")))
     :transient t)]

   ["Settings"
    ("C" "Customize Calc group" casual-calc--customize-group)
    ("S" "Save Calc Settings" calc-save-modes :transient t)
    ("O" "Open Calc Settings File" casual-calc-open-settings-file :transient nil)
    ("C-M-r" "Calc Reset" calc-reset :transient t)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          (casual-lib-quit-one)

          ("M-a" "About" casual-calc-about :transient nil)
          (casual-lib-quit-all)])


(defun casual-calc--customize-group ()
  "Customize calc group."
  (interactive)
  (customize-group "calc"))

(transient-define-prefix casual-calc-complex-format-tmenu ()
  "Casual complex formats menu."
  ["Complex Number Format"
   :description (lambda ()
                  (format "Complex Number Format (now %s)"
                          (casual-calc-complex-format-label)))
   ("c" calc-complex-notation
    :description "complex (rectangular) notation"
    :transient nil)

   ("i" calc-i-notation
    :description "𝑖 notation"
    :transient nil)

   ("j" calc-j-notation
    :description "𝑗 notation"
    :transient nil)]

  casual-calc-navigation-group)


(transient-define-prefix casual-calc-float-format-tmenu ()
  "Casual float formats menu."
  ["Float Format (𝑛 is 𝟣: on stack)"
   ("n" "Normal" calc-normal-notation :transient nil)
   ("f" "Fixed Point 𝑛" calc-fix-notation :transient nil)
   ("s" "Scientific" calc-sci-notation :transient nil)
   ("e" "Engineering" calc-eng-notation :transient nil)]

  casual-calc-navigation-group)

;; = Functions =
(defun casual-calc-about-calc ()
  "Casual Calc is an opinionated user interface for the Emacs calculator.

Learn more about using Casual Calc at our discussion group on GitHub.
Any questions or comments about Casual should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Calc, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Calc was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Calc.

Always choose love."
  (ignore))

(defun casual-calc-about ()
  "About information for Casual."
  (interactive)
  (describe-function 'casual-calc-about-calc))

(defun casual-calc--infinite-mode ()
  "Toggle infinite mode on or off.

Divide-by-zero (e.g. ‘1 / 0’) results are normally treated as
errors; formulas like this are left in unsimplified form. An
alternate behavior is to treat a divide-by-zero condition as an
infinite result. This command toggles this behavior.

This function is a wrapper over `calc-infinite-mode'.

* References
- info node `(calc) Infinite Mode'
- `calc-infinite-mode'"
  (interactive)
  (call-interactively #'calc-infinite-mode))

(provide 'casual-calc-settings)
;;; casual-calc-settings.el ends here
