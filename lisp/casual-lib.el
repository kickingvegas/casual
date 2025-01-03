;;; casual-lib.el --- Library routines for Casual user interfaces -*- lexical-binding: t; -*-

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

;; Library routines for Casual user interfaces.

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'transient)

(defgroup casual nil
  "Settings for Casual user interfaces."
  :group 'convenience)

(defcustom casual-lib-hide-navigation nil
  "If non-nil then hide navigation controls.

If non-nil, customize Casual user interfaces to hide navigation controls for
`transient-quit-all' (control-q) and `transient-quit-one' (control-g)."
  :type 'boolean
  :group 'casual)

(defun casual-lib-customize-casual-lib-hide-navigation ()
  "Customize `casual-lib-hide-navigation'.

Customize Casual user interfaces to hide navigation commands."
  (interactive)
  (customize-variable 'casual-lib-hide-navigation))

(defcustom casual-lib-use-unicode nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'casual)

(defun casual-lib-customize-casual-lib-use-unicode ()
  "Customize `casual-lib-use-unicode'.

Customize Casual user interfaces to use Unicode symbols in place of strings
when appropriate."
  (interactive)
  (customize-variable 'casual-lib-use-unicode))

(defun casual-lib-unicode-db-get (key db)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.
- DB alist containing Unicode symbol map.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (let* ((db db)
         (unicode casual-lib-use-unicode)
         (item (alist-get key db)))
    (if unicode
        (nth 0 (eval item))
      (nth 1 (eval item)))))

;; Predicates
(defun casual-lib-display-line-numbers-mode-p ()
  "Predicate to test if `display-line-numbers-mode' is enabled."
  (bound-and-true-p display-line-numbers))

(defun casual-lib-buffer-writeable-p ()
  "Predicate to test if buffer is writeable."
  (not buffer-read-only))

(defun casual-lib-buffer-writeable-and-region-active-p ()
  "Predicate to test if buffer is writeable and region is active."
  (and (casual-lib-buffer-writeable-p) (region-active-p)))

(defun casual-lib-hide-navigation-p ()
  "Predicate for variable `casual-lib-hide-navigation'."
  (if casual-lib-hide-navigation t nil))

(defun casual-lib-quit-all-hide-navigation-p ()
  "Predicate for hiding navigation for the `transient-quit-all' command."
  (if casual-lib-hide-navigation
      t
    (if transient--stack
        nil
      t)))

;; Labels
(defun casual-lib--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if casual-lib-use-unicode
      (if v "☑︎" "◻︎")
    (if v "[x]" "[ ]")))

(defun casual-lib--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun casual-lib-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (casual-lib--prefix-label label (casual-lib--variable-to-checkbox v)))

(defun casual-lib--suffix-label (label suffix)
  "Label constructed with LABEL and SUFFIX separated by a space."
  (format "%s %s" label suffix))

(defun casual-lib--quit-one-suffix-label ()
  "Description label for Transient suffix `casual-lib-quit-one'."
  (if transient--stack
      "‹Back"
    "Dismiss"))

;; Transients
(transient-define-suffix casual-lib-quit-all ()
  "Casual suffix to call `transient-quit-all'."
  :transient nil
  :if-not #'casual-lib-quit-all-hide-navigation-p
  :key "C-q"
  :description "Dismiss"
  (interactive)
  (transient-quit-all))

(transient-define-suffix casual-lib-quit-one ()
  "Casual suffix to call `transient-quit-one'."
  :transient nil
  :key "C-g"
  :if-not #'casual-lib-hide-navigation-p
  :description #'casual-lib--quit-one-suffix-label
  (interactive)
  (transient-quit-one))

(transient-define-suffix casual-lib-customize-unicode ()
  "Customize Casual to use Unicode symbols.

This Transient suffix invokes the customize interface for the
variable `casual-lib-use-unicode'.

Note that this variable affects all Casual user interfaces."
  :key "u"
  :transient nil
  :description (lambda ()
                 (casual-lib-checkbox-label
                  casual-lib-use-unicode
                  "Use Unicode Symbols"))
  (interactive)
  (casual-lib-customize-casual-lib-use-unicode))

(transient-define-suffix casual-lib-customize-hide-navigation ()
  "Customize Casual hide navigation controls.

This Transient suffix invokes the customize interface for the
variable `casual-lib-hide-navigation'.

Note that this variable affects all Casual user interfaces."
  :key "n"
  :transient nil
  :description (lambda ()
                 (casual-lib-checkbox-label
                  casual-lib-hide-navigation
                  "Hide Navigation Commands"))
  (interactive)
  (casual-lib-customize-casual-lib-hide-navigation))

(provide 'casual-lib)
;;; casual-lib.el ends here
