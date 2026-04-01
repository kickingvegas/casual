;;; casual-ispell-settings.el --- Casual Ispell Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'ispell)
(require 'casual-lib)

(transient-define-prefix casual-ispell-settings-tmenu ()
  "Casual Ispell settings menu."
  ["Casual Ispell: Settings"
   [("c" "Comment or String Predicate"
     casual-ispell--customize-ispell-comment-or-string-predicate)
    ("G" "Ispell Group" casual-ispell--customize-group)]

   [(casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-ispell-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-ispell--customize-group ()
  "Customize Ispell group."
  (interactive)
  (customize-group "ispell"))

(defun casual-ispell--customize-ispell-comment-or-string-predicate ()
  "Customize variable `casual-ispell-comment-or-string-predicate'."
  (interactive)
  (customize-variable 'casual-ispell-comment-or-string-predicate))

(defun casual-ispell-about-ispell ()
  "Casual Ispell is a Transient menu for the Emacs spell checker `ispell'.

Learn more about using Casual Ispell at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Ispell, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Ispell was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual Ispell.

Always choose love."
  (ignore))

(defun casual-ispell-about ()
  "About information for Casual Ispell."
  (interactive)
  (describe-function #'casual-ispell-about-ispell))

(provide 'casual-ispell-settings)
;;; casual-ispell-settings.el ends here
