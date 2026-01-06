;;; casual-html-settings.el --- Casual HTML Settings -*- lexical-binding: t; -*-

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
(require 'sgml-mode)
(require 'casual-lib)

(defvar html-ts-mode-indent-offset)

(transient-define-prefix casual-html-settings-tmenu ()
  "Casual HTML settings menu."
  ["Casual HTML: Settings"
   ["Offsets"
    ("b" "Basic" casual-html--customize-sgml-basic-offset
     :description (lambda () (format "Basic (%d)" sgml-basic-offset)))
    ("t" "Tree-sitter Basic" casual-html--customize-html-ts-mode-indent-offset
     :if (lambda () (derived-mode-p 'html-ts-mode))
     :description (lambda () (format "Tree-sitter Basic (%d)"
                                html-ts-mode-indent-offset)))
    ("A" "Attribute" casual-html--customize-sgml-attribute-offset
     :description (lambda () (format "Attribute (%d)" sgml-attribute-offset)))]

   ["Group"
    ("G" "SGML Group" casual-html--customize-group-sgml)]]

  ["General"
   :class transient-row
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-html-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-html--customize-group-sgml ()
  "Customize SGML group."
  (interactive)
  (customize-group "sgml"))

(defun casual-html--customize-sgml-basic-offset ()
  "Customize SGML `sgml-basic-offset'."
  (interactive)
  (customize-variable 'sgml-basic-offset))

(defun casual-html--customize-sgml-attribute-offset ()
  "Customize SGML `sgml-attribute-offset'."
  (interactive)
  (customize-variable 'sgml-attribute-offset))

(defun casual-html--customize-html-ts-mode-indent-offset ()
  "Customize SGML `html-ts-mode-indent-offset'."
  (interactive)
  (customize-variable 'html-ts-mode-indent-offset))

(defun casual-html-about-html ()
  "Casual HTML is a Transient menu for HTML mode.

Learn more about using Casual HTML at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual HTML, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual HTML was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual HTML.

Always choose love."
  (ignore))

(defun casual-html-about ()
  "About information for Casual HTML."
  (interactive)
  (describe-function #'casual-html-about-html))

(provide 'casual-html-settings)
;;; casual-html-settings.el ends here
