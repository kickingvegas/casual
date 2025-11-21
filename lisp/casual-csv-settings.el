;;; casual-csv-settings.el --- Casual CSV Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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
(require 'csv-mode)
(require 'casual-lib)

(transient-define-prefix casual-csv-settings-tmenu ()
  "Casual csv settings menu."
  ["Casual csv: Settings"
   ["Customize"
    ("A" "Align Style" casual-csv--customize-align-style
     :description (lambda () (format "Align Style (%s)"
                                (capitalize (symbol-name csv-align-style)))))
    ("s" "Separators" casual-csv--customize-separators)
    ("i" "Invisibility Default" casual-csv--customize-invisibility-default
     :description (lambda () (casual-lib-checkbox-label csv-invisibility-default
                                                   "Invisibility Default")))]

   [""
    ("G" "CSV Group" casual-csv--customize-group)
    ("h" "Header Lines" casual-csv--customize-header-lines
     :description (lambda () (format "Header Lines (%d)" csv-header-lines)))
    ("c" "Comment Start Default" casual-csv--customize-comment-start-default
     :description (lambda () (format
                         "Comment Start Default (%s)"
                         csv-comment-start-default)))
    ("f" "Field Quotes" casual-csv--customize-field-quotes
     :description (lambda () (format
                         "Field Quotes (%s)"
                         (string-join csv-field-quotes))))]

   ["Width"
    ("w" "Min" casual-csv--customize-align-min-width
     :description (lambda () (format "Min (%d)" csv-align-min-width)))
    ("W" "Max" casual-csv--customize-align-max-width
     :description (lambda () (format "Max (%d)" csv-align-max-width)))]]

  [:class transient-row
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-csv-about)
   (casual-lib-quit-all)])


;; -------------------------------------------------------------------
;; Functions

(defun casual-csv--customize-group ()
  "Customize csv group."
  (interactive)
  (customize-group "CSV"))

(defun casual-csv--customize-align-style ()
  "Customize `csv-align-style'."
  (interactive)
  (customize-variable 'csv-align-style))

(defun casual-csv--customize-separators ()
  "Customize `csv-separators'."
  (interactive)
  (customize-variable 'csv-separators))

(defun casual-csv--customize-field-quotes ()
  "Customize `csv-field-quotes'."
  (interactive)
  (customize-variable 'csv-field-quotes))

(defun casual-csv--customize-align-max-width ()
  "Customize `csv-align-max-width'."
  (interactive)
  (customize-variable 'csv-align-max-width))

(defun casual-csv--customize-align-min-width ()
  "Customize `csv-align-min-width'."
  (interactive)
  (customize-variable 'csv-align-min-width))

(defun casual-csv--customize-invisibility-default ()
  "Customize `csv-invisibility-default'."
  (interactive)
  (customize-variable 'csv-invisibility-default))

(defun casual-csv--customize-comment-start-default ()
  "Customize `csv-comment-start-default'."
  (interactive)
  (customize-variable 'csv-comment-start-default))

(defun casual-csv--customize-header-lines ()
  "Customize `csv-comment-header-lines'."
  (interactive)
  (customize-variable 'csv-header-lines))

(defun casual-csv-about-csv ()
  "Casual csv is a Transient menu for csv pages.

Learn more about using Casual csv at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual csv, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual csv was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual csv.

Always choose love."
  (ignore))

(defun casual-csv-about ()
  "About information for Casual csv."
  (interactive)
  (describe-function #'casual-csv-about-csv))

(provide 'casual-csv-settings)
;;; casual-csv-settings.el ends here
