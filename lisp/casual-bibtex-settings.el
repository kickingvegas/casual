;;; casual-bibtex-settings.el --- Casual BibTeX Settings -*- lexical-binding: t; -*-

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
(require 'casual-lib)

(transient-define-prefix casual-bibtex-settings-tmenu ()
  "Casual BibTeX settings menu."
  ["Casual BibTeX Settings"
   ["Settings"
    ("d" "Dialect" casual-bibtex--customize-dialect
     :description (lambda ()
                    (if bibtex-dialect
                        (format "Dialect (%s)" bibtex-dialect)
                      "Dialect")))
    ("G" "BibTeX Group" casual-bibtex--customize-group)]

   ["Files"
    ("p" "Path" casual-bibtex--customize-file-path
      :description (lambda ()
                    (if bibtex-file-path
                        (format "Path (%s)" bibtex-file-path)
                      "Path")))
    ("f" "Files" casual-bibtex--customize-files)]

   ["Search"
    ("g" "Entry Globally" casual-bibtex--customize-search-entry-globally
     :description (lambda () (casual-lib-checkbox-label bibtex-search-entry-globally
                                                   "Entry Globally")))]

   ["Hooks"
    ("C" "Clean" casual-bibtex--customize-clean-entry-hook)
    ("A" "Add" casual-bibtex--customize-add-entry-hook)]]

  ["Misc"
    :class transient-row
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-bibtex-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-bibtex-about-bibtex ()
  "Casual BibTeX is a Transient menu for BibTeX.

Learn more about using Casual BibTeX at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual BibTeX, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual BibTeX was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual BibTeX.

Always choose love."
  (ignore))

(defun casual-bibtex-about ()
  "About information for Casual BibTeX."
  (interactive)
  (describe-function #'casual-bibtex-about-bibtex))


;;; Customize Functions
(defun casual-bibtex--customize-group ()
  "Customize BibTeX group."
  (interactive)
  (customize-group "bibtex"))

(defun casual-bibtex--customize-dialect ()
  "Set BibTeX dialect.

This customizes the variable `bibtex-dialect'."
  (interactive)
  (customize-variable 'bibtex-dialect))

(defun casual-bibtex--customize-files ()
  "Set BibTeX files.

This customizes the variable `bibtex-files'."
  (interactive)
  (customize-variable 'bibtex-files))

(defun casual-bibtex--customize-file-path ()
  "Set BibTeX file path.

This customizes the variable `bibtex-file-path'."
  (interactive)
  (customize-variable 'bibtex-file-path))

(defun casual-bibtex--customize-search-entry-globally ()
  "Set BibTeX search to be global.

This customizes the variable `bibtex-search-entry-globally'."
  (interactive)
  (customize-variable 'bibtex-search-entry-globally))

(defun casual-bibtex--customize-clean-entry-hook ()
  "Set hook for cleaning a BibTeX entry.

This customizes the variable `bibtex-clean-entry-hook'."
  (interactive)
  (customize-variable 'bibtex-clean-entry-hook))

(defun casual-bibtex--customize-add-entry-hook ()
  "Set hook for adding a BibTeX entry.

This customizes the variable `bibtex-add-entry-hook'."
  (interactive)
  (customize-variable 'bibtex-add-entry-hook))


(provide 'casual-bibtex-settings)
;;; casual-bibtex-settings.el ends here
