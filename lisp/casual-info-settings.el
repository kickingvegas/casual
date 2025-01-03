;;; casual-info-settings.el --- Casual Info Settings  -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'info)
(require 'casual-lib)
(require 'casual-info-variables)

(require 'casual-info-utils)

;;; Menus
(transient-define-prefix casual-info-settings-tmenu ()
  ["Customize Info"
   ("A" "Additional directories list"
    casual-info--customize-info-additional-directory-list)
   ("i" "I-search multiple nodes"
    casual-info--customize-info-isearch-search
    :description (lambda ()
                   (casual-lib-checkbox-label
                    Info-isearch-search
                    "I-search through multiple nodes")))
   (casual-lib-customize-unicode)
   ("c" "Prefer subnodes when scrolling"
    casual-info--customize-info-scroll-prefer-subnodes
    :description (lambda ()
                   (casual-lib-checkbox-label
                    Info-scroll-prefer-subnodes
                    "Prefer subnodes when scrolling")))
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-info-about :transient nil)

          (casual-lib-quit-all)])

;;; Functions
(defun casual-info--customize-info-isearch-search ()
  "Customize variable `Info-isearch-search'.

If non-nil, isearch in Info searches through multiple nodes."
  (interactive)
  (customize-variable 'Info-isearch-search))

(defun casual-info--customize-info-additional-directory-list ()
  "Customize variable `Info-additional-directory-list'.

List of additional directories to search for Info documentation files.
These directories are searched after those in `Info-directory-list'."
  (interactive)
  (customize-variable 'Info-additional-directory-list))

(defun casual-info--customize-info-scroll-prefer-subnodes ()
  "Customize variable `Info-scroll-prefer-subnodes'.

If non-nil, \\<Info-mode-map>\\[Info-scroll-up] in a menu visits subnodes."
  (interactive)
  (customize-variable 'Info-scroll-prefer-subnodes))

(defun casual-info--customize-info-group ()
  "Call the Info customization group.

Sadly, Info variables are not grouped so this command is really of
limited usefulness."
  (interactive)
  (customize-group "info"))

(defun casual-info-about-info ()
  "Casual Info is an opinionated porcelain for Emacs Info.

Learn more about using Casual Info at our discussion group on GitHub.
Any questions or comments about Casual should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Info, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Info was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Info.

Always choose love."
  (ignore))

(defun casual-info-about ()
  "About information for Casual Info."
  (interactive)
  (describe-function #'casual-info-about-info))

(provide 'casual-info-settings)
;;; casual-info-settings.el ends here
