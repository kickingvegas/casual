;;; casual-ibuffer-settings.el --- Casual IBuffer Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Charles Choi

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
(require 'ibuffer)
(require 'casual-lib)


(transient-define-prefix casual-ibuffer-settings-tmenu ()
  "Casual IBuffer settings menu."
  ["IBuffer: Settings"
   ["Customize"
   ("f" "Saved Filters" casual-ibuffer--customize-ibuffer-saved-filters)
   ("g" "Saved Filter Groups" casual-ibuffer--customize-ibuffer-saved-filter-groups)
   ("G" "IBuffer Group" casual-ibuffer--customize-group)
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          ("a" "About" casual-ibuffer-about :transient nil)

          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(defun casual-ibuffer--customize-ibuffer-saved-filters ()
  "Customize `ibuffer-saved-filters'."
  (interactive)
  (customize-variable 'ibuffer-saved-filters))

(defun casual-ibuffer--customize-ibuffer-saved-filter-groups ()
  "Customize `ibuffer-saved-filter-groups'."
  (interactive)
  (customize-variable 'ibuffer-saved-filter-groups))

(defun casual-ibuffer--customize-group ()
  "Customize IBuffer group."
  (interactive)
  (customize-group "ibuffer"))

(defun casual-ibuffer-about-ibuffer ()
  "Casual IBuffer is a Transient menu for IBuffer.

Learn more about using Casual IBuffer at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual IBuffer, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual IBuffer was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual IBuffer.

Always choose love."
  (ignore))

(defun casual-ibuffer-about ()
  "About information for Casual IBuffer."
  (interactive)
  (describe-function #'casual-ibuffer-about-ibuffer))

(provide 'casual-ibuffer-settings)
;;; casual-ibuffer-settings.el ends here
