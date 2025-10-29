;;; casual-ediff-settings.el --- Casual Ediff Settings -*- lexical-binding: t; -*-

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
(require 'ediff)
(require 'casual-lib)

(transient-define-prefix casual-ediff-settings-tmenu ()
  "Casual Ediff settings menu."
  ["Casual Ediff: Settings"
   [("k" "Keep Variants"
     casual-ediff-customize-ediff-keep-variants
     :description (lambda () (casual-lib-checkbox-label ediff-keep-variants
                                                   "Keep Variants")))

    ("w" "Window Setup Function"
     casual-ediff-customize-ediff-window-setup-function)

    ("s" "Split Window Function"
     casual-ediff-customize-ediff-split-window-function)]

   [("G" "Ediff Group" casual-ediff-customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-ediff-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-ediff-customize-group ()
  "Customize Ediff group."
  (interactive)
  (customize-group "ediff"))

(defun casual-ediff-customize-ediff-keep-variants ()
  "Customize `ediff-keep-variants'."
  (interactive)
  (customize-variable 'ediff-keep-variants))

(defun casual-ediff-customize-ediff-window-setup-function ()
  "Customize `ediff-window-setup-function'."
  (interactive)
  (customize-variable 'ediff-window-setup-function))

(defun casual-ediff-customize-ediff-split-window-function ()
  "Customize `ediff-split-window-function'."
  (interactive)
  (customize-variable 'ediff-split-window-function))

(defun casual-ediff-about-ediff ()
  "Casual Ediff is a Transient menu for Ediff.

Learn more about using Casual Ediff at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Ediff, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Ediff was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual Ediff.

Always choose love."
  (ignore))

(defun casual-ediff-about ()
  "About information for Casual Ediff."
  (interactive)
  (describe-function #'casual-ediff-about-ediff))

(provide 'casual-ediff-settings)
;;; casual-ediff-settings.el ends here
