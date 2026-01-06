;;; casual-css-settings.el --- Casual CSS Settings -*- lexical-binding: t; -*-

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
(require 'css-mode)
(require 'casual-lib)

(transient-define-prefix casual-css-settings-tmenu ()
  "Casual CSS settings menu."
  ["Casual CSS: Settings"
   ["Customize"
    ("o" "Indent Offset" casual-css--customize-indent-offset
     :description (lambda () (format "Indent Offset (%d)" css-indent-offset)))]

   ["Group"
    ("G" "CSS Group" casual-css--customize-group)]]

  ["General"
   :class transient-row
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-css-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-css--customize-indent-offset ()
  "Customize CSS `css-indent-offset'."
  (interactive)
  (customize-variable 'css-indent-offset))

(defun casual-css--customize-group ()
  "Customize CSS group."
  (interactive)
  (customize-group "css"))

(defun casual-css-about-css ()
  "Casual CSS is a Transient menu for CSS mode.

Learn more about using Casual CSS at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual CSS, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual CSS was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual CSS.

Always choose love."
  (ignore))

(defun casual-css-about ()
  "About information for Casual CSS."
  (interactive)
  (describe-function #'casual-css-about-css))

(provide 'casual-css-settings)
;;; casual-css-settings.el ends here
