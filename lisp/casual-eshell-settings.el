;;; casual-eshell-settings.el --- Casual Eshell Settings -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-eshell-settings-tmenu ()
  "Casual Eshell settings menu."
  ["Casual Eshell: Settings"
   [("G" "Eshell Group" casual-eshell--customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-eshell-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-eshell--customize-group ()
  "Customize Eshell group."
  (interactive)
  (customize-group "eshell"))


(defun casual-eshell-about-eshell ()
  "Casual Eshell is a Transient menu for Eshell.

Learn more about using Casual Eshell at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Eshell, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Eshell was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual Eshell.

Always choose love."
  (ignore))

(defun casual-eshell-about ()
  "About information for Casual Eshell."
  (interactive)
  (describe-function #'casual-eshell-about-eshell))

(provide 'casual-eshell-settings)
;;; casual-eshell-settings.el ends here
