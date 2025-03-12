;;; casual-make-settings.el --- Casual Make Settings -*- lexical-binding: t; -*-

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
(require 'make-mode)
(require 'casual-lib)

(transient-define-prefix casual-make-settings-tmenu ()
  "Casual Make settings menu."
  ["Make: Settings"
   ["Customize"
    ("G" "Makefile Group" casual-make--customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-make-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-make--customize-group ()
  "Customize Makefile group."
  (interactive)
  (customize-group "makefile"))

(defun casual-make-about-make ()
  "Casual Make is a Transient menu for makefiles.

Learn more about using Casual Make at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Make, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Make was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Make.

Always choose love."
  (ignore))

(defun casual-make-about ()
  "About information for Casual Make."
  (interactive)
  (describe-function #'casual-make-about-make))

(provide 'casual-make-settings)
;;; casual-make-settings.el ends here
