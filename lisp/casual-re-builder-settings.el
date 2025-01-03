;;; casual-re-builder-settings.el --- Casual Re-Builder Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

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
(require 're-builder)
(require 'casual-lib)


(transient-define-prefix casual-re-builder-settings-tmenu ()
  "Casual RE-Builder settings menu."
  ["RE-Builder: Settings"
   ["Customize"
    ("G" "RE-Builder Group" casual-re-builder--customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-re-builder-about :transient nil)

          (casual-lib-quit-all)])

(defun casual-re-builder--customize-group ()
  "Customize Re-Builder group."
  (interactive)
  (customize-group "re-builder"))

(defun casual-re-builder-about-re-builder ()
  "Casual RE-Builder is a Transient menu for RE-Builder.

Learn more about using Casual RE-Builder at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual RE-Builder, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual RE-Builder was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual RE-Builder.

Always choose love."
  (ignore))

(defun casual-re-builder-about ()
  "About information for Casual RE-Builder."
  (interactive)
  (describe-function #'casual-re-builder-about-re-builder))

(provide 'casual-re-builder-settings)
;;; casual-re-builder-settings.el ends here
