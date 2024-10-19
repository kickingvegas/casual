;;; casual-isearch-settings.el --- Casual Re-Builder Settings -*- lexical-binding: t; -*-

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
(require 'isearch)
(require 'casual-lib)


(transient-define-prefix casual-isearch-settings-tmenu ()
  "Casual I-Search settings menu."
  ["I-Search: Settings"
   ["Customize"
    ("G" "I-Search Group" casual-isearch--customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-isearch-about :transient nil)

          (casual-lib-quit-all)])

(defun casual-isearch--customize-group ()
  "Customize I-Search group."
  (interactive)
  (customize-group "isearch"))

(defun casual-isearch-about-isearch ()
  "Casual I-Search is a Transient menu for I-Search.

Learn more about using Casual I-Search at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual I-Search, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual I-Search was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual I-Search.

Always choose love."
  (ignore))

(defun casual-isearch-about ()
  "About information for Casual I-Search."
  (interactive)
  (describe-function #'casual-isearch-about-isearch))

(provide 'casual-isearch-settings)
;;; casual-isearch-settings.el ends here
