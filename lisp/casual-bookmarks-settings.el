;;; casual-bookmarks-settings.el --- Casual Bookmarks Settings -*- lexical-binding: t; -*-

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
(require 'casual-lib)


(transient-define-prefix casual-bookmarks-settings-tmenu ()
  "Casual Bookmarks settings menu."
  ["Bookmarks: Settings"
   ["Customize"
    ("G" "Bookmark Group" casual-bookmarks--customize-group)
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-bookmarks-about :transient nil)

          (casual-lib-quit-all)])

(defun casual-bookmarks--customize-group ()
  "Customize Bookmarks group."
  (interactive)
  (customize-group "bookmark"))

(defun casual-bookmarks-about-bookmarks ()
  "Casual Bookmarks is a Transient user interface for Bookmarks.

Learn more about using Casual Bookmarks at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Bookmarks, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Bookmarks was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Bookmarks.

Always choose love."
  (ignore))

(defun casual-bookmarks-about ()
  "About information for Casual Bookmarks."
  (interactive)
  (describe-function #'casual-bookmarks-about-bookmarks))

(provide 'casual-bookmarks-settings)
;;; casual-bookmarks-settings.el ends here
