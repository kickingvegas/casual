;;; casual-editkit-settings.el --- Casual Bookmarks Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools, wp

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
(require 'casual-editkit-version)

(transient-define-prefix casual-editkit-settings-tmenu ()
  "Casual EditKit settings menu."
  ["Casual EditKit: Settings"
   ["Customize"
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-editkit-about :transient nil)
   ("v" "Version" casual-editkit-version :transient nil)
   (casual-lib-quit-all)])

(defun casual-editkit-about-editkit ()
  "Casual EditKit is a user interface library for Emacs editing commands.

Casual EditKit uses the Transient library to implement its user
interfaces.

Learn more about using Casual EditKit at our discussion
group on GitHub. Any questions or comments about it should be
made there.
URL `https://github.com/kickingvegas/casual-editkit/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual-editkit/issues'

If you enjoy using Casual EditKit, consider making a
modest financial contribution to help support its development and
maintenance. URL `https://www.buymeacoffee.com/kickingvegas'

Casual EditKit was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual EditKit.

Always choose love."
  (ignore))

(defun casual-editkit-about ()
  "About information for Casual EditKit."
  (interactive)
  (describe-function #'casual-editkit-about-editkit))

(provide 'casual-editkit-settings)
;;; casual-editkit-settings.el ends here
