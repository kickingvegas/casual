;;; casual-man-settings.el --- Casual Man Settings -*- lexical-binding: t; -*-

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

(transient-define-prefix casual-man-settings-tmenu ()
  "Casual Man settings menu."
  ["Casual Man: Settings"
   [("s" "Switches" casual-man--customize-man-switches)
    ("S" "Call Synchronously" casual-man--customize-man-prefer-synchronous-call)
    ("r" "Support Remote" casual-man--customize-man-support-remote-systems)]

   [("G" "Man Group" casual-man--customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-man-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-man--customize-group ()
  "Customize Manfile group."
  (interactive)
  (customize-group "man"))

(defun casual-man--customize-man-switches ()
  "Switch values passed to the man command, as a single string.

Customize `Man-switches'."
  (interactive)
  (customize-variable 'Man-switches))

(defun casual-man--customize-man-prefer-synchronous-call ()
  "Whether to call the Un*x \"man\" program synchronously.

Customize `Man-prefer-synchronous-call'."
  (interactive)
  (customize-variable 'Man-prefer-synchronous-call))

(defun casual-man--customize-man-support-remote-systems ()
  "Whether to call the Un*x \"man\" program on remote systems.

Customize `Man-support-remote-systems'."
  (interactive)
  (customize-variable 'Man-support-remote-systems))


(defun casual-man-about-man ()
  "Casual Man is a Transient menu for man pages.

Learn more about using Casual Man at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Man, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Man was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual Man.

Always choose love."
  (ignore))

(defun casual-man-about ()
  "About information for Casual Man."
  (interactive)
  (describe-function #'casual-man-about-man))

(provide 'casual-man-settings)
;;; casual-man-settings.el ends here
