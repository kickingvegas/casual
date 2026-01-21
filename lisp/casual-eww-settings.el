;;; casual-eww-settings.el --- Casual EWW Settings -*- lexical-binding: t; -*-

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
(require 'eww)
(require 'casual-lib)

(transient-define-prefix casual-eww-settings-tmenu ()
  "Casual EWW settings menu."
  ["Casual EWW: Settings"

   ["Browsing"
    ("r" "Retrieve Command" casual-eww--customize-retrieve-command)
    ("R" "Readable URLs" casual-eww--customize-readable-urls)
    ("h" "History Limit" casual-eww--customize-history-limit
     :description (lambda () (format "History Limit (%d)" eww-history-limit)))]

   ["Directory"
    ("d" "Download" casual-eww--customize-download-directory
     :description (lambda () (format "Directory (%s)" (eww--download-directory))))
    ("b" "Bookmarks" casual-eww--customize-bookmarks-directory
     :description (lambda () (format "Directory (%s)" eww-bookmarks-directory)))]

   ["Display"
    ("f" "Use Fonts" casual-eww--customize-shr-use-fonts
     :description (lambda () (casual-lib-checkbox-label
                         shr-use-fonts "Use Fonts"))
     :transient t)
    ("c" "Use Colors" casual-eww--customize-shr-use-colors
     :description (lambda () (casual-lib-checkbox-label
                         shr-use-colors "Use Colors"))
     :transient t)
    ("i" "Inhibit Images" casual-eww--customize-shr-inhibit-images
     :description (lambda () (casual-lib-checkbox-label
                         shr-inhibit-images "Inhibit Images"))
     :transient t)]]

  ["General"
   :class transient-row
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-eww-about :transient nil)
   ("G" "EWW Group" casual-eww--customize-group)
   (casual-lib-quit-all)])

(defun casual-eww--customize-group ()
  "Customize EWW group."
  (interactive)
  (customize-group "eww"))

(defun casual-eww--customize-retrieve-command ()
  "Customize `eww-retrieve-commmand'."
  (interactive)
  (customize-variable 'eww-retrieve-command))

(defun casual-eww--customize-readable-urls ()
  "Customize `eww-readable-urls'."
  (interactive)
  (customize-variable 'eww-readable-urls))

(defun casual-eww--customize-download-directory ()
  "Customize `eww-download-directory'."
  (interactive)
  (customize-variable 'eww-download-directory))

(defun casual-eww--customize-bookmarks-directory ()
  "Customize `eww-bookmarks-directory'."
  (interactive)
  (customize-variable 'eww-bookmarks-directory))

(defun casual-eww--customize-history-limit ()
  "Customize `eww-history-limit'."
  (interactive)
  (customize-variable 'eww-history-limit))

(defun casual-eww--customize-shr-use-fonts ()
  "Customize `shr-use-fonts'."
  (interactive)
  (customize-variable 'shr-use-fonts))

(defun casual-eww--customize-shr-use-colors ()
  "Customize `shr-use-colors'."
  (interactive)
  (customize-variable 'shr-use-colors))

(defun casual-eww--customize-shr-inhibit-images ()
  "Customize `shr-inhibit-images'."
  (interactive)
  (customize-variable 'shr-inhibit-images))

(defun casual-eww-about-eww ()
  "Casual EWW is a Transient menu for Emacs Web Wowser (EWW).

Learn more about using Casual EWW at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual EWW, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual EWW was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual EWW.

Always choose love."
  (ignore))

(defun casual-eww-about ()
  "About information for Casual EWW."
  (interactive)
  (describe-function #'casual-eww-about-eww))

(provide 'casual-eww-settings)
;;; casual-eww-settings.el ends here
