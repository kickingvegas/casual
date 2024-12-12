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
(require 'simple)
(require 'autorevert)
(require 'delsel)
(require 'saveplace)
(require 'savehist)
(require 'casual-lib)

(transient-define-prefix casual-editkit-settings-tmenu ()
  "Casual EditKit settings menu."
  ["Casual EditKit: Settings"
   ["Format/Layout"
    ("f" "Auto-fill Mode" auto-fill-mode
     :transient t
     :description (lambda ()
                    (casual-lib-checkbox-label
                     auto-fill-function
                     "Auto-fill Mode")))

    ("t" "Indent Tabs Mode" indent-tabs-mode
     :transient t
     :description (lambda ()
                    (casual-lib-checkbox-label
                     indent-tabs-mode
                     "Indent Tabs Mode")))

    ("." "Double Space" casual-editkit--customize-sentence-end-double-space
     :description (lambda ()
                    (casual-lib-checkbox-label
                     sentence-end-double-space
                     "Double Space Sentences")))
    ("C" "Fill Column" set-fill-column
     :description (lambda ()
                    (format "Fill Column (%d)" fill-column)))]

   ["Save"
    ("p" "Save Place Mode" casual-editkit--customize-save-place-mode
     :description (lambda ()
                    (casual-lib-checkbox-label
                     save-place-mode
                     "Save Place Mode")))

    ("h" "Save History Mode" casual-editkit--customize-savehist-mode
     :description (lambda ()
                    (casual-lib-checkbox-label
                     savehist-mode
                     "Save History Mode")))]

   ["Misc"
    ("S" "Delete Selection Mode" casual-editkit--customize-delete-selection-mode
     :description (lambda ()
                    (casual-lib-checkbox-label
                     delete-selection-mode
                     "Delete Selection Mode")))

    ("N" "Require Final Newline" casual-editkit--customize-require-final-newline
     :description (lambda ()
                    (casual-lib-checkbox-label
                     require-final-newline
                     "Require Final Newline")))


    ("v" "View Read-Only" casual-editkit--customize-view-read-only
     :description (lambda ()
                    (casual-lib-checkbox-label
                     view-read-only
                     "View Read-Only")))]]

  [["Global Auto-Revert Buffers"
    ("F" "File Buffers" casual-editkit--customize-global-auto-revert-mode
     :description (lambda ()
                    (casual-lib-checkbox-label
                     global-auto-revert-mode
                     "File Buffers")))

    ("B" "Non-File Buffers" casual-editkit--customize-global-auto-revert-non-file-buffers
     :description (lambda ()
                    (casual-lib-checkbox-label
                     global-auto-revert-non-file-buffers
                     "Non-File Buffers")))]

   ["Casual"
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-editkit-about :transient nil)
          (casual-lib-quit-all)])

(defun casual-editkit-about-editkit ()
  "Casual EditKit is a user interface library for Emacs editing commands.

Casual EditKit uses the Transient library to implement its user
interfaces.

Learn more about using Casual EditKit at our discussion
group on GitHub. Any questions or comments about it should be
made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

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

(defun casual-editkit--customize-sentence-end-double-space ()
  "Customize variable `sentence-end-double-space'."
  (interactive)
  (customize-variable 'sentence-end-double-space))

(defun casual-editkit--customize-delete-selection-mode ()
  "Customize variable `delete-selection-mode'."
  (interactive)
  (customize-variable 'delete-selection-mode))

(defun casual-editkit--customize-require-final-newline ()
  "Customize variable `require-final-newline'."
  (interactive)
  (customize-variable 'require-final-newline))

(defun casual-editkit--customize-save-place-mode ()
  "Customize variable `save-place-mode'."
  (interactive)
  (customize-variable 'save-place-mode))

(defun casual-editkit--customize-savehist-mode ()
  "Customize variable `savehist-mode'."
  (interactive)
  (customize-variable 'savehist-mode))

(defun casual-editkit--customize-view-read-only ()
  "Customize variable `view-read-only'."
  (interactive)
  (customize-variable 'view-read-only))

(defun casual-editkit--customize-global-auto-revert-mode ()
  "Customize variable `global-auto-revert-mode'."
  (interactive)
  (customize-variable 'global-auto-revert-mode))

(defun casual-editkit--customize-global-auto-revert-non-file-buffers ()
  "Customize variable `global-auto-revert-non-file-buffers'."
  (interactive)
  (customize-variable 'global-auto-revert-non-file-buffers))

(provide 'casual-editkit-settings)
;;; casual-editkit-settings.el ends here
