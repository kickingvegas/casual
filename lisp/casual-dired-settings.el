;;; casual-dired-settings.el --- Casual Dired Settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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
(require 'dired)
(require 'dired-aux)
(require 'wdired)
(require 'casual-lib)
(require 'casual-dired-utils)
(require 'casual-dired-variables)


;;; Menus
(transient-define-prefix casual-dired-settings-tmenu ()
  ["Dired Settings"
   ["Customize"
    ("r" "Revert Policy"
     casual-dired--customize-dired-auto-revert-buffer
     :transient nil)
    ("t" "Target Directory"
     casual-dired--customize-dired-dwim-target
     :transient nil)
    ("T" "Use System Trash Can"
     casual-dired--customize-delete-by-moving-to-trash
     :description (lambda ()
                   (casual-lib-checkbox-label
                    delete-by-moving-to-trash
                    "Use System Trash Can"))
     :transient nil)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)
    ("R" "Rename via VC"
     casual-dired--customize-dired-vc-rename-file
     :description (lambda ()
                   (casual-lib-checkbox-label
                    dired-vc-rename-file
                    "Rename via VC"))
     :transient nil)]

   ["GNU ‘ls’"
    ("l" "Use GNU ‘ls’ with “--dired”"
     casual-dired--customize-dired-use-ls-dired
     :transient nil)
    ("s" "Initial Listing Switches"
     casual-dired--customize-dired-listing-switches
     :transient nil)
    ("c" "Initial Listing Switches for “Sort By” Menu"
     casual-dired--customize-casual-dired-listing-switches
     :transient nil)]]

  [["wdired"
    ("p" "Allow Changing Permissions"
     casual-dired--customize-wdired-allow-to-change-permissions
     :description (lambda ()
                   (casual-lib-checkbox-label
                    wdired-allow-to-change-permissions
                    "Allow Changing Permissions"))
     :transient nil)

    ("L" "Allow Redirecting Links"
     casual-dired--customize-wdired-allow-to-redirect-links
     :description (lambda ()
                   (casual-lib-checkbox-label
                    wdired-allow-to-redirect-links
                    "Allow Redirecting Links"))
     :transient nil)]

   ["Dired"
    ("d" "Dired Group"
     casual-dired--customize-dired-group
     :transient nil)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-dired-about :transient nil)

          (casual-lib-quit-all)])

;;; Functions

(defun casual-dired--customize-dired-use-ls-dired ()
  "Customize if “--dired” switch is passed to ‘ls’.

Customize the variable `dired-use-ls-dired'.  If
`dired-use-ls-dired' is non-nil, then pass the “--dired” option
to ‘ls’.

NOTE: If `dired-use-ls-dired' is t, Casual Dired presumes that
GNU ‘ls’ is installed."
  (interactive)
  (customize-variable 'dired-use-ls-dired))

(defun casual-dired--customize-dired-auto-revert-buffer ()
  "Customize Dired revert buffer policy.

Customize the variable `dired-auto-revert-buffer' which
determines if Dired buffers are automatically reverted on
revisiting their directory."
  (interactive)
  (customize-variable 'dired-auto-revert-buffer))

(defun casual-dired--customize-dired-dwim-target ()
  "Customize `dired-dwim-target'.

If non-nil, Dired tries to guess a default target directory."
  (interactive)
  (customize-variable 'dired-dwim-target))

(defun casual-dired--customize-dired-listing-switches ()
  "Customize `dired-listing-switches'.

Switches passed to ‘ls’ for Dired.  MUST contain the ‘l’ option."
  (interactive)
  (customize-variable 'dired-listing-switches))

(defun casual-dired--customize-dired-vc-rename-file ()
    "Customize `dired-vc-rename-file'.

This variable configures whether Dired should register file
renaming in underlying vc system."
  (interactive)
  (customize-variable 'dired-vc-rename-file))

(defun casual-dired--customize-casual-dired-listing-switches ()
  "Customize `casual-dired-listing-switches'.

Customizes switches that initialize the Casual Dired “Sort By”
menu (`casual-dired-sort-by-tmenu').  Note that this variable is
unused if the user saves the state of this Transient menu.

To avoid unforeseen dependencies, this variable is independent
from the value of `dired-listing-switches'.  That said, it is
recommended to set both `dired-listing-switches' and
`casual-dired-listing-switches' to be consistent with each other."
  (interactive)
  (customize-variable 'casual-dired-listing-switches))

(defun casual-dired--customize-delete-by-moving-to-trash ()
  "Customize `delete-by-moving-to-trash'.

Customize the variable `delete-by-moving-to-trash'.  Specifies
whether to use the system’s trash can."
  (interactive)
  (customize-variable 'delete-by-moving-to-trash))

(defun casual-dired--customize-wdired-allow-to-change-permissions ()
  "Customize `wdired-allow-to-change-permissions'.

Customize the variable `wdired-allow-to-change-permissions'.
If non-nil, the permissions bits of the files are editable."
  (interactive)
  (customize-variable 'wdired-allow-to-change-permissions))

(defun casual-dired--customize-wdired-allow-to-redirect-links ()
  "Customize `wdired-allow-to-redirect-links'.

Customize the variable `wdired-allow-to-redirect-links'.
If non-nil, the permissions bits of the files are editable."
  (interactive)
  (customize-variable 'wdired-allow-to-redirect-links))

(defun casual-dired--customize-dired-group ()
  "Call the Dired customization group."
  (interactive)
  (customize-group "dired"))

(defun casual-dired-about-dired ()
  "Casual Dired is an opinionated user interface for the Emacs file manager Dired.

Learn more about using Casual Dired at our discussion group on GitHub.
Any questions or comments about Casual should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Dired, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Dired was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Dired.

Always choose love."
  (ignore))

(defun casual-dired-about ()
  "About information for Casual Dired."
  (interactive)
  (describe-function #'casual-dired-about-dired))

(provide 'casual-dired-settings)
;;; casual-dired-settings.el ends here
