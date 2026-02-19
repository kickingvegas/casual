;;; casual-org-settings.el --- Casual Org Settings -*- lexical-binding: t; -*-

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

;;; Code:
(require 'org)
(require 'casual-lib)


;; -------------------------------------------------------------------
;; Customize Functions

;; Groups
(defun casual-org--customize-group ()
  "Customize Org group."
  (interactive)
  (customize-group "org"))

(defun casual-org--customize-group-priorities ()
  "Customize Org Priorities group."
  (interactive)
  (customize-group "Org Priorities"))

(defun casual-org--customize-group-goto ()
  "Customize Org Goto group."
  (interactive)
  (customize-group "Org Goto"))


;; Customize Variables
(defun casual-org--customize-directory ()
  "Customize variable `org-directory'."
  (interactive)
  (customize-variable 'org-directory))

(defun casual-org--customize-default-notes-file ()
  "Customize variable `org-default-notes-file'."
  (interactive)
  (customize-variable 'org-default-notes-file))

(defun casual-org--customize-todo-keywords ()
  "Customize variable `org-todo-keywords'."
  (interactive)
  (customize-variable 'org-todo-keywords))

(defun casual-org--customize-refile-targets ()
  "Customize variable `org-refile-targets'."
  (interactive)
  (customize-variable 'org-refile-targets))

(defun casual-org--customize-imenu-depth ()
  "Customize variable `org-imenu-depth'."
  (interactive)
  (customize-variable 'org-imenu-depth))

(defun casual-org--customize-support-shift-select ()
  "Customize variable `org-support-shift-select'."
  (interactive)
  (customize-variable 'org-support-shift-select))

(defun casual-org--customize-startup-folded ()
  "Customize variable `org-startup-folded'."
  (interactive)
  (customize-variable 'org-startup-folded))

(defun casual-org--customize-startup-indented ()
  "Customize variable `org-startup-indented'."
  (interactive)
  (customize-variable 'org-startup-indented))

(defun casual-org--customize-show-notification-handler ()
  "Customize variable `org-show-notification-handler'."
  (interactive)
  (customize-variable 'org-show-notification-handler))

(defun casual-org--customize-log-done ()
  "Customize variable `org-log-done'."
  (interactive)
  (customize-variable 'org-log-done))

(defun casual-org--customize-insert-heading-respect-content ()
  "Customize variable `org-insert-heading-respect-content'."
  (interactive)
  (customize-variable 'org-insert-heading-respect-content))

(defun casual-org--customize-hide-leading-stars ()
  "Customize variable `casual-org-hide-leading-stars'."
  (interactive)
  (customize-variable 'org-hide-leading-stars))

(defun casual-org--customize-hide-emphasis-markers ()
  "Customize variable `org-hide-emphasis-markers'."
  (interactive)
  (customize-variable 'org-hide-emphasis-markers))

(defun casual-org--customize-use-speed-commands ()
  "Customize variable `org-use-speed-commands'."
  (interactive)
  (customize-variable 'org-use-speed-commands))

(defun casual-org--customize-yank-image-save-method ()
  "Customize variable `org-yank-image-save-method'."
  (interactive)
  (customize-variable 'org-yank-image-save-method))

(defun casual-org-about-org ()
  "Casual Org is a Transient menu for Org mode (`org-mode').

Learn more about using Casual Org at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Org, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Org was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Casual Org.

Always choose love."
  (ignore))

(defun casual-org-about ()
  "About information for Casual Org."
  (interactive)
  (describe-function #'casual-org-about-org))


;; -------------------------------------------------------------------
;; Transients
(transient-define-prefix casual-org-settings-tmenu ()
  "Casual Org settings menu."
  ["Casual Org Settings"

   [("h" "Heading & TODO›" casual-org-settings-heading-tmenu)
    ("f" "Files & Directories›" casual-org-settings-files-tmenu)
    ("d" "Display›" casual-org-settings-display-tmenu)]

   [("k" "Keyboard›" casual-org-settings-keyboard-tmenu)
    ("c" "Clocks›" casual-org-settings-clock-tmenu)
    ("y" "Copy & Paste›" casual-org-settings-killyank-tmenu)]

   [("G" "Org Group" casual-org--customize-group)]]

  ["General"
   :class transient-row
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-org-about :transient nil)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])

(transient-define-prefix casual-org-settings-heading-tmenu ()
  "Customize Org heading settings."
  ["Casual Org Settings: Heading"
   ["TODO"
    ("k" "TODO Keywords" casual-org--customize-todo-keywords)
    ("l" "Log Done" casual-org--customize-log-done)
    ("p" "Priorities" casual-org--customize-group-priorities)]

   ["Heading"
    ("i" "Index Menu Depth" casual-org--customize-imenu-depth
     :description (lambda () (format "imenu Depth (%d)" org-imenu-depth)))
    ("r" "Insertion Style" casual-org--customize-insert-heading-respect-content)]

   ["Goto"
    ("g" "Goto" casual-org--customize-group-goto)]]

  casual-lib-navigation-group-with-return)

(transient-define-prefix casual-org-settings-files-tmenu ()
  "Customize Org file and directory settings."
  ["Casual Org Settings: Files & Directories"
   ("o" "Default Directory" casual-org--customize-directory
    :description (lambda () (format "Default Directory (%s)" org-directory)))
   ("n" "Default Notes File" casual-org--customize-default-notes-file
    :description (lambda () (format "Default Notes File (%s)" org-default-notes-file)))
   ("r" "Refile Targets" casual-org--customize-refile-targets)]
  casual-lib-navigation-group-with-return)

(transient-define-prefix casual-org-settings-display-tmenu ()
  "Customize Org display settings. ."
  ["Casual Org Settings: Display"
   ["Hide"
    ("s" "Leading Stars" casual-org--customize-hide-leading-stars
     :description (lambda () (casual-lib-checkbox-label org-hide-leading-stars
                                                   "Hide Leading Stars")))
    ("e" "Emphasis Markers" casual-org--customize-hide-emphasis-markers
     :description (lambda () (casual-lib-checkbox-label org-hide-emphasis-markers
                                                   "Hide Emphasis Markers")))]
   ["Startup"
    ("f" "Folded" casual-org--customize-startup-folded
     :description (lambda () (format "Startup Folded (%s)" org-startup-folded)))

    ("i" "Indented" casual-org--customize-startup-indented
     :description (lambda () (format "Startup Indented (%s)" org-startup-indented)))]]
  casual-lib-navigation-group-with-return)

(transient-define-prefix casual-org-settings-keyboard-tmenu ()
  "Customize Org keyboard settings."
  ["Casual Org Settings: Keyboard"
   ("s" "Support Shift Select" casual-org--customize-support-shift-select
    :description (lambda () (casual-lib-checkbox-label org-support-shift-select
                                                  "Shift Select")))
   ("S" "Use Speed Commands" casual-org--customize-use-speed-commands
    :description (lambda () (casual-lib-checkbox-label org-use-speed-commands
                                                  "Speed Commands")))]
  casual-lib-navigation-group-with-return)

(transient-define-prefix casual-org-settings-clock-tmenu ()
  "Customize Org clock settings."
  ["Casual Org Settings: Clock"
   ("n" "Notification Handler" casual-org--customize-show-notification-handler)]
  casual-lib-navigation-group-with-return)

(transient-define-prefix casual-org-settings-killyank-tmenu ()
  "Customize Org copy & paste (kill & yank) settings."
  ["Casual Org Settings: Copy & Paste (Kill & Yank)"
   ("y" "Yank Image Save Method" casual-org--customize-yank-image-save-method)]
  casual-lib-navigation-group-with-return)

(provide 'casual-org-settings)
;;; casual-org-settings.el ends here
