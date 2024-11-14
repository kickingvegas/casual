;;; casual-agenda-settings.el --- Casual Agenda Settings -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'org-agenda)
(require 'casual-lib)

(defun casual-agenda--customize-group ()
  "Customize Agenda group."
  (interactive)
  (customize-group "org-agenda"))

(defun casual-agenda-about-agenda ()
  "Casual Agenda is a Transient menu for Agenda.

Learn more about using Casual Agenda at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Agenda, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Agenda was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Agenda.

Always choose love."
  (ignore))

(defun casual-agenda-about ()
  "About information for Casual Agenda."
  (interactive)
  (describe-function #'casual-agenda-about-agenda))


(transient-define-prefix casual-agenda-settings-tmenu ()
  "Settings."
  ["Modes"
   [("l" "Log" org-agenda-log-mode
     :description (lambda () (casual-lib-checkbox-label org-agenda-show-log "Log"))
     :transient t)
    ("G" "Grid" org-agenda-toggle-time-grid
     :description (lambda () (casual-lib-checkbox-label org-agenda-use-time-grid "Grid"))
     :transient t)]
   [("D" "Diary" org-agenda-toggle-diary
     :description (lambda () (casual-lib-checkbox-label org-agenda-include-diary "Diary"))
     :transient t)
    ("F" "Follow" org-agenda-follow-mode
     :description (lambda () (casual-lib-checkbox-label org-agenda-follow-mode "Follow"))
     :transient t)]
   [("R" "Clock Report" org-agenda-clockreport-mode
     :description (lambda () (casual-lib-checkbox-label org-agenda-clockreport-mode "Clock Report"))
     :transient t)
    ("E" "Entry" org-agenda-entry-text-mode
     :description (lambda () (casual-lib-checkbox-label org-agenda-entry-text-mode "Entry"))
     :transient t)]]

  ["Customize"
   [("f" "Agenda Files" casual-agenda-customize-agenda-files)
    ("d" casual-agenda-customize-agenda-include-diary
     :description (lambda ()
                    (casual-lib-checkbox-label org-agenda-include-diary "Include Diary")))
    ("i" casual-agenda-customize-agenda-include-inactive-timestamps
     :description (lambda ()
                    (casual-lib-checkbox-label org-agenda-include-inactive-timestamps "Include Inactive Timestamps")))

    ("x" casual-agenda-customize-agenda-include-deadlines
     :description (lambda ()
                    (casual-lib-checkbox-label org-agenda-include-deadlines "Include Deadlines")))

    ("m" casual-agenda-customize-ampm
     :description (lambda ()
                    (casual-lib-checkbox-label org-agenda-timegrid-use-ampm "Use AM/PM")))]

   [("I" "Next state on clock-in" casual-agenda-customize-org-clock-in-switch-to-state)
    ("O" "Next state on clock-out" casual-agenda-customize-org-clock-out-switch-to-state)
    ("," "Agenda Group" casual-agenda--customize-group)
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-agenda-about :transient nil)

          (casual-lib-quit-all)])


(defun casual-agenda-customize-ampm ()
  "Customize variable `org-agenda-timegrid-use-ampm'."
  (interactive)
  (customize-variable 'org-agenda-timegrid-use-ampm))

(defun casual-agenda-customize-agenda-files ()
  "Customize variable `org-agenda-files'."
  (interactive)
  (customize-variable 'org-agenda-files))

(defun casual-agenda-customize-agenda-include-diary ()
  "Customize variable `org-agenda-include-diary'."
  (interactive)
  (customize-variable 'org-agenda-include-diary))

(defun casual-agenda-customize-agenda-include-deadlines ()
  "Customize variable `org-agenda-include-deadlines'."
  (interactive)
  (customize-variable 'org-agenda-include-deadlines))

(defun casual-agenda-customize-agenda-include-inactive-timestamps ()
  "Customize variable `org-agenda-include-inactive-timestamps'."
  (interactive)
  (customize-variable 'org-agenda-include-inactive-timestamps))

(defun casual-agenda-customize-org-clock-in-switch-to-state ()
  "Customize variable `org-clock-in-switch-to-state'."
  (interactive)
  (customize-variable 'org-clock-in-switch-to-state))

(defun casual-agenda-customize-org-clock-out-switch-to-state ()
  "Customize variable `org-clock-out-switch-to-state'."
  (interactive)
  (customize-variable 'org-clock-out-switch-to-state))

(provide 'casual-agenda-settings)
;;; casual-agenda-settings.el ends here
