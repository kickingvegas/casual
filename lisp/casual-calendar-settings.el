;;; casual-calendar-settings.el --- Casual Calendar Settings -*- lexical-binding: t; -*-

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
(require 'solar)
(require 'org-agenda)
(require 'casual-calendar-constants)

(transient-define-prefix casual-calendar-settings-tmenu ()
  "Casual Calendar & Diary settings menu.

Customize settings for Calendar and Diary modes."

  ["Calendar"
   ["Customize"
    ("C" "Calendar Group" casual-calendar--customize-calendar-group)
    ("H" "Mark Holidays" casual-calendar--customize-calendar-mark-holidays-flag
     :description (lambda ()
                    (casual-lib-checkbox-label
                     calendar-mark-holidays-flag
                     "Mark Holidays")))
    ("E" "Mark Diary Entries" casual-calendar--customize-calendar-mark-diary-entries-flag
     :description (lambda ()
                    (casual-lib-checkbox-label
                     calendar-mark-diary-entries-flag
                     "Mark Diary Entries")))]

   ["Hooks"
    ("v" "Move Hook" casual-calendar--customize-calendar-move-hook)]

   ["Location"
    ("N" "Location Name" casual-calendar--customize-calendar-location-name)
    ("A" "Latitude" casual-calendar--customize-calendar-latitude)
    ("O" "Longitude" casual-calendar--customize-calendar-longitude)]]

  ["Diary"
   ["Customize"
    ("D" "Diary Group" casual-calendar--customize-diary-group)]

   ["Hooks"
    ("l" "List" casual-calendar--customize-diary-list-entries-hook)
    ("m" "Mark" casual-calendar--customize-diary-mark-entries-hook)]

   ["Non-Gregorian Hooks"
    ("L" "List" casual-calendar--customize-diary-nongregorian-listing-hook)
    ("M" "Mark" casual-calendar--customize-diary-nongregorian-marking-hook)]

   ["Org Agenda"
    ("d" "Include Diary" casual-calendar--customize-org-agenda-include-diary
     :description (lambda ()
                    (casual-lib-checkbox-label
                     org-agenda-include-diary
                     "Include Diary")))]]

  ["General"
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-calendar-about :transient nil)

          (casual-lib-quit-all)])

(defun casual-calendar--customize-calendar-group ()
  "Customize calendar group."
  (interactive)
  (customize-group "calendar"))

(defun casual-calendar--customize-diary-group ()
  "Customize diary group."
  (interactive)
  (customize-group "diary"))

(defun casual-calendar--customize-org-agenda-include-diary ()
  "Customize variable `org-agenda-include-diary'."
  (interactive)
  (customize-variable 'org-agenda-include-diary))

(defun casual-calendar--customize-diary-list-entries-hook ()
  "Customize variable `diary-list-entries-hook'."
  (interactive)
  (customize-variable 'diary-list-entries-hook))

(defun casual-calendar--customize-diary-mark-entries-hook ()
  "Customize variable `diary-mark-entries-hook'."
  (interactive)
  (customize-variable 'diary-mark-entries-hook))

(defun casual-calendar--customize-diary-nongregorian-listing-hook ()
  "Customize variable `diary-nongregorian-listing-hook'."
  (interactive)
  (customize-variable 'diary-nongregorian-listing-hook))

(defun casual-calendar--customize-diary-nongregorian-marking-hook ()
  "Customize variable `diary-nongregorian-marking-hook'."
  (interactive)
  (customize-variable 'diary-nongregorian-marking-hook))

(defun casual-calendar--customize-calendar-mark-holidays-flag ()
  "Customize variable `calendar-mark-holidays-flag'."
  (interactive)
  (customize-variable 'calendar-mark-holidays-flag))

(defun casual-calendar--customize-calendar-mark-diary-entries-flag ()
  "Customize variable `calendar-mark-diary-entries-flag'."
  (interactive)
  (customize-variable 'calendar-mark-diary-entries-flag))

(defun casual-calendar--customize-calendar-location-name ()
  "Customize variable `calendar-location-name'."
  (interactive)
  (customize-variable 'calendar-location-name))

(defun casual-calendar--customize-calendar-latitude ()
  "Customize variable `calendar-latitude'."
  (interactive)
  (customize-variable 'calendar-latitude))

(defun casual-calendar--customize-calendar-longitude ()
  "Customize variable `calendar-longitude'."
  (interactive)
  (customize-variable 'calendar-longitude))

(defun casual-calendar--customize-calendar-move-hook ()
  "Customize variable `calendar-move-hook'."
  (interactive)
  (customize-variable 'calendar-move-hook))


(defun casual-calendar-about-calendar ()
  "Casual Calendar is a Transient menu for Calendar.

Learn more about using Casual Calendar at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Calendar, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Calendar was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Calendar.

Always choose love."
  (ignore))

(defun casual-calendar-about ()
  "About information for Casual Calendar."
  (interactive)
  (describe-function #'casual-calendar-about-calendar))

(provide 'casual-calendar-settings)
;;; casual-calendar-settings.el ends here
