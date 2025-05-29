;;; casual-timezone-settings.el --- Casual Timezone Settings -*- lexical-binding: t; -*-

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
(require 'casual-timezone-utils)

(transient-define-prefix casual-timezone-settings-tmenu ()
  "Casual Timezone settings menu."
  ["Timezone: Settings"
   ["Working Hours"
    ("r" "Range" casual-timezone--customize-working-hours-range
     :description (lambda ()
                    (format
                     "Range (%d..%d)"
                     (map-elt casual-timezone-working-hours-range :start)
                     (map-elt casual-timezone-working-hours-range :stop))))

    ("g" "Glyph" casual-timezone--customize-working-hour-glyph
     :description (lambda ()
                    (format
                     "Glyph (%s)"
                     casual-timezone-working-hour-glyph)))

    ("F" "Face" casual-timezone--customize-planner-working-highlight)]

   ["Formats"
    ("c" "Convert" casual-timezone--customize-convert-timestamp-format
     :description (lambda ()
                    (format
                     "Convert: (ex: %s)"
                     (format-time-string
                     casual-timezone-convert-datestamp-format
                     (current-time)))))

    ("p" "Planner" casual-timezone--customize-datestamp-format
     :description (lambda ()
                    (format
                     "Planner: (ex: %s)"
                     (format-time-string
                     casual-timezone-datestamp-format
                     (current-time)))))
    ("f" "Describe Format" casual-timezone--describe-format-time-string)]]

  [:class transient-row
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)]

  [:class transient-row
   (casual-lib-quit-one)
   ("a" "About" casual-timezone-about :transient nil)

   (casual-lib-quit-all)])

(defun casual-timezone--customize-working-hour-glyph ()
  "Set working hour glyph.

This customizes the variable `casual-timezone-working-hour-glyph'."
  (interactive)
  (customize-variable 'casual-timezone-working-hour-glyph))

(defun casual-timezone--customize-planner-working-highlight ()
  "Set working hour highlight face.

This customizes the face `casual-timezone-working-highlight'."
  (interactive)
  (customize-face 'casual-timezone-planner-working-highlight))

(defun casual-timezone--customize-working-hours-range ()
  "Set working hours range.

This customizes the variable `casual-timezone-working-hours-range'."
  (interactive)
  (customize-variable 'casual-timezone-working-hours-range))

(defun casual-timezone--customize-convert-timestamp-format ()
  "Set conversion timestamp format.

This customizes the variable `casual-timezone-convert-datestamp-format'."
  (interactive)
  (customize-variable 'casual-timezone-convert-datestamp-format))

(defun casual-timezone--customize-datestamp-format ()
  "Set planner timestamp format.

This customizes the variable `casual-timezone-datestamp-format'."
  (interactive)
  (customize-variable 'casual-timezone-datestamp-format))

(defun casual-timezone--describe-format-time-string ()
  "Describe time string format.

This describes the command `format-time-string'."
  (interactive)
  (describe-command #'format-time-string))

(defun casual-timezone-about ()
  "Casual Timezone is a Transient menu for working with timezones.

Learn more about using Casual Timezone at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual/issues'

If you enjoy using Casual Timezone, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Timezone was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Timezone.

Always choose love."
  (interactive)
  (describe-function #'casual-timezone-about))

(provide 'casual-timezone-settings)
;;; casual-timezone-settings.el ends here
