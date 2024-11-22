;;; test-casual-agenda-settings.el --- Casual Agenda Settings Tests  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'casual-agenda-test-utils)
(require 'casual-agenda-settings)

(ert-deftest test-casual-agenda-about ()
  (should (stringp (casual-agenda-about))))

(ert-deftest test-casual-agenda-settings-tmenu ()
  (casualt-agenda-setup)
  (cl-letf (((symbol-function #'casual-agenda-headlinep) (lambda () t))
            (casualt-mock #'org-agenda-log-mode)
            (casualt-mock #'org-agenda-toggle-time-grid)
            (casualt-mock #'org-agenda-toggle-diary)
            (casualt-mock #'org-agenda-follow-mode)
            (casualt-mock #'org-agenda-clockreport-mode)
            (casualt-mock #'org-agenda-entry-text-mode))

    (let ((test-vectors
           '((:binding "l" :command org-agenda-log-mode)
             (:binding "G" :command org-agenda-toggle-time-grid)
             (:binding "D" :command org-agenda-toggle-diary)
             (:binding "F" :command org-agenda-follow-mode)
             (:binding "R" :command org-agenda-clockreport-mode)
             (:binding "E" :command org-agenda-entry-text-mode)

             (:binding "f" :command casual-agenda-customize-agenda-files)
             (:binding "d" :command casual-agenda-customize-agenda-include-diary)
             (:binding "i" :command casual-agenda-customize-agenda-include-inactive-timestamps)
             (:binding "x" :command casual-agenda-customize-agenda-include-deadlines)
             (:binding "m" :command casual-agenda-customize-ampm)
             (:binding "I" :command casual-agenda-customize-org-clock-in-switch-to-state)
             (:binding "O" :command casual-agenda-customize-org-clock-out-switch-to-state)
             (:binding "," :command casual-agenda--customize-group))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-agenda-settings-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-agenda-breakdown t))

(provide 'test-casual-agenda-settings)
;;; test-casual-agenda-setttings.el ends here
