;;; test-casual-calendar-settings.el --- Casual Calendar Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'casual-calendar-test-utils)
(require 'casual-calendar-settings)

(ert-deftest test-casual-calendar-settings-tmenu ()
  (let ()
    (casualt-calendar-setup)
    (cl-letf ()
      (let ((test-vectors
             '((:binding "C" :command casual-calendar--customize-calendar-group)
               (:binding "H" :command casual-calendar--customize-calendar-mark-holidays-flag)
               (:binding "E" :command casual-calendar--customize-calendar-mark-diary-entries-flag)
               (:binding "v" :command casual-calendar--customize-calendar-move-hook)
               (:binding "N" :command casual-calendar--customize-calendar-location-name)
               (:binding "A" :command casual-calendar--customize-calendar-latitude)
               (:binding "O" :command casual-calendar--customize-calendar-longitude)
               (:binding "D" :command casual-calendar--customize-diary-group)
               (:binding "l" :command casual-calendar--customize-diary-list-entries-hook)
               (:binding "m" :command casual-calendar--customize-diary-mark-entries-hook)
               (:binding "L" :command casual-calendar--customize-diary-nongregorian-listing-hook)
               (:binding "M" :command casual-calendar--customize-diary-nongregorian-marking-hook)
               (:binding "d" :command casual-calendar--customize-org-agenda-include-diary)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-calendar-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-calendar-settings-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-calendar-breakdown)))

(ert-deftest test-casual-calendar-about ()
  (should (stringp (casual-calendar-about))))

(provide 'test-casual-calendar-settings)
;;; test-casual-calendar-settings.el ends here
