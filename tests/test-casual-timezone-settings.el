;;; test-casual-timezone-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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
(require 'casual-timezone-test-utils)
(require 'casual-timezone-settings)

(ert-deftest test-casual-timezone-settings-tmenu ()
  (let ()
    (casualt-timezone-setup)
    (cl-letf ((casualt-mock #'casual-timezone-about)
              (casualt-mock #'casual-timezone--customize-working-hours-range)
              (casualt-mock #'casual-timezone--customize-working-hour-glyph)
              (casualt-mock #'casual-timezone--customize-planner-working-highlight)
              (casualt-mock #'casual-timezone--customize-convert-timestamp-format)
              (casualt-mock #'casual-timezone--customize-datestamp-format)
              (casualt-mock #'casual-timezone--describe-format-time-string))

      (let ((test-vectors
             '((:binding "r" :command casual-timezone--customize-working-hours-range)
               (:binding "g" :command casual-timezone--customize-working-hour-glyph)
               (:binding "F" :command casual-timezone--customize-planner-working-highlight)
               (:binding "c" :command casual-timezone--customize-convert-timestamp-format)
               (:binding "p" :command casual-timezone--customize-datestamp-format)
               (:binding "f" :command casual-timezone--describe-format-time-string)
               (:binding "a" :command casual-timezone-about)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-timezone-settings-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-timezone-breakdown)))

(ert-deftest test-casual-timezone-about ()
  (should (stringp (casual-timezone-about))))

(provide 'test-casual-timezone-settings)
;;; test-casual-timezone-setttings.el ends here
