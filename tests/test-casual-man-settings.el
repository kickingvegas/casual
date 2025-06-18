;;; test-casual-man-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-man-test-utils)
(require 'casual-man-settings)

(ert-deftest test-casual-man-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-man--customize-group)
              (casualt-mock #'casual-man-about)
              (casualt-mock #'casual-man--customize-man-switches)
              (casualt-mock #'casual-man--customize-man-prefer-synchronous-call)
              (casualt-mock #'casual-man--customize-man-support-remote-systems))

      (let ((test-vectors
             '((:binding "s" :command casual-man--customize-man-switches)
               (:binding "S" :command casual-man--customize-man-prefer-synchronous-call)
               (:binding "r" :command casual-man--customize-man-support-remote-systems)
               (:binding "G" :command casual-man--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-man-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-man-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-man-about ()
  (should (stringp (casual-man-about))))

(provide 'test-casual-man-settings)
;;; test-casual-man-setttings.el ends here
