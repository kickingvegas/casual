;;; test-casual-eww-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Y. Choi

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
(require 'casual-eww-test-utils)
(require 'casual-eww-settings)

(ert-deftest test-casual-eww-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-eww--customize-retrieve-command)
              (casualt-mock #'casual-eww--customize-readable-urls)
              (casualt-mock #'casual-eww--customize-history-limit)
              (casualt-mock #'casual-eww--customize-download-directory)
              (casualt-mock #'casual-eww--customize-bookmarks-directory)
              (casualt-mock #'casual-eww--customize-shr-use-fonts)
              (casualt-mock #'casual-eww--customize-shr-use-colors)
              (casualt-mock #'casual-eww--customize-shr-inhibit-images)
              (casualt-mock #'casual-eww--customize-group)
              (casualt-mock #'casual-eww-about))

      (let ((test-vectors
             '((:binding "r" :command casual-eww--customize-retrieve-command)
               (:binding "R" :command casual-eww--customize-readable-urls)
               (:binding "h" :command casual-eww--customize-history-limit)
               (:binding "d" :command casual-eww--customize-download-directory)
               (:binding "b" :command casual-eww--customize-bookmarks-directory)
               (:binding "f" :command casual-eww--customize-shr-use-fonts)
               (:binding "c" :command casual-eww--customize-shr-use-colors)
               (:binding "i" :command casual-eww--customize-shr-inhibit-images)
               (:binding "G" :command casual-eww--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-eww-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-eww-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-eww-about ()
  (should (stringp (casual-eww-about))))

(provide 'test-casual-eww-settings)
;;; test-casual-eww-setttings.el ends here
