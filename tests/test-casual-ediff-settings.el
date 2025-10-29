;;; test-casual-ediff-settings.el --- Casual Elisp Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-ediff-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-ediff-settings)

(ert-deftest test-casual-ediff-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-ediff-customize-group)
              (casualt-mock #'casual-ediff-customize-ediff-keep-variants)
              (casualt-mock #'casual-ediff-customize-ediff-split-window-function)
              (casualt-mock #'casual-ediff-customize-ediff-window-setup-function)
              (casualt-mock #'casual-lib-customize-casual-lib-hide-navigation)
              (casualt-mock #'casual-lib-customize-casual-lib-use-unicode)
              (casualt-mock #'casual-ediff-about))

      (let ((test-vectors
             '(
               (:binding "k" :command casual-ediff-customize-ediff-keep-variants)
               (:binding "w" :command casual-ediff-customize-ediff-window-setup-function)
               (:binding "s" :command casual-ediff-customize-ediff-split-window-function)
               (:binding "G" :command casual-ediff-customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-ediff-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-ediff-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-ediff-about ()
  (should (stringp (casual-ediff-about))))

(provide 'test-casual-ediff-settings)
;;; test-casual-ediff-setttings.el ends here
