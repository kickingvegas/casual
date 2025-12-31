;;; test-casual-css-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'casual-css-test-utils)
(require 'casual-css-settings)

(ert-deftest test-casual-css-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-css--customize-indent-offset)
              (casualt-mock #'casual-css--customize-group)
              (casualt-mock #'casual-css-about))

      (let ((test-vectors
             '((:binding "o" :command casual-css--customize-indent-offset)
               (:binding "G" :command casual-css--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-css-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-css-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-css-about ()
  (should (stringp (casual-css-about))))

(provide 'test-casual-css-settings)
;;; test-casual-css-setttings.el ends here
