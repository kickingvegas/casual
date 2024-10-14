;;; test-casual-editkit-settings.el --- Casual IBuffer Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-editkit-test-utils)
(require 'casual-editkit-settings)

(ert-deftest test-casual-editkit-settings-tmenu ()
  (let ((tmpfile "casual-editkit-settings-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ()
      (let ((test-vectors
             '((:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-editkit-about)
               (:binding "v" :command casual-editkit-version))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-settings-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-about ()
  (should (stringp (casual-editkit-about))))

(provide 'test-casual-editkit-settings)
;;; test-casual-editkit-setttings.el ends here
