;;; test-casual-elisp-settings.el --- Casual Elisp Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-elisp-test-utils)
(require 'casual-elisp-settings)

(ert-deftest test-casual-elisp-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-lib-customize-casual-lib-hide-navigation)
              (casualt-mock #'casual-lib-customize-casual-lib-use-unicode)
              (casualt-mock #'casual-elisp-about))

      (let ((test-vectors
             '((:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-elisp-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-elisp-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-elisp-about ()
  (should (stringp (casual-elisp-about))))

(provide 'test-casual-elisp-settings)
;;; test-casual-elisp-setttings.el ends here
