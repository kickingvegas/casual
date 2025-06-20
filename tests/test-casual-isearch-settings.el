;;; test-casual-isearch-settings.el --- Casual Re-Builder Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-lib-test-utils)
(require 'casual-isearch-test-utils)
(require 'casual-isearch-settings)

(ert-deftest test-casual-isearch-settings-tmenu ()
  (let ()
    (casualt-setup)
    (cl-letf ((casualt-mock #'casual-isearch--customize-group)
              (casualt-mock #'casual-isearch--customize-allow-scroll)
              (casualt-mock #'casual-isearch--customize-allow-motion)
              (casualt-mock #'casual-isearch--customize-lazy-count)
              (casualt-mock #'casual-isearch--customize-lazy-highlight))

      (let ((test-vectors
             '((:binding "s" :command casual-isearch--customize-allow-scroll)
               (:binding "m" :command casual-isearch--customize-allow-motion)
               (:binding "h" :command casual-isearch--customize-lazy-highlight)
               (:binding "c" :command casual-isearch--customize-lazy-count)
               (:binding "G" :command casual-isearch--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-isearch-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-isearch-settings-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-breakdown)))

(ert-deftest test-casual-isearch-about ()
  (should (stringp (casual-isearch-about))))

(provide 'test-casual-isearch-settings)
;;; test-casual-isearch-setttings.el ends here
