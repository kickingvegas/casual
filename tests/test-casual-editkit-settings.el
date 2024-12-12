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
  (casualt-editkit-setup)
  (cl-letf ((casualt-mock #'auto-fill-mode)
            (casualt-mock #'indent-tabs-mode)
            (casualt-mock #'set-fill-column))
    (let ((test-vectors
           '((:binding "f" :command auto-fill-mode)
             (:binding "t" :command indent-tabs-mode)
             (:binding "C" :command set-fill-column)
             (:binding "." :command casual-editkit--customize-sentence-end-double-space)
             (:binding "p" :command casual-editkit--customize-save-place-mode)
             (:binding "h" :command casual-editkit--customize-savehist-mode)
             (:binding "S" :command casual-editkit--customize-delete-selection-mode)
             (:binding "N" :command casual-editkit--customize-require-final-newline)
             (:binding "v" :command casual-editkit--customize-view-read-only)
             (:binding "F" :command casual-editkit--customize-global-auto-revert-mode)
             (:binding "B" :command casual-editkit--customize-global-auto-revert-non-file-buffers)
             (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
             (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
             (:binding "a" :command casual-editkit-about))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-editkit-settings-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-editkit-breakdown))

(ert-deftest test-casual-editkit-about ()
  (should (stringp (casual-editkit-about))))

(provide 'test-casual-editkit-settings)
;;; test-casual-editkit-setttings.el ends here
