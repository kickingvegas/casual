;;; test-casual-org-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-org-test-utils)
(require 'casual-org-settings)

(ert-deftest test-casual-org-settings-tmenu ()
  (cl-letf ((casualt-mock #'casual-org-settings-heading-tmenu)
            (casualt-mock #'casual-org-settings-files-tmenu)
            (casualt-mock #'casual-org-settings-display-tmenu)
            (casualt-mock #'casual-org-settings-keyboard-tmenu)
            (casualt-mock #'casual-org-settings-clock-tmenu)
            (casualt-mock #'casual-org-settings-killyank-tmenu)
            (casualt-mock #'casual-org--customize-group)
            (casualt-mock #'casual-org-about))

    (let ((test-vectors
           '((:binding "h" :command casual-org-settings-heading-tmenu)
             (:binding "f" :command casual-org-settings-files-tmenu)
             (:binding "d" :command casual-org-settings-display-tmenu)
             (:binding "k" :command casual-org-settings-keyboard-tmenu)
             (:binding "c" :command casual-org-settings-clock-tmenu)
             (:binding "y" :command casual-org-settings-killyank-tmenu)
             (:binding "G" :command casual-org--customize-group)
             (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
             (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
             (:binding "a" :command casual-org-about))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-casual-org-settings-heading-tmenu ()
  (cl-letf (

            (casualt-mock #'casual-org--customize-todo-keywords)
            (casualt-mock #'casual-org--customize-log-done)
            (casualt-mock #'casual-org--customize-group-priorities)
            (casualt-mock #'casual-org--customize-imenu-depth)
            (casualt-mock #'casual-org--customize-insert-heading-respect-content)
            (casualt-mock #'casual-org--customize-group-goto)

            )

    (let ((test-vectors
           '(
            (:binding "k" :command casual-org--customize-todo-keywords)
            (:binding "l" :command casual-org--customize-log-done)
            (:binding "p" :command casual-org--customize-group-priorities)
            (:binding "i" :command casual-org--customize-imenu-depth)
            (:binding "r" :command casual-org--customize-insert-heading-respect-content)
            (:binding "g" :command casual-org--customize-group-goto)

             )))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-heading-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-casual-org-settings-files-tmenu ()
  (cl-letf ((casualt-mock #'casual-org--customize-directory)
            (casualt-mock #'casual-org--customize-default-notes-file)
            (casualt-mock #'casual-org--customize-refile-targets))

    (let ((test-vectors
           '((:binding "o" :command casual-org--customize-directory)
             (:binding "n" :command casual-org--customize-default-notes-file)
             (:binding "r" :command casual-org--customize-refile-targets))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-files-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-casual-org-settings-display-tmenu ()
  (cl-letf ((casualt-mock #'casual-org--customize-hide-leading-stars)
            (casualt-mock #'casual-org--customize-hide-emphasis-markers)
            (casualt-mock #'casual-org--customize-startup-folded)
            (casualt-mock #'casual-org--customize-startup-indented))

    (let ((test-vectors
           '((:binding "s" :command casual-org--customize-hide-leading-stars)
             (:binding "e" :command casual-org--customize-hide-emphasis-markers)
             (:binding "f" :command casual-org--customize-startup-folded)
             (:binding "i" :command casual-org--customize-startup-indented))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-display-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-casual-org-settings-keyboard-tmenu ()
  (cl-letf ((casualt-mock #'casual-org--customize-support-shift-select)
            (casualt-mock #'casual-org--customize-use-speed-commands))

    (let ((test-vectors
           '((:binding "s" :command casual-org--customize-support-shift-select)
             (:binding "S" :command casual-org--customize-use-speed-commands))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-keyboard-tmenu
                                      '(lambda () (random 5000))))))

(ert-deftest test-casual-org-settings-clock-tmenu ()
  (cl-letf ((casualt-mock #'casual-org--customize-show-notification-handler))

    (let ((test-vectors
           '((:binding "n"
              :command casual-org--customize-show-notification-handler))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-clock-tmenu
                                      '(lambda () (random 5000))))))

(ert-deftest test-casual-org-settings-killyank-tmenu ()
  (cl-letf ((casualt-mock #'casual-org--customize-yank-image-save-method))

    (let ((test-vectors
           '((:binding "y"
              :command casual-org--customize-yank-image-save-method))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-settings-killyank-tmenu
                                      '(lambda () (random 5000))))))

(ert-deftest test-casual-org-about ()
  (should (stringp (casual-org-about))))

(provide 'test-casual-org-settings)
;;; test-casual-org-setttings.el ends here
