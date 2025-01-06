;;; test-casual-editkit.el --- Casual IBuffer Tests -*- lexical-binding: t; -*-

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
(require 'casual-editkit-test-utils)
(require 'casual-editkit)

(ert-deftest test-casual-editkit-tmenu ()
  (let ((tmpfile "casual-editkit-main-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf (((symbol-function #'casual-editkit-version-controlled-p) (lambda () t))
              ((symbol-function #'casual-editkit-package-symbol-overlay-installed-p) (lambda () t))
              ((symbol-function #'casual-editkit-package-magit-installed-p) (lambda () t))
              (casualt-mock #'find-file)
              (casualt-mock #'dired-jump-other-window)
              (casualt-mock #'ibuffer)
              (casualt-mock #'recentf-open-files)
              (casualt-mock #'revert-buffer)
              (casualt-mock #'save-buffer)

              (casualt-mock #'insert-char)
              (casualt-mock #'fill-paragraph)
              (casualt-mock #'newline)
              (casualt-mock #'join-line)
              (casualt-mock #'mark-sexp)
              (casualt-mock #'kill-sexp)
              (casualt-mock #'transpose-sexps)

              (casualt-mock #'org-agenda)
              (casualt-mock #'compile)
              (casualt-mock #'quick-calc)
              (casualt-mock #'shell-command)

              (casualt-mock #'make-frame-command)
              (casualt-mock #'undo)
              (casualt-mock #'save-buffers-kill-emacs))

      (let ((test-vectors
             '((:binding "o" :command casual-editkit-open-tmenu)
               (:binding "f" :command find-file)
               (:binding "d" :command dired-jump-other-window)
               (:binding "b" :command ibuffer)
               (:binding "R" :command recentf-open-files)
               (:binding "v" :command revert-buffer)
               (:binding "s" :command save-buffer)

               (:binding "e" :command casual-editkit-edit-tmenu)
               (:binding "p" :command fill-paragraph)
               (:binding "l" :command join-line)
               (:binding "C-o" :command open-line)
               (:binding "E" :command casual-editkit-emoji-symbols-tmenu)

               (:binding "m" :command mark-sexp)
               (:binding "c" :command casual-editkit-copy-sexp)
               (:binding "k" :command kill-sexp)
               (:binding "t" :command transpose-sexps)

               (:binding "T" :command casual-editkit-tools-tmenu)
               (:binding "a" :command org-agenda)
               (:binding "C" :command compile)
               (:binding "*" :command quick-calc)
               (:binding "!" :command shell-command)
               (:binding "g" :command casual-editkit-select-magit-command)
               (:binding "h" :command casual-editkit-symbol-overlay-put)

               (:binding "B" :command casual-editkit-bookmarks-tmenu)
               (:binding "J" :command bookmark-jump)

               (:binding "w" :command casual-editkit-windows-tmenu)
               (:binding "M-n" :command make-frame-command)

               (:binding "P" :command casual-editkit-project-tmenu)
               (:binding "/" :command casual-editkit-search-tmenu)

               (:binding "r" :command casual-editkit-registers-tmenu)
               (:binding "U" :command undo)
               (:binding "," :command casual-editkit-settings-tmenu)
               (:binding "x" :command save-buffers-kill-emacs))))


        (insert "hello")
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-main-tmenu
                                        '(lambda () (random 5000)))
        (save-buffer)))

    (casualt-editkit-breakdown tmpfile)))

(provide 'test-casual-editkit)
;;; test-casual-editkit.el ends here
