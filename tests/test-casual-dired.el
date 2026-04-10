;;; test-casual-dired.el --- Casual Dired Tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

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
(require 'casual-dired-test-utils)
(require 'casual-dired)

(ert-deftest test-casual-dired-tmenu ()
  (let ((tmpdir "/usr/share"))
    (casualt-dired-setup tmpdir)
    (dired-goto-file "/usr/share/man")
    (dired-insert-subdir "/usr/share/man")
    (call-interactively #'dired-mark)
    (next-line)

    (cl-letf (;; (symbol-function #'display-graphic-p (lambda (&optional display) t))
              (casualt-mock #'dired-find-file-other-window)
              (casualt-mock #'dired-view-file)
              (casualt-mock #'dired-do-copy)
              (casualt-mock #'dired-do-rename)
              (casualt-mock #'dired-do-delete)
              (casualt-mock #'casual-dired-link-tmenu)
              (casualt-mock #'casual-dired-change-tmenu)
              (casualt-mock #'dired-show-file-type)
              (casualt-mock #'dired-copy-filename-as-kill)
              (casualt-mock #'dired-do-shell-command)
              (casualt-mock #'dired-do-async-shell-command)
              ;; (casualt-mock #'image-dired-dired-toggle-marked-thumbs)
              (casualt-mock #'browse-url-of-dired-file)
              (casualt-mock #'casual-dired-sort-by-tmenu)
              (casualt-mock #'dired-hide-details-mode)
              (casualt-mock #'dired-omit-mode)
              (casualt-mock #'dired-maybe-insert-subdir)
              (casualt-mock #'dired-hide-subdir)
              (casualt-mock #'dired-do-kill-lines)
              (casualt-mock #'revert-buffer)
              (casualt-mock #'casual-dired-find-dired-regexp)
              (casualt-mock #'wdired-change-to-wdired-mode)
              ;; (casualt-mock #'image-dired)
              (casualt-mock #'dired-mark)
              (casualt-mock #'dired-unmark-all-marks)
              (casualt-mock #'dired-toggle-marks)
              (casualt-mock #'dired-flag-backup-files)
              (casualt-mock #'dired-do-flagged-delete)
              (casualt-mock #'casual-dired-regexp-tmenu)
              (casualt-mock #'dired-up-directory)
              (casualt-mock #'dired-previous-line)
              (casualt-mock #'dired-next-line)
              (casualt-mock #'dired-prev-dirline)
              (casualt-mock #'dired-next-dirline)
              (casualt-mock #'dired-prev-subdir)
              (casualt-mock #'dired-next-subdir)
              (casualt-mock #'dired-goto-file)
              (casualt-mock #'dired-goto-subdir)
              (casualt-mock #'bookmark-jump)
              (casualt-mock #'bookmark-set-no-overwrite)
              (casualt-mock #'ibuffer)
              (casualt-mock #'dired-isearch-filenames)
              (casualt-mock #'dired-isearch-filenames-regexp)
              (casualt-mock #'casual-dired-search-replace-tmenu)
              (casualt-mock #'dired-create-directory)
              (casualt-mock #'dired-create-empty-file)
              (casualt-mock #'rgrep))

      (let ((test-vectors
             '((:binding "o" :command dired-find-file-other-window)
               (:binding "v" :command dired-view-file)
               (:binding "C" :command dired-do-copy)
               (:binding "R" :command dired-do-rename)
               (:binding "D" :command dired-do-delete)
               (:binding "l" :command casual-dired-link-tmenu)
               (:binding "c" :command casual-dired-change-tmenu)
               (:binding "y" :command dired-show-file-type)
               (:binding "w" :command dired-copy-filename-as-kill)
               (:binding "!" :command dired-do-shell-command)
               (:binding "&" :command dired-do-async-shell-command)
               ;; (:binding ";" :command image-dired-dired-toggle-marked-thumbs)
               (:binding "W" :command browse-url-of-dired-file)
               (:binding "s" :command casual-dired-sort-by-tmenu)
               (:binding "h" :command dired-hide-details-mode)
               (:binding "O" :command dired-omit-mode)
               (:binding "i" :command dired-maybe-insert-subdir)
               (:binding "$" :command dired-hide-subdir)
               (:binding "k" :command dired-do-kill-lines)
               (:binding "g" :command revert-buffer)
               (:binding "f" :command casual-dired-find-dired-regexp)
               (:binding "E" :command wdired-change-to-wdired-mode)
               ;; (:binding "T" :command image-dired)
               (:binding "m" :command dired-mark)
               (:binding "U" :command dired-unmark-all-marks)
               (:binding "t" :command dired-toggle-marks)
               (:binding "~" :command dired-flag-backup-files)
               (:binding "x" :command dired-do-flagged-delete)
               (:binding "r" :command casual-dired-regexp-tmenu)
               (:binding "^" :command dired-up-directory)
               (:binding "p" :command dired-previous-line)
               (:binding "n" :command dired-next-line)
               (:binding "[" :command dired-prev-subdir)
               (:binding "]" :command dired-next-subdir)
               (:binding "M-p" :command dired-prev-dirline)
               (:binding "M-n" :command dired-next-dirline)
               (:binding "j" :command dired-goto-file)
               (:binding "M-j" :command dired-goto-subdir)
               (:binding "J" :command bookmark-jump)
               (:binding "B" :command bookmark-set-no-overwrite)
               (:binding "b" :command ibuffer)
               (:binding "C-s" :command dired-isearch-filenames)
               (:binding "M-s" :command dired-isearch-filenames-regexp)
               (:binding "/" :command casual-dired-search-replace-tmenu)
               (:binding "+" :command dired-create-directory)
               (:binding "F" :command dired-create-empty-file)
               (:binding "M-f" :command rgrep))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-dired-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-dired-breakdown)))

(ert-deftest test-casual-dired-tmenu-image ()
  (when (display-graphic-p)
    (let ((tmpdir "/usr/share"))
      (casualt-dired-setup tmpdir)
      (dired-goto-file "/usr/share/man")
      (dired-insert-subdir "/usr/share/man")

      (cl-letf ((casualt-mock #'image-dired-dired-toggle-marked-thumbs)
                (casualt-mock #'image-dired))

        (let ((test-vectors
               '((:binding ";" :command image-dired-dired-toggle-marked-thumbs)
                 (:binding "T" :command image-dired))))

          (casualt-suffix-testcase-runner test-vectors
                                          #'casual-dired-tmenu
                                          '(lambda () (random 5000)))))
      (casualt-dired-breakdown))))

(ert-deftest test-casual-dired-tmenu-unmark ()
  (let ((tmpdir "/usr/share"))
    (casualt-dired-setup tmpdir)
    (dired-goto-file "/usr/share/man")
    (dired-insert-subdir "/usr/share/man")
    (call-interactively #'dired-mark)
    (previous-line)

    (cl-letf ((casualt-mock #'dired-unmark))

      (let ((test-vectors
             '((:binding "u" :command dired-unmark))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-dired-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-dired-breakdown)))

(ert-deftest test-casual-dired-regexp-tmenu ()
  (let ((tmpdir "/usr/share"))
    (casualt-dired-setup tmpdir)
    (dired-goto-file "/usr/share/man")
    (dired-insert-subdir "/usr/share/man")
    (call-interactively #'dired-mark)
    (previous-line)

    (cl-letf ((casualt-mock #'dired-mark-files-regexp)
              (casualt-mock #'dired-mark-files-containing-regexp)
              (casualt-mock #'dired-do-find-marked-files)
              (casualt-mock #'dired-do-copy-regexp)
              (casualt-mock #'dired-do-rename-regexp)
              (casualt-mock #'dired-do-delete)
              (casualt-mock #'dired-flag-files-regexp)
              (casualt-mock #'dired-do-flagged-delete))

      (let ((test-vectors
             '((:binding "m" :command dired-mark-files-regexp)
               (:binding "c" :command dired-mark-files-containing-regexp)
               (:binding "F" :command dired-do-find-marked-files)
               (:binding "C" :command dired-do-copy-regexp)
               (:binding "D" :command dired-do-delete)
               (:binding "r" :command dired-do-rename-regexp)
               (:binding "d" :command dired-flag-files-regexp)
               (:binding "x" :command dired-do-flagged-delete))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-dired-regexp-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-dired-breakdown)))

(ert-deftest test-casual-dired-regexp-unmark ()
  (let ((tmpdir "/usr/share"))
    (casualt-dired-setup tmpdir)
    (dired-goto-file "/usr/share/man")
    (dired-insert-subdir "/usr/share/man")
    (call-interactively #'dired-mark)
    (previous-line)

    (cl-letf ((casualt-mock #'dired-unmark)
              (casualt-mock #'dired-unmark-all-marks))

      (let ((test-vectors
             '((:binding "u" :command dired-unmark)
               (:binding "U" :command dired-unmark-all-marks))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-dired-regexp-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-dired-breakdown)))


(ert-deftest test-casual-dired-change-tmenu ()
  (let ((tmpdir "/usr/share"))
    (casualt-dired-setup tmpdir)
    (dired-goto-file "/usr/share/man")
    (dired-insert-subdir "/usr/share/man")
    (next-line)

    (cl-letf ((casualt-mock #'dired-do-chmod)
              (casualt-mock #'dired-do-chgrp)
              (casualt-mock #'dired-do-chown)
              (casualt-mock #'dired-do-touch))

      (let ((test-vectors
             '((:binding "M" :command dired-do-chmod)
               (:binding "G" :command dired-do-chgrp)
               (:binding "O" :command dired-do-chown)
               (:binding "T" :command dired-do-touch))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-dired-change-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-dired-breakdown)))


(ert-deftest test-casual-dired-format-arrow ()
  (should (string-equal (casual-dired-format-arrow "hey" t) " hey"))
  (should (string-equal (casual-dired-format-arrow "hey" nil) "hey")))

(provide 'test-casual-dired)
;;; test-casual-dired.el ends here
