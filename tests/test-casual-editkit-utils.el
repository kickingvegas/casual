;;; test-casual-editkit-utils.el --- Casual IBuffer Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-editkit-utils)

(ert-deftest test-casual-editkit-open-tmenu ()
  (let ((tmpfile "casual-editkit-open-tmenu.txt"))
    (casualt-editkit-setup tmpfile)

    (cl-letf (;; ((symbol-function #'buffer-file-name) (lambda () t))
              (casualt-mock #'find-file)
              (casualt-mock #'rename-visited-file)
              (casualt-mock #'find-file-other-window)
              (casualt-mock #'find-file-other-frame)
              (casualt-mock #'find-alternate-file)
              (casualt-mock #'insert-file)
              (casualt-mock #'kill-buffer))

      (let ((test-vectors
             '((:binding "f" :command find-file)
               (:binding "r" :command rename-visited-file)
               (:binding "R" :command rename-buffer)
               (:binding "M-r" :command rename-uniquely)
               (:binding "F" :command find-file-other-window)
               (:binding "M-n" :command find-file-other-frame)
               (:binding "a" :command find-alternate-file)
               (:binding "i" :command insert-file)
               (:binding "c" :command kill-buffer))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-open-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))


(ert-deftest test-casual-editkit-project-tmenu ()
  (let ((tmpfile "casual-editkit-project-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (casualt-mock #'project-find-file)
              (casualt-mock #'project-dired)
              (casualt-mock #'project-find-dir)
              (casualt-mock #'project-vc-dir)
              (casualt-mock #'project-find-regexp)
              (casualt-mock #'project-query-replace-regexp)
              (casualt-mock #'project-search)
              (casualt-mock #'fileloop-continue)
              (casualt-mock #'project-compile)
              (casualt-mock #'project-shell-command)
              (casualt-mock #'project-async-shell-command)
              (casualt-mock #'project-eshell)
              (casualt-mock #'project-shell)
              (casualt-mock #'project-switch-project)
              (casualt-mock #'project-list-buffers)
              (casualt-mock #'project-kill-buffers)
              (casualt-mock #'project-forget-project)
              (casualt-mock #'project-switch-to-buffer))

      (let ((test-vectors
             '((:binding "f" :command project-find-file)
               (:binding "BC-g" :command project-switch-to-buffer)
               (:binding "d" :command project-dired)
               (:binding "D" :command project-find-dir)
               (:binding "v" :command project-vc-dir)
               (:binding "r" :command project-find-regexp)
               (:binding "q" :command project-query-replace-regexp)
               (:binding "S" :command project-search)
               (:binding "n" :command fileloop-continue)
               (:binding "c" :command project-compile)
               (:binding "!" :command project-shell-command)
               (:binding "&" :command project-async-shell-command)
               (:binding "e" :command project-eshell)
               (:binding "M-s" :command project-shell)
               ;; (:binding "s" :command project-switch-project)
               (:binding "b" :command project-list-buffers)
               (:binding "k" :command project-kill-buffers)
               (:binding "FC-g" :command project-forget-project))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-project-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-edit-tmenu ()
  (let ((tmpfile "casual-editkit-edit-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'duplicate-dwim)
              (casualt-mock #'flush-lines)
              (casualt-mock #'keep-lines)
              (casualt-mock #'fill-paragraph)
              (casualt-mock #'yank)
              (casualt-mock #'align-regexp))

      (let ((test-vectors
             '((:binding "m" :command casual-editkit-mark-tmenu)
               (:binding "c" :command casual-editkit-copy-tmenu)
               (:binding "k" :command casual-editkit-kill-tmenu)
               (:binding "y" :command yank)
               (:binding "t" :command casual-editkit-transpose-tmenu)
               (:binding "T" :command casual-editkit-transform-text-tmenu)
               (:binding "v" :command casual-editkit-move-text-tmenu)
               (:binding "d" :command casual-editkit-delete-tmenu)
               (:binding "s" :command casual-editkit-sort-tmenu)
               (:binding "D" :command duplicate-dwim)
               (:binding "F" :command flush-lines)
               (:binding "K" :command keep-lines)
               (:binding "f" :command fill-paragraph)
               (:binding "a" :command align-regexp)
               (:binding "R" :command casual-editkit-rectangle-tmenu)
               (:binding "r" :command casual-editkit-reformat-tmenu))))
        (casualt-mock-active-region)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-edit-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-emoji-symbols-tmenu ()
  (let ((tmpfile "casual-editkit-emoji-symbols-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (emacs-lisp-mode)
    (cl-letf ()
      (let ((test-vectors
             '((:binding "e" :command emoji-search)
               (:binding "l" :command emoji-list)
               (:binding "d" :command emoji-describe)
               (:binding "+" :command emoji-zoom-increase)
               (:binding "-" :command emoji-zoom-decrease)
               (:binding "0" :command emoji-zoom-reset)
               (:binding "'" :command casual-editkit-smart-single-quote-dwim)
               (:binding "\"" :command casual-editkit-smart-double-quote-dwim)
               (:binding "_" :command casual-editkit-smart-low-quote-dwim)
               (:binding "c" :command casual-editkit-smart-comillas-quote-dwim)
               ;;(:binding "i" :command insert-char)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-emoji-symbols-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-mark-tmenu ()
  (let ((tmpfile "casual-editkit-mark-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (emacs-lisp-mode)
    (cl-letf ((casualt-mock #'mark-word)
              (casualt-mock #'mark-end-of-sentence)
              (casualt-mock #'mark-paragraph)
              (casualt-mock #'mark-sexp)
              (casualt-mock #'mark-defun))

      (let ((test-vectors
             '((:binding "w" :command mark-word)
               (:binding "s" :command mark-end-of-sentence)
               (:binding "p" :command mark-paragraph)
               (:binding "b" :command mark-sexp)
               (:binding "d" :command mark-defun))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-mark-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-sort-tmenu ()
  (let ((tmpfile "casual-editkit-sort-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'sort-lines)
              (casualt-mock #'sort-paragraphs)
              (casualt-mock #'sort-pages)
              (casualt-mock #'sort-fields)
              (casualt-mock #'sort-numeric-fields)
              (casualt-mock #'sort-regexp-fields)
              (casualt-mock #'sort-columns)
              (casualt-mock #'reverse-region))

      (let ((test-vectors
             '((:binding "l" :command sort-lines)
               (:binding "p" :command sort-paragraphs)
               (:binding "P" :command sort-pages)
               (:binding "f" :command sort-fields)
               (:binding "n" :command sort-numeric-fields)
               (:binding "r" :command sort-regexp-fields)
               (:binding "c" :command sort-columns)
               (:binding "-" :command reverse-region))))

        (casualt-mock-active-region)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-sort-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))


(ert-deftest test-casual-editkit-copy-tmenu ()
  (let ((tmpfile "casual-editkit-copy-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (emacs-lisp-mode)
    (cl-letf ((casualt-mock #'kill-ring-save)
              (casualt-mock #'copy-matching-lines))
      (let ((test-vectors
             '((:binding "w" :command casual-editkit-copy-word)
               (:binding "s" :command casual-editkit-copy-sentence)
               (:binding "p" :command casual-editkit-copy-paragraph)
               (:binding "b" :command casual-editkit-copy-sexp)
               (:binding "d" :command casual-editkit-copy-defun)
               (:binding "m" :command copy-matching-lines)
               (:binding "r" :command kill-ring-save))))

        (casualt-mock-active-region)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-copy-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-kill-tmenu ()
  (let ((tmpfile "casual-editkit-kill-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'kill-word)
              (casualt-mock #'kill-sentence)
              (casualt-mock #'kill-paragraph)
              (casualt-mock #'kill-line)
              (casualt-mock #'kill-matching-lines)
              (casualt-mock #'kill-sexp)
              (casualt-mock #'kill-region))

      (let ((test-vectors
             '((:binding "w" :command kill-word)
               (:binding "s" :command kill-sentence)
               (:binding "p" :command kill-paragraph)
               (:binding "l" :command kill-line)
               (:binding "b" :command kill-sexp)
               (:binding "m" :command kill-matching-lines)
               (:binding "r" :command kill-region))))

        (casualt-mock-active-region)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-kill-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-transpose-tmenu ()
  (let ((tmpfile "casual-editkit-transpose-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'transpose-chars)
              (casualt-mock #'transpose-words)
              (casualt-mock #'transpose-lines)
              (casualt-mock #'transpose-sentences)
              (casualt-mock #'transpose-paragraphs)
              (casualt-mock #'transpose-sexps)
              (casualt-mock #'transpose-regions))

      (let ((test-vectors
             '((:binding "c" :command transpose-chars)
               (:binding "w" :command transpose-words)
               (:binding "l" :command transpose-lines)
               (:binding "s" :command transpose-sentences)
               (:binding "p" :command transpose-paragraphs)
               (:binding "b" :command transpose-sexps)
               ;;(:binding "r" :command transpose-regions) ; TODO mock
               )))
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-transpose-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-delete-tmenu ()
  (let ((tmpfile "casual-editkit-delete-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'just-one-space)
              (casualt-mock #'join-line)
              (casualt-mock #'delete-horizontal-space)
              (casualt-mock #'delete-blank-lines)
              (casualt-mock #'whitespace-cleanup)
              (casualt-mock #'delete-trailing-whitespace)
              (casualt-mock #'zap-up-to-char)
              (casualt-mock #'zap-to-char))

      (let ((test-vectors
             '((:binding "o" :command just-one-space)
               (:binding "j" :command join-line)
               (:binding "h" :command delete-horizontal-space)
               (:binding "b" :command delete-blank-lines)
               (:binding "w" :command whitespace-cleanup)
               (:binding "d" :command delete-trailing-whitespace)
               (:binding "z" :command zap-up-to-char)
               (:binding "Z" :command zap-to-char))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-delete-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-move-text-tmenu ()
  (let ((tmpfile "casual-editkit-move-text-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ()
      (let ((test-vectors
             '((:binding "w" :command casual-editkit-move-word-tmenu)
               (:binding "s" :command casual-editkit-move-sentence-tmenu)
               (:binding "b" :command casual-editkit-move-sexp-tmenu))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-move-text-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-move-word-tmenu ()
  (let ((tmpfile "casual-editkit-move-word-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ()

      (let ((test-vectors
             '((:binding "b" :command casual-editkit-move-word-backward)
               (:binding "f" :command casual-editkit-move-word-forward)
               (:binding "RET" :command transient-quit-all))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-move-word-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-move-sentence-tmenu ()
  (let ((tmpfile "casual-editkit-move-sentence-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ()

      (let ((test-vectors
             '((:binding "b" :command casual-editkit-move-sentence-backward)
               (:binding "f" :command casual-editkit-move-sentence-forward)
               (:binding "RET" :command transient-quit-all))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-move-sentence-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-move-sexp-tmenu ()
  (let ((tmpfile "casual-editkit-move-sexp-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ()

      (let ((test-vectors
             '((:binding "b" :command casual-editkit-move-sexp-backward)
               (:binding "f" :command casual-editkit-move-sexp-forward)
               (:binding "RET" :command transient-quit-all))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-move-sexp-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

;; (ert-deftest test-casual-editkit-windows-tmenu ()
;;   (let ((tmpfile "casual-editkit-windows-tmenu.txt"))
;;     (casualt-editkit-setup tmpfile)
;;     (cl-letf ((casualt-mock #'other-window)
;;               (casualt-mock #'windmove-up)
;;               (casualt-mock #'windmove-down)
;;               (casualt-mock #'windmove-left)
;;               (casualt-mock #'windmove-right)

;;               (casualt-mock #'window-swap-states)
;;               (casualt-mock #'windmove-swap-states-up)
;;               (casualt-mock #'windmove-swap-states-down)
;;               (casualt-mock #'windmove-swap-states-left)
;;               (casualt-mock #'windmove-swap-states-right)

;;               (casualt-mock #'delete-other-windows)
;;               (casualt-mock #'split-window-below)
;;               (casualt-mock #'split-window-horizontally)

;;               (casualt-mock #'transpose-frame)

;;               (casualt-mock #'enlarge-window)
;;               (casualt-mock #'shrink-window)
;;               (casualt-mock #'enlarge-window-horizontally)
;;               (casualt-mock #'shrink-window-horizontally))

;;       (let ((test-vectors
;;              '((:binding "o" :command other-window)
;;                (:binding "p" :command windmove-up)
;;                (:binding "n" :command windmove-down)
;;                (:binding "b" :command windmove-left)
;;                (:binding "f" :command windmove-right)

;;                (:binding "s" :command window-swap-states)
;;                (:binding "M-p" :command windmove-swap-states-up)
;;                (:binding "M-n" :command windmove-swap-states-down)
;;                (:binding "M-b" :command windmove-swap-states-left)
;;                (:binding "M-f" :command windmove-swap-states-right)

;;                (:binding "1" :command delete-other-windows)
;;                (:binding "2" :command split-window-below)
;;                (:binding "3" :command split-window-horizontally)

;;                (:binding "t" :command transpose-frame)
;;                (:binding "d" :command casual-editkit-windows-delete-tmenu)

;;                (:binding "+" :command enlarge-window)
;;                (:binding "-" :command shrink-window)
;;                (:binding ">" :command enlarge-window-horizontally)
;;                (:binding "<" :command shrink-window-horizontally))))

;;         (split-window-below)
;;         (casualt-suffix-testcase-runner test-vectors
;;                                         #'casual-editkit-windows-tmenu
;;                                         '(lambda () (random 5000)))))
;;     (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-windows-delete-tmenu ()
  (let ((tmpfile "casual-editkit-windows-delete-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'windmove-delete-up)
              (casualt-mock #'windmove-delete-down)
              (casualt-mock #'windmove-delete-left)
              (casualt-mock #'windmove-delete-right))

      (let ((test-vectors
             '((:binding "p" :command windmove-delete-up)
               (:binding "n" :command windmove-delete-down)
               (:binding "b" :command windmove-delete-left)
               (:binding "f" :command windmove-delete-right))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-windows-delete-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-bookmarks-tmenu ()
  (let ((tmpfile "casual-editkit-bookmarks-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'bookmark-set-no-overwrite)
              (casualt-mock #'bookmark-jump))

      (let ((test-vectors
             '((:binding "e" :command casual-editkit-list-bookmarks-transient)
               (:binding "a" :command bookmark-set-no-overwrite)
               (:binding "J" :command bookmark-jump))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-bookmarks-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-search-tmenu ()
  (let ((tmpfile "casual-editkit-search-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'occur)
              (casualt-mock #'find-name-dired)
              (casualt-mock #'find-grep-dired)
              (casualt-mock #'rgrep))

      (let ((test-vectors
             '((:binding "s" :command casual-editkit--isearch)
               (:binding "S" :command casual-editkit--search)
               (:binding "r" :command casual-editkit--query-replace)
               (:binding "o" :command occur)
               (:binding "d" :command find-name-dired)
               (:binding "G" :command find-grep-dired)
               (:binding "g" :command rgrep))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-search-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

(ert-deftest test-casual-editkit-tools-tmenu ()
  (let ((tmpfile "casual-editkit-tools-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (casualt-mock #'shell)
              (casualt-mock #'shell-command)
              (casualt-mock #'async-shell-command)
              (casualt-mock #'eshell)
              (casualt-mock #'ielm)
              (casualt-mock #'term)
              (casualt-mock #'run-python)
              (casualt-mock #'calc)
              (casualt-mock #'weather)
              (casualt-mock #'re-builder)
              (casualt-mock #'count-words)
              (casualt-mock #'calendar)
              (casualt-mock #'world-clock)
              (casualt-mock #'sunrise-sunset)
              (casualt-mock #'erc)
              (casualt-mock #'eww)
              (casualt-mock #'tetris)
              (casualt-mock #'zone))

      (let ((test-vectors
             '((:binding "s" :command shell)
               (:binding "!" :command shell-command)
               (:binding "&" :command async-shell-command)
               (:binding "e" :command eshell)
               (:binding "i" :command ielm)
               (:binding "t" :command term)
               (:binding "p" :command run-python)
               (:binding "c" :command calc)
               (:binding "r" :command re-builder)
               (:binding "w" :command count-words)
               (:binding "a" :command calendar)
               (:binding "C" :command world-clock)
               (:binding "S" :command sunrise-sunset)
               (:binding "E" :command erc)
               (:binding "M-e" :command eww)
               (:binding "z" :command zone)
               (:binding "T" :command tetris))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-tools-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

;; (ert-deftest test-casual-editkit-registers-tmenu ()
;;   (let ((tmpfile "casual-editkit-registers-tmenu.txt"))
;;     (casualt-editkit-setup tmpfile)
;;     (cl-letf ((casualt-mock #'point-to-register)
;;               (casualt-mock #'window-configuration-to-register)
;;               (casualt-mock #'kmacro-to-register)
;;               (casualt-mock #'jump-to-register)
;;               (casualt-mock #'copy-to-register)
;;               (casualt-mock #'copy-rectangle-to-register)
;;               (casualt-mock #'append-to-register)
;;               (casualt-mock #'prepend-to-register)
;;               (casualt-mock #'insert-register))

;;       (let ((test-vectors
;;              '(
;;                (:binding "p" :command point-to-register)
;;                (:binding "w" :command window-configuration-to-register)
;;                (:binding "m" :command kmacro-to-register)
;;                (:binding "j" :command jump-to-register)
;;                (:binding "c" :command copy-to-register)
;;                (:binding "r" :command copy-rectangle-to-register)
;;                (:binding "a" :command append-to-register)
;;                (:binding "P" :command prepend-to-register)
;;                (:binding "i" :command insert-register))))

;;         (casualt-mock-active-region)
;;         (casualt-suffix-testcase-runner test-vectors
;;                                         #'casual-editkit-registers-tmenu
;;                                         '(lambda () (random 5000)))))
;;     (casualt-editkit-breakdown tmpfile)))


(ert-deftest test-casual-editkit-rectangle-tmenu ()
  (let ((tmpfile "casual-editkit-rectangle-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf (((symbol-function #'use-region-p) (lambda () t))
              (casualt-mock #'kill-rectangle)
              (casualt-mock #'copy-rectangle-as-kill)
              (casualt-mock #'yank-rectangle)
              (casualt-mock #'kill-rectangle)
              (casualt-mock #'copy-rectangle-as-kill)
              (casualt-mock #'yank-rectangle)
              (casualt-mock #'delete-rectangle)
              (casualt-mock #'string-rectangle)
              (casualt-mock #'string-insert-rectangle)
              (casualt-mock #'open-rectangle)
              (casualt-mock #'rectangle-mark-mode)
              (casualt-mock #'rectangle-number-lines)
              (casualt-mock #'clear-rectangle)
              (casualt-mock #'delete-whitespace-rectangle))

      (let ((test-vectors
             '((:binding "k" :command kill-rectangle)
               (:binding "c" :command copy-rectangle-as-kill)
               (:binding "y" :command yank-rectangle)
               (:binding "d" :command delete-rectangle)
               (:binding "s" :command string-rectangle)
               (:binding "i" :command string-insert-rectangle)
               (:binding "o" :command open-rectangle)
               (:binding "m" :command rectangle-mark-mode)
               (:binding "N" :command rectangle-number-lines)
               (:binding "C" :command clear-rectangle)
               (:binding "D" :command delete-whitespace-rectangle)
               (:binding "RET" :command transient-quit-all))))

        (set-mark-command nil)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-rectangle-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))


(ert-deftest test-casual-editkit-transform-text-tmenu ()
  (let ((tmpfile "casual-editkit-transform-text-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf ((casualt-mock #'capitalize-dwim)
              (casualt-mock #'upcase-initials-region)
              (casualt-mock #'downcase-dwim)
              (casualt-mock #'upcase-dwim))

      (let ((test-vectors
             '((:binding "c" :command capitalize-dwim)
               (:binding "t" :command upcase-initials-region)
               (:binding "l" :command downcase-dwim)
               (:binding "u" :command upcase-dwim)
               (:binding "RET" :command transient-quit-all))))

        (casualt-mock-active-region)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-transform-text-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))


(ert-deftest test-casual-editkit-reformat-tmenu ()
  (let ((tmpfile "casual-editkit-reformat-tmenu.txt"))
    (casualt-editkit-setup tmpfile)
    (cl-letf (((symbol-function #'use-region-p) (lambda () t))
              (casualt-mock #'fill-paragraph)
              (casualt-mock #'fill-region)
              (casualt-mock #'fill-region-as-paragraph)
              (casualt-mock #'fill-individual-paragraphs)
              (casualt-mock #'fill-nonuniform-paragraphs)
              (casualt-mock #'center-line)
              (casualt-mock #'center-region)
              (casualt-mock #'center-paragraph)
              (casualt-mock #'repunctuate-sentences)
              (casualt-mock #'untabify)
              (casualt-mock #'auto-fill-mode)
              (casualt-mock #'set-fill-column))

      (let ((test-vectors
             '((:binding "p" :command fill-paragraph)
               (:binding "r" :command fill-region)
               (:binding "P" :command fill-region-as-paragraph)
               (:binding "M-p" :command fill-individual-paragraphs)
               (:binding "n" :command fill-nonuniform-paragraphs)
               (:binding "C-l" :command center-line)
               (:binding "C-r" :command center-region)
               (:binding "C-p" :command center-paragraph)
               (:binding "R" :command repunctuate-sentences)
               (:binding "u" :command untabify)
               (:binding "a" :command auto-fill-mode)
               (:binding "d" :command casual-editkit--customize-sentence-end-double-space)
               (:binding "C" :command set-fill-column)

               )))
        (set-mark-command nil)
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-editkit-reformat-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-editkit-breakdown tmpfile)))

;; -------
(provide 'test-casual-editkit-utils)
;;; test-casual-editkit-utils.el ends here
