;;; test-casual-info.el --- Casual Info Tests      -*- lexical-binding: t; -*-

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
(require 'casual-info-test-utils)
(require 'casual-info)

(ert-deftest test-casual-info-tmenu ()
  (let ((tmpfile "casual-info-tmenu.txt"))
    (casualt-info-setup)
    (cl-letf (
              (casualt-mock #'Info-directory)
              (casualt-mock #'Info-top-node)
              (casualt-mock #'Info-toc)
              (casualt-mock #'Info-menu)
              (casualt-mock #'Info-goto-node)
              (casualt-mock #'Info-index)
              (casualt-mock #'Info-virtual-index)
              (casualt-mock #'Info-history)
              (casualt-mock #'Info-history-back)
              (casualt-mock #'Info-history-forward)
              (casualt-mock #'Info-search)
              (casualt-mock #'isearch-forward)
              (casualt-mock #'Info-search-case-sensitively)
              (casualt-mock #'Info-scroll-up)
              (casualt-mock #'Info-scroll-down)
              (casualt-mock #'Info-prev-reference)
              (casualt-mock #'Info-next-reference)
              (casualt-mock #'Info-backward-node)
              (casualt-mock #'Info-forward-node)
              (casualt-mock #'Info-prev)
              (casualt-mock #'Info-next)
              (casualt-mock #'Info-top-node)
              (casualt-mock #'Info-final-node)
              (casualt-mock #'Info-follow-nearest-node)
              (casualt-mock #'Info-up)
              (casualt-mock #'Info-copy-current-node-name)
              (casualt-mock #'Info-goto-node-web)
              (casualt-mock #'clone-buffer)
              (casualt-mock #'bookmark-jump)
              (casualt-mock #'bookmark-set)
              (casualt-mock #'ibuffer)
              (casualt-mock #'quit-window)
              (casualt-mock #'info-apropos))

      (let ((test-vectors
             '(;; Overview
               (:binding "d" :command Info-directory)
               (:binding "t" :command Info-top-node)
               (:binding "T" :command Info-toc)

               ;; Goto
               (:binding "m" :command Info-menu)
               ;;(:binding "g" :command Info-goto-node)
               (:binding "i" :command Info-index)
               (:binding "I" :command Info-virtual-index)

               ;; Search
               (:binding "C-s" :command isearch-forward)
               (:binding "s" :command Info-search)
               (:binding "S" :command Info-search-case-sensitively)
               (:binding "a" :command info-apropos)

               ;; History
               (:binding "L" :command Info-history)
               (:binding "M-[" :command Info-history-back)
               (:binding "M-]" :command Info-history-forward)

               ;; Scroll
               ;;(:binding "S-SPC" :command Info-scroll-down)
               (:binding "SPC" :command Info-scroll-up)
               (:binding "DEL" :command Info-scroll-down)

               ;; Navigation
               (:binding "k" :command Info-prev-reference)
               (:binding "j" :command Info-next-reference)
               (:binding "p" :command casual-info-browse-backward-paragraph)
               (:binding "n" :command casual-info-browse-forward-paragraph)
               (:binding "[" :command Info-backward-node)
               (:binding "]" :command Info-forward-node)
               (:binding "h" :command Info-prev)
               (:binding "l" :command Info-next)
               (:binding "<" :command Info-top-node)
               (:binding ">" :command Info-final-node)
               (:binding "RET" :command Info-follow-nearest-node)
               (:binding "^" :command Info-up)

               ;; Quick
               (:binding "c" :command Info-copy-current-node-name)
               (:binding "M-n" :command clone-buffer)
               (:binding "C-M-n" :command casual-info-new-info-frame)
               (:binding "G" :command Info-goto-node-web)

               ;; Menu Navigation
               (:binding "," :command casual-info-settings-tmenu)
               (:binding "q" :command quit-window))))

        (info "Emacs")
        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-info-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-info-breakdown t)))

(provide 'test-casual-info)
;;; test-casual-info.el ends here
