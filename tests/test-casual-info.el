;;; test-casual-info.el --- Casual Info Tests      -*- lexical-binding: t; -*-

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
(require 'casual-info-test-utils)
(require 'casual-info)

(ert-deftest test-casual-info-tmenu-bindings ()
  (casualt-info-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "d" #'Info-directory) test-vectors)
    (push (casualt-suffix-test-vector "t" #'Info-top-node) test-vectors)
    (push (casualt-suffix-test-vector "T" #'Info-toc) test-vectors)
    ;;(push (casualt-suffix-test-vector "m" #'Info-menu) test-vectors)
    ;;(push (casualt-suffix-test-vector "g" #'Info-goto-node) test-vectors)
    ;;(push (casualt-suffix-test-vector "i" #'Info-index) test-vectors)
    ;;(push (casualt-suffix-test-vector "I" #'Info-virtual-index) test-vectors)

    (push (casualt-suffix-test-vector "L" #'Info-history) test-vectors)
    (push (casualt-suffix-test-vector "Û" #'Info-history-back) test-vectors)
    (push (casualt-suffix-test-vector "Ý" #'Info-history-forward) test-vectors)

    (push (casualt-suffix-test-vector " " #'Info-scroll-up) test-vectors)
    (push (casualt-suffix-test-vector "" #'Info-scroll-down) test-vectors)

    (push (casualt-suffix-test-vector "k" #'Info-prev-reference) test-vectors)
    (push (casualt-suffix-test-vector "j" #'Info-next-reference) test-vectors)

    (push (casualt-suffix-test-vector "p" #'casual-info-browse-backward-paragraph) test-vectors)
    (push (casualt-suffix-test-vector "n" #'casual-info-browse-forward-paragraph) test-vectors)

    (push (casualt-suffix-test-vector "[" #'Info-backward-node) test-vectors)
    (push (casualt-suffix-test-vector "]" #'Info-forward-node) test-vectors)

    (push (casualt-suffix-test-vector "h" #'Info-prev) test-vectors)
    (push (casualt-suffix-test-vector "l" #'Info-next) test-vectors)

    (push (casualt-suffix-test-vector "<" #'Info-top-node) test-vectors)
    (push (casualt-suffix-test-vector ">" #'Info-final-node) test-vectors)

    (push (casualt-suffix-test-vector "^" #'Info-up) test-vectors)

    (push (casualt-suffix-test-vector "J" #'bookmark-jump) test-vectors)
    (push (casualt-suffix-test-vector "B" #'bookmark-set) test-vectors)
    (push (casualt-suffix-test-vector "b" #'ibuffer) test-vectors)
    (push (casualt-suffix-test-vector "c" #'Info-copy-current-node-name) test-vectors)
    ;;(push (casualt-suffix-test-vector "G" #'Info-goto-node-web) test-vectors)

    (push (casualt-suffix-test-vector "" #'casual-info-new-info-frame) test-vectors)
    (push (casualt-suffix-test-vector "î" #'clone-buffer) test-vectors)
    ;;(push (casualt-suffix-test-vector "" #'Info-follow-nearest-node) test-vectors)
    ;;(push (casualt-suffix-test-vector " " #'Info-scroll-up) test-vectors)

    (push (casualt-suffix-test-vector "," #'casual-info-settings-tmenu) test-vectors)
    (push (casualt-suffix-test-vector "q" #'quit-window) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-info-tmenu
                                     '(lambda () (random 5000))))
  (casualt-info-breakdown t))

(provide 'test-casual-info)
;;; test-casual-info.el ends here
