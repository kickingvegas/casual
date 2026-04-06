;;; test-casual-ispell.el --- Casual Make Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'casual-ispell-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-ispell)

(ert-deftest test-casual-ispell-tmenu ()
  (let ()
    (casualt-ispell-setup)

    (cl-letf ((casualt-mock #'ispell-word)
              (casualt-mock #'ispell-region)
              (casualt-mock #'ispell-buffer)
              (casualt-mock #'ispell-comment-or-string-at-point)
              (casualt-mock #'ispell-comments-and-strings)
              (casualt-mock #'ispell-complete-word)
              (casualt-mock #'ispell-complete-word-interior-frag)
              (casualt-mock #'ispell-kill-ispell)
              (casualt-mock #'casual-ispell-settings-tmenu)
              (casualt-mock #'casual-ispell-info)
              (casualt-mock #'ispell-change-dictionary)
              (casualt-mock #'transient-quit-all))

      (let ((test-vectors
             '((:binding "w" :command ispell-word)
               ;; (:binding "r" :command ispell-region)
               (:binding "b" :command ispell-buffer)
               ;; (:binding "s" :command ispell-comment-or-string-at-point)
               ;; (:binding "c" :command ispell-comments-and-strings)
               (:binding "TAB" :command ispell-complete-word)
               (:binding "SPC" :command ispell-complete-word-interior-frag)
               (:binding "x" :command ispell-kill-ispell)
               (:binding "," :command casual-ispell-settings-tmenu)
               (:binding "I" :command casual-ispell-info)
               (:binding "D" :command ispell-change-dictionary)
               (:binding "RET" :command transient-quit-all))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-ispell-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-ispell-breakdown)))

(provide 'test-casual-ispell)
;;; test-casual-ispell.el ends here
