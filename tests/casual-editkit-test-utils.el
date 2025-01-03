;;; casual-editkit-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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
(require 'casual-lib)
(require 'casual-lib-test-utils)
(require 'kmacro)

(defun casualt-editkit-setup (&optional filename)
  "Casual editkit test with FILENAME.

- FILENAME file name to be stored in /tmp."
  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (with-temp-file temp-filename
        (insert "hello"))
      (find-file temp-filename))))

(defun casualt-editkit-breakdown (&optional filename)
  "Casual menu test breakdown function with FILENAME.

- FILENAME filename that is stored in /tmp"

  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (kill-buffer)
      (delete-file temp-filename))))


(defun casualt-mock-active-region ()
  "Mock an active region to test `use-region-p'."

  (let ((p1 (line-beginning-position))
        (p2 (line-end-position)))
    (transient-mark-mode t)
    (insert "hey there")
    (goto-char p1)
    (push-mark p1)
    (goto-char p2)
    (setq mark-active t)))

(provide 'casual-editkit-test-utils)
;;; casual-editkit-test-utils.el ends here
