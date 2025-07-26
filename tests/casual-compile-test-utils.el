;;; casual-compile-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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
(require 'kmacro)
(require 'compile)

(defun casualt-compile-setup (&optional filename)
  "Casual menu test setup function."
  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (with-temp-file temp-filename
        (insert "# Hello
Hello 1
Hello 2
Hello 3")
        (goto-char (point-min)))
      (find-file temp-filename)
      (compilation-mode t)
      (rgrep "Hello" "foo" "/tmp/")
      (switch-to-buffer "*grep*")
      )))

(defun casualt-compile-breakdown (&optional filename)
  "Casual menu test breakdown function, if CLEAR is non-nil then clear state."
  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (switch-to-buffer "*grep*")
      (kill-buffer)
      (switch-to-buffer filename)
      (kill-buffer)
      (delete-file temp-filename))))

(provide 'casual-compile-test-utils)
;;; casual-compile-test-utils.el ends here
