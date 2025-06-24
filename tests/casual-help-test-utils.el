;;; casual-help-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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

(defun casualt-help-setup ()
  "Casual Help setup."
  (describe-symbol 'find-file)
  (describe-symbol 'apropos-do-all)
  (switch-to-buffer "*Help*"))

(defun casualt-help-breakdown ()
  "Casual help breakdown."
  )

(provide 'casual-help-test-utils)
;;; casual-help-test-utils.el ends here
