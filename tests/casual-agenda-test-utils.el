;;; casual-agenda-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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

(defun casualt-agenda-setup ()
  "Casual menu test setup function."
  )

(defun casualt-agenda-breakdown (&optional clear dismiss)
  "Menu breakdown function with CLEAR, DISMISS.

If CLEAR is non-nil then clear state.
If DISMISS is non-nil then dismiss Transient menu."

  (if dismiss
      (execute-kbd-macro "")))

(provide 'casual-agenda-test-utils)
;;; casual-agenda-test-utils.el ends here
