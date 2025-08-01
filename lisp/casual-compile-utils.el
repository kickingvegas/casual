;;; casual-compile-utils.el --- Casual Compile Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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
(require 'simple)
(require 'grep)
(require 'compile)
(require 'casual-lib)

(defconst casual-compile-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:goto . '("🚀" "Goto…"))
    (:refresh . '("⟳" "Refresh"))
    (:kill . '("×" "Kill"))
    (:display . '("👀" "Display")))

  "Unicode symbol DB to use for Compile Transient menus.")

(defun casual-compile-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-compile-unicode-db))

(defun casual-compile--select-mode-label (clabel glabel &optional dlabel)
  "Depending on `major-mode', output CLABEL, GLABEL or DLABEL."
  (let ((dlabel (if dlabel dlabel clabel)))
    (cond
     ((eq major-mode 'grep-mode) glabel)
     ((eq major-mode 'compilation-mode) clabel)
     (t dlabel))))

(defun casual-compile--compilation-running-p ()
  "Predicate if compilation is running."
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	t
      nil)))

(provide 'casual-compile-utils)
;;; casual-compile-utils.el ends here
