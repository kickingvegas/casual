;;; casual-eshell-utils.el --- Casual Eshell Utils -*- lexical-binding: t; -*-

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

(require 'eshell)
(require 'em-alias)
(require 'tramp)
(require 'casual-lib)

(defconst casual-eshell-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:forward . '("→" "Forward"))
    (:backward . '("←" "Backward"))
    (:repeat . '("⥅" "Repeat"))
    (:clear . '("⌫" "Clear")))

  "Unicode symbol DB to use for Eshell Transient menus.")

(defun casual-eshell-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-eshell-unicode-db))

(defun casual-eshell-info ()
  "Open Info for Eshell."
  (interactive) (info "(eshell) Top"))

(defun casual-eshell-info-builtins ()
  "Open Info for Eshell built-in commands."
  (interactive) (info "(eshell) List of Built-ins"))

(defun casual-eshell-info-aliases ()
  "Open Info for Eshell aliases."
  (interactive) (info "(eshell) Aliases"))

(defun casual-eshell-info-remote-access ()
  "Open Info for Eshell remote access."
  (interactive) (info "(eshell) Remote Access"))

(defun casual-eshell-info-control-flow ()
  "Open Info for Eshell control flow."
  (interactive) (info "(eshell) Control Flow"))

(defun casual-eshell-info-expansion ()
  "Open Info for Eshell expansion."
  (interactive) (info "(eshell) Expansion"))

(defun casual-eshell-info-dollars-expansion ()
  "Open Info for Eshell $ expansion."
  (interactive) (info "(eshell) Dollars Expansion"))

(defun casual-eshell-info-redirection ()
  "Open Info for Eshell redirection."
  (interactive) (info "(eshell) Redirection"))

(defun casual-eshell-info-pipelines ()
  "Open Info for Eshell pipelines."
  (interactive) (info "(eshell) Pipelines"))

(defun casual-eshell-edit-aliases ()
  "Edit file in `eshell-aliases-file'."
  (interactive)
  (find-file eshell-aliases-file))

(defun casual-eshell-tilde-path (path)
  "Parse PATH for display."
  (let ((test-ssh-path (string-search "/ssh:" path)))
    (if (and test-ssh-path (= test-ssh-path 0))
        (let* ((path-obj (tramp-dissect-file-name path))
               (host (tramp-file-name-host path-obj))
               (localname (tramp-file-name-localname path-obj)))
          (format "(%s) %s" host localname))

      (if (string= path (getenv "HOME"))
          "~"
        (replace-regexp-in-string
         (concat "^" (getenv "HOME")) "~" path)))))

(provide 'casual-eshell-utils)
;;; casual-eshell-utils.el ends here
