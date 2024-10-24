;;; casual-isearch.el --- Transient UI for I-Search -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: wp

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

;; Provides a Transient menu interface to a subset of isearch functions.
;; Said functions are grouped as follows in the following sections:
;; - Edit Search String
;; - Replace
;; - Toggle
;; - Misc
;; - Navigation

;; INSTALLATION
;; Enter the code below into your init file to load and install
;; `casual-isearch-tmenu'. Tune the keybinding to your taste.

;; (require 'casual-isearch) ; optional if using autoloaded menu
;; (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'casual-lib)
(require 'casual-isearch-utils)
(require 'casual-isearch-settings)

(defun casual-isearch--toggle-regex-and-edit ()
  "Invoke `isearch-toggle-regexp' then `isearch-edit-string'."
  (interactive)
  (isearch-toggle-regexp)
  (isearch-edit-string))

(defun casual-isearch--toggle-symbol-and-edit ()
  "Invoke `isearch-toggle-symbol' then `isearch-edit-string'."
  (interactive)
  (isearch-toggle-symbol)
  (isearch-edit-string))

(defun casual-isearch--toggle-word-and-edit ()
  "Invoke `isearch-toggle-word' then `isearch-edit-string'."
  (interactive)
  (isearch-toggle-symbol)
  (isearch-edit-string))

(transient-define-prefix casual-isearch-tmenu ()
  "Transient menu for I-Search."
  [["Edit Search String"
    ("e" "Edit the search string (recursive)" isearch-edit-string
     :transient t)
    ("w" "Pull next word or character from buffer" isearch-yank-word-or-char
     :transient t)
    ("s" "Pull next symbol or character from buffer" isearch-yank-symbol-or-char
     :transient t)
    ("l" "Pull rest of line from buffer" isearch-yank-line :transient t)
    ("y" "Pull string from kill ring" isearch-yank-kill :transient t)
    ("t" "Pull thing from buffer" isearch-forward-thing-at-point)]

   ["Replace"
    ("r" "Start ‘query-replace’" isearch-query-replace
     :if-nil buffer-read-only)
    ("x" "Start ‘query-replace-regexp’" isearch-query-replace-regexp
     :if-nil buffer-read-only)]]

  [["Toggle"
    ("X" "Regexp searching (edit)"
     casual-isearch--toggle-regex-and-edit
     :transient t)
    ("S" "Symbol searching (edit)"
     casual-isearch--toggle-symbol-and-edit
     :transient t)
    ("W" "Word searching (edit)"
     casual-isearch--toggle-word-and-edit
     :transient t)
    ("F" "Case fold"
     isearch-toggle-case-fold
     :transient t)
    ("L" "Lax whitespace"
     isearch-toggle-lax-whitespace
     :transient t)]

   ["Misc"
    ("o" "Occur" isearch-occur)
    ("h" "Highlight" isearch-highlight-regexp)
    ("H" "Highlight lines" isearch-highlight-lines-matching-regexp)
    ("u" "Unhighlight" unhighlight-regexp)]

   ["Navigation"
    ("p" "Previous" isearch-repeat-backward
     :description (lambda () (casual-isearch-unicode-get :previous))
     :transient t)
    ("n" "Next" isearch-repeat-forward
     :description (lambda () (casual-isearch-unicode-get :next))
     :transient t)
    ("<" "First" isearch-beginning-of-buffer
     :description (lambda () (casual-isearch-unicode-get :first))
     :transient t)
    (">" "Last" isearch-end-of-buffer
     :description (lambda () (casual-isearch-unicode-get :last))
     :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Exit Search" isearch-exit)
   ("," "Settings›" casual-isearch-settings-tmenu)
   (casual-lib-quit-all)])

(provide 'casual-isearch)
;;; casual-isearch.el ends here
