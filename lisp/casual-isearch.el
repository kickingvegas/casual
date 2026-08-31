;;; casual-isearch.el --- Transient UI for I-Search -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024-2026  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: text

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

;; Casual I-Search is a Transient user interface for the I-Search library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual I-Search by running the hook
;; function `casual-isearch-init'.

;; Ensure that `casual-isearch-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) I-Search Install' for more detail on
;; installation.

;;; Code:
(require 'casual-lib)
(require 'casual-isearch-utils)
(require 'casual-isearch-settings)

;;;###autoload (autoload 'casual-isearch-init "casual-isearch" nil t)
(defun casual-isearch-init ()
  "Initialize and configure Casual I-Search.

This hook binds `casual-isearch-tmenu' to `casual-keybinding-primary'.

If `casual-isearch-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-isearch-setup' will be set."
  (add-hook 'isearch-mode-hook #'casual-isearch-setup))

(defun casual-isearch-setup ()
  "Setup `isearch-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set isearch-mode-map casual-keybinding-primary #'casual-isearch-tmenu))


;;;###autoload (autoload 'casual-isearch-tmenu "casual-isearch" nil t)
(transient-define-prefix casual-isearch-tmenu ()
  "Transient menu for I-Search."
  :refresh-suffixes t
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
    :if-nil buffer-read-only
    ("r" "Start ‘query-replace’" isearch-query-replace)
    ("x" "Start ‘query-replace-regexp’" isearch-query-replace-regexp)]]

  [["Toggle"
    ("X" "Regexp searching (edit)"
     isearch-toggle-regexp
     :description (lambda () (casual-lib-checkbox-label isearch-regexp
                                                        "Regexp search")))

    ("S" "Symbol searching (edit)"
     isearch-toggle-symbol
     :description (lambda () (casual-lib-checkbox-label
                              (eq isearch-regexp-function #'isearch-symbol-regexp)
                              "Symbol search")))

    ("W" "Word searching (edit)"
     isearch-toggle-word
     :description (lambda () (casual-lib-checkbox-label
                              (eq isearch-regexp-function #'word-search-regexp)
                              "Word search")))

    ("F" "Case fold"
     isearch-toggle-case-fold
     :description (lambda () (casual-lib-checkbox-label
                              isearch-case-fold-search
                              "Case insensitive")))
    ("L" "Lax whitespace"
     isearch-toggle-lax-whitespace
     :description (lambda () (casual-lib-checkbox-label
                              (if isearch-regexp
                                  isearch-regexp-lax-whitespace
                                isearch-lax-whitespace)
                              "Lax whitespace")))]

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
   ("," "Settings›" casual-isearch-settings-tmenu)
   ("RET" "Done" isearch-exit)
   (casual-lib-quit-all)])

(provide 'casual-isearch)
;;; casual-isearch.el ends here
