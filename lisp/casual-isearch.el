;;; casual-isearch.el --- Transient UI for I-Search -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024-2025  Charles Y. Choi

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

;;; Code:
(require 'casual-lib)
(require 'casual-isearch-utils)
(require 'casual-isearch-settings)

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
   ("RET" "Exit Search" isearch-exit)
   ("," "Settings›" casual-isearch-settings-tmenu)
   (casual-lib-quit-all)])

(provide 'casual-isearch)
;;; casual-isearch.el ends here
