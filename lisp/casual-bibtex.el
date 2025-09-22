;;; casual-bibtex.el --- Transient UI for BibTeX -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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

;; This library provides a Transient-based user interface for the `bibtex'
;; package.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-bibtex-tmenu' to your
;; key binding of preference.

;; (require 'casual-bibtex) ; optional if using autoloaded menu
;; (keymap-set bibtex-mode-map "M-m" #'casual-bibtex-tmenu)

;; It is highly recommended to modify the bindings in `bibtex-mode-map' to be
;; compatible with the bindings used in `casual-bibtex-tmenu':

;; (keymap-set bibtex-mode-map "<TAB>" #'bibtex-next-field)
;; (keymap-set bibtex-mode-map "<backtab>" #'previous-line)
;; (keymap-set bibtex-mode-map "C-n" #'bibtex-next-field)
;; (keymap-set bibtex-mode-map "M-n" #'bibtex-next-entry)
;; (keymap-set bibtex-mode-map "M-p" #'bibtex-previous-entry)
;; (keymap-set bibtex-mode-map "<prior>" #'bibtex-previous-entry)
;; (keymap-set bibtex-mode-map "<next>" #'bibtex-next-entry)
;; (keymap-set bibtex-mode-map "C-c C-o" #'bibtex-url)
;; (keymap-set bibtex-mode-map "C-a" #'casual-bibtex-beginning-of-field)
;; (keymap-set bibtex-mode-map "C-e" #'casual-bibtex-end-of-field)
;; (keymap-set bibtex-mode-map "<clear>" #'bibtex-empty-field)
;; (keymap-set bibtex-mode-map "M-<clear>" #'bibtex-kill-field)
;; (keymap-set bibtex-mode-map "M-DEL" #'bibtex-kill-field)

;;; Code:
(require 'bookmark)
(require 'replace)
(require 'casual-bibtex-settings)
(require 'casual-bibtex-utils)

;;;###autoload (autoload 'casual-bibtex-tmenu "casual-bibtex" nil t)
(transient-define-prefix casual-bibtex-tmenu ()
  "Transient menu for BibTeX."
  :refresh-suffixes t
  ["Casual BibTeX"
   :pad-keys t
   ["Field"
    ("a" "Add…" bibtex-make-field :transient nil)
    ("c" "Copy∙ k,v" bibtex-copy-field-as-kill :transient nil)]

   [""
    ("w" "Copy v" casual-bibtex-copy-field-value :transient nil)
    ("x" "Clear v" bibtex-empty-field :transient nil)]

   [""
    ("DEL" "Delete k,v" bibtex-kill-field :transient nil)
    ("o" "Remove OPT/ALT" bibtex-remove-OPT-or-ALT :transient nil)]

   [""
    ("C-a" "Begin" casual-bibtex-beginning-of-field
     :description (lambda () (casual-bibtex-unicode-get :begin-field))
     :transient t)
    ("C-e" "End" casual-bibtex-end-of-field
     :description (lambda () (casual-bibtex-unicode-get :end-field))
     :transient t)]

   [""
    ("p" "Previous" previous-line
     :description (lambda () (casual-bibtex-unicode-get :previous))
     :transient t)
    ("n" "Next" bibtex-next-field
     :description (lambda () (casual-bibtex-unicode-get :next))
     :transient t)]]

  ["Entry"
   :pad-keys t
   [("A" "Add…" bibtex-entry :transient nil)
    ("C" "Copy∙" bibtex-copy-entry-as-kill :transient nil)]

   [("k" "Kill∙" bibtex-kill-entry :transient nil)
    ("u" "Update" bibtex-entry-update :transient nil)]

   [("m" "Mark" bibtex-mark-entry :transient nil)]

   [("f" "Fill" bibtex-fill-entry :transient nil)
    ("C-c" "Clean" bibtex-clean-entry :transient nil)]

   [("<" "Begin Entry" bibtex-beginning-of-entry
     :description (lambda () (casual-bibtex-unicode-get :begin-entry))
     :transient t)
    (">" "End Entry" bibtex-end-of-entry
     :description (lambda () (casual-bibtex-unicode-get :end-entry))
     :transient t)]

   [("M-p" "Previous Entry" bibtex-previous-entry
     :description (lambda () (casual-bibtex-unicode-get :previous))
     :transient t)
    ("M-n" "Next Entry" bibtex-next-entry
     :description (lambda () (casual-bibtex-unicode-get :next))
     :transient t)]]

  ["Misc"
   :pad-keys t
   [("y" "Yank∙" bibtex-yank :transient t)
    ("M-y" "Yank-Pop∙" bibtex-yank-pop :transient t)]

   [("/" "Search…" bibtex-search-entries :transient t)
    ("j" "Jump…" bibtex-search-entry :transient t)]
   [("." "Xref…" bibtex-search-crossref :transient t)
    ("s" "Sort" bibtex-sort-buffer :transient t)]
   [("O" "Occur…" occur)
    ("N" "Narrow" bibtex-narrow-to-entry
     :if-not buffer-narrowed-p
     :transient nil)
    ("W" "Widen" widen
     :if buffer-narrowed-p
     :transient nil)]
   [("C-s" "Save" save-buffer :transient t)
    ("J" "Jump to Bookmark…" bookmark-jump)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("RET" "Edit" transient-quit-all)
          ("U" "Undo" undo :transient t)
          ("," "Settings›" casual-bibtex-settings-tmenu)
          (casual-lib-quit-all)])

(provide 'casual-bibtex)
;;; casual-bibtex.el ends here
