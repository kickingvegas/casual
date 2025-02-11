;;; casual-editkit-utils.el --- Casual Bookmarks Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools, wp

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
(require 'fileloop)
(require 'bookmark)
(require 'org-agenda)
(require 'recentf)
(require 'simple)
(require 'text-mode)
(require 'tabify)
(require 'casual-editkit-constants)
(require 'casual-editkit-settings)

;;; Predicates

(defun casual-editkit-version-controlled-p ()
  "Predicate if version controlled."
  (vc-responsible-backend default-directory t))

(defun casual-editkit-prog-mode-p ()
  "Predicate if current buffer is `prog-mode'."
  (derived-mode-p 'prog-mode))

(defun casual-editkit-org-mode-p ()
  "Predicate if current buffer is `org-mode'."
  (derived-mode-p 'org-mode))

(defun casual-editkit-org-agenda-all-todos ()
  "Invoke `org-agenda' to show all TODO items."
  (interactive)
  (org-agenda nil "n"))

(defun casual-editkit-window-system-mac-p ()
  "Predicate if window system is macOS."
  (eq window-system 'mac))

(defun casual-editkit-buffer-read-only-p ()
  "Predicate for `buffer-read-only'."
  (if buffer-read-only t nil))

(defun casual-editkit-package-symbol-overlay-installed-p ()
  "Predicate to test if package `symbol-overlay' is installed."
  (package-installed-p 'symbol-overlay))

(defun casual-editkit-package-magit-installed-p ()
  "Predicate to test if package `magit' is installed."
  (package-installed-p 'magit))

(defun casual-editkit-package-transpose-frame-installed-p ()
  "Predicate to test if package `transpose-frame' is installed."
  (package-installed-p 'transpose-frame))

(defun casual-editkit-transpose-frame ()
  "Dynamically dispatch command call to `transpose-frame'."
  (interactive)
  (if (fboundp 'transpose-frame)
      (call-interactively #'transpose-frame)  ; autoloaded
    (message "%s not installed. Unable to call %s"
             (symbol-name 'transpose-frame)
             (symbol-name 'transpose-frame))))

(defun casual-editkit-symbol-overlay-put ()
  "Dynamically dispatch command call to `symbol-overlay'."
  (interactive)
  (if (fboundp 'symbol-overlay-put)
      (call-interactively #'symbol-overlay-put)  ; autoloaded
    (message "%s not installed. Unable to call %s"
             (symbol-name 'symbol-overlay)
             (symbol-name 'symbol-overlay-put))))

(defun casual-editkit-select-magit-command ()
  "Dynamically dispatch appropriate Magit command call given context."
  (interactive)
  (if (casual-editkit-version-controlled-p)
      (cond
       ((derived-mode-p 'dired-mode)
        (if (fboundp 'magit-status)
            (funcall-interactively #'magit-status)))
       ((or (derived-mode-p 'prog-mode) (derived-mode-p 'text-mode))
        (if (fboundp 'magit-file-dispatch)
            (funcall-interactively #'magit-file-dispatch)))
       (t
        (if (fboundp 'magit-status)
            (funcall-interactively #'magit-status))))
    (message "Not a version controlled buffer.")))

(defun casual-editkit-select-magit-command-description ()
  "Select appropriate Magit command description given context."
  (if (casual-editkit-version-controlled-p)
      (cond
       ((derived-mode-p 'dired-mode) "Magit Status")
       ((or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
        "Magit Dispatch")
       (t "Magit Status"))
    (message "Not a version controlled buffer.")))

;;;###autoload (autoload 'casual-editkit-open-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-open-tmenu ()
  "Menu for ‘Open’ commands.

Commands pertaining to opening and visiting files can be accessed
from here. The Project menu `casual-editkit-project-tmenu' is
also available from here."
  ["Open"
   ["File"
    :pad-keys t
    ("f" "File…" find-file)
    ("F" "Find other…" find-file-other-window)
    ("M-n" "Find in frame…" find-file-other-frame)
    ("a" "Find alternate…" find-alternate-file)
    ("i" "Insert…" insert-file)
    ("c" "Close" kill-buffer)]

   ["Visit"
    :pad-keys t
    ("v" "Visit…" find-file-read-only)
    ("V" "Visit other…" find-file-read-only-other-window)
    ("M-v" "Visit in frame…" find-file-read-only-other-frame)]

   ["Rename"
    :pad-keys t
    ("r" "File…" rename-visited-file
     :inapt-if-not buffer-file-name)
    ("R" "Buffer…" rename-buffer)
    ("M-r" "Buffer Uniquely" rename-uniquely)]

   ["Project"
    ("p" "Project›" casual-editkit-project-tmenu)]]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-project-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-project-tmenu ()
  "Menu for ‘Project’ commands.

Commands pertaining to project operations can be accessed here."
  ["Project"
   :description (lambda () (casual-editkit--current-project-label))
   ["File"
    ("f" "Open…" project-find-file)
    ("B" "Switch buffer…" project-switch-to-buffer)]

   ["Dired"
    ("d" "Dired…" project-dired)
    ("D" "Dired in…" project-find-dir)
    ("v" "VC Dired…" project-vc-dir)]

   ["Search/Replace"
    ("r" "Search regexp…" project-find-regexp)
    ("q" "Query replace regexp…" project-query-replace-regexp)
    ("S" "Search regexp sequential…" project-search :transient t)
    ("n" "Next sequential" fileloop-continue :transient t)]

   ["Tools"
    :pad-keys t
    ("c" "Compile…" project-compile)
    ("e" "Eshell" project-eshell)
    ("M-s" "Shell" project-shell)
    ("!" "Shell command…" project-shell-command)
    ("&" "Async command…" project-async-shell-command)]]

  ["Project"
   :class transient-row
    ("s" "Switch…" project-switch-project)
    ("b" "List buffers" project-list-buffers)
    ("k" "Kill buffers" project-kill-buffers)
    ("F" "Forget" project-forget-project)]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-edit-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-edit-tmenu ()
  "Menu for ‘Edit’ commands.

Commands pertaining to editing operations can be accessed here."
  ["Edit"
   [("m" "Mark›" casual-editkit-mark-tmenu)
    ("c" "Copy›" casual-editkit-copy-tmenu)
    ("k" "Kill (Cut)›" casual-editkit-kill-tmenu
     :if-not casual-editkit-buffer-read-only-p)
    ("y" "Yank (Paste)" yank
    :if-not (lambda () buffer-read-only))]

   [("t" "Transpose›" casual-editkit-transpose-tmenu
     :if-not casual-editkit-buffer-read-only-p)
    ("T" "Transform›" casual-editkit-transform-text-tmenu
     :if-not casual-editkit-buffer-read-only-p)
    ("v" "Move›" casual-editkit-move-text-tmenu
     :if-not casual-editkit-buffer-read-only-p)
    ("d" "Delete›" casual-editkit-delete-tmenu
     :if-not casual-editkit-buffer-read-only-p)]

   [("s" "Sort›" casual-editkit-sort-tmenu
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p)
    ("D" "Duplicate" duplicate-dwim
     :if-not casual-editkit-buffer-read-only-p
     :transient t)
    ("F" "Flush Lines…" flush-lines
     :inapt-if-not use-region-p
     :if-not casual-editkit-buffer-read-only-p)
    ("K" "Keep Lines…" keep-lines
     :inapt-if-not use-region-p
     :if-not casual-editkit-buffer-read-only-p)]

   [("f" "Fill Paragraph" fill-paragraph
     :if-not casual-editkit-buffer-read-only-p)
    ("a" "Align Regexp" align-regexp
     :inapt-if-not use-region-p)
    ("R" "Rectangle›" casual-editkit-rectangle-tmenu)
    ("r" "Reformat›" casual-editkit-reformat-tmenu
     :if-not (lambda () buffer-read-only))]]


  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Done" transient-quit-all)
   ("U" "Undo" undo :transient t)
   (casual-lib-quit-all)])

;;;###autoload (autoload 'casual-editkit-emoji-symbols-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-emoji-symbols-tmenu ()
    "Menu for ‘Emoji & Symbols’ commands.

Commands pertaining to insert character operations can be
accessed here. Included are commands for smart quotes and
inserting common miscellaneous symbols."
  ["Emoji & Symbols"
   ["Insert"
    ("e" "Emoji…" emoji-search)
    ;;("r" "Recent Emoji…" emoji-recent)
    ("i" "Character…" insert-char)]

   ["Emoji"
    ("l" "List" emoji-list)
    ("d" "Describe" emoji-describe)]

   ["Zoom"
    ("+" "Increase" emoji-zoom-increase :transient t)
    ("-" "Decrease" emoji-zoom-decrease :transient t)]

   [""
    ("0" "Reset" emoji-zoom-reset :transient t)]]

  ["Smart Quotes"
   :class transient-row
   ("'" "‘single’" casual-editkit-smart-single-quote-dwim)
   ("\"" "“double”" casual-editkit-smart-double-quote-dwim)
   ("_" "„low”" casual-editkit-smart-low-quote-dwim)
   ("c" "«comillas»" casual-editkit-smart-comillas-quote-dwim)
   ("a" "’" (lambda () (interactive) (insert "’")))]

  ["Misc"
   :class transient-row
   ("." "…" (lambda () (interactive) (insert "…")))
   ("b" "•" (lambda () (interactive) (insert "•")))
   ("m" "—" (lambda () (interactive) (insert "—")))
   ("o" "°" (lambda () (interactive) (insert "°")))
   ("?" "¿" (lambda () (interactive) (insert "¿")))
   ("!" "¡" (lambda () (interactive) (insert "¡")))
   ("p" "¶" (lambda () (interactive) (insert "¶")))
   ("s" "§" (lambda () (interactive) (insert "§")))
   ("C" "©" (lambda () (interactive) (insert "©")))
   ("r" "®" (lambda () (interactive) (insert "®")))
   ("t" "™" (lambda () (interactive) (insert "™")))]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Done" transient-quit-all)
   ("U" "Undo" undo :transient t)
   (casual-lib-quit-all)])

;;;###autoload (autoload 'casual-editkit-mark-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-mark-tmenu ()
  "Menu for ‘Mark’ commands.

Commands pertaining to marking operations can be accessed here."
  ["Mark"
   [("w" "Word"  mark-word)
    ("s" "Sentence" mark-end-of-sentence)
    ("p" "Paragraph" mark-paragraph)]
   [("b" "Balanced Expression (sexp)" mark-sexp)
    ("d" "Defun" mark-defun
     :if (lambda () (derived-mode-p 'prog-mode)))]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-copy-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-copy-tmenu ()
  "Menu for ‘Copy’ commands.

Commands pertaining to copying can be accessed here."
  ["Copy"
   [("w" "Word" casual-editkit-copy-word)
    ("s" "Sentence" casual-editkit-copy-sentence)
    ("p" "Paragraph" casual-editkit-copy-paragraph)]
   [("b" "Balanced Expression (sexp)" casual-editkit-copy-sexp)
    ("d" "Defun" casual-editkit-copy-defun
     :if (lambda () (derived-mode-p 'prog-mode)))
    ("m" "Matching Lines…" copy-matching-lines)
    ("r" "Region" kill-ring-save
     :inapt-if-not use-region-p)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-kill-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-kill-tmenu ()
  "Menu for ‘Kill (Cut)’ commands.

Commands pertaining to kill ring operations can be accessed here."
  ["Kill (Cut)"
   [("w" "Word"  kill-word)
    ("s" "Sentence" kill-sentence)
    ("p" "Paragraph" kill-paragraph)]
   [("l" "Line" kill-line)
    ("b" "Balanced Expression (sexp)" kill-sexp)
    ("m" "Matching Lines…" kill-matching-lines)
    ("r" "Region" kill-region
     :inapt-if-not use-region-p)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-sort-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-sort-tmenu ()
  "Menu for ‘Sort’ commands.

Commands pertaining to sorting operations can be accessed here."
  ["Sort"
   :inapt-if-not use-region-p
   [("l" "Lines" sort-lines)
    ("p" "Paragraphs" sort-paragraphs)
    ("P" "Pages" sort-pages)]

   [("f" "Fields" sort-fields)
    ("n" "Numeric Fields" sort-numeric-fields)
    ("r" "Regexp Fields…" sort-regexp-fields)]

   [("-" "Reverse" reverse-region)
    ("c" "Columns" sort-columns)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-transpose-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-transpose-tmenu ()
  "Menu for ‘Transpose’ commands.

Commands pertaining to transpose operations can be accessed here."
  ["Transpose"
   [("c" "Characters" transpose-chars)
    ("w" "Words" transpose-words)
    ("l" "Lines" transpose-lines)
    ("s" "Sentences" transpose-sentences)]
   [("p" "Paragraphs" transpose-paragraphs)
    ("b" "Balanced Expression (sexp)" transpose-sexps)
    ("r" "Regions" transpose-regions)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-delete-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-delete-tmenu ()
  "Menu for ‘Delete’ commands.

Commands pertaining to delete can be accessed here."
  ["Delete"
   [("o" "Just One Space" just-one-space)
    ("j" "Join Line" join-line)
    ("h" "Horizontal Space" delete-horizontal-space)
    ("p" "Pair" delete-pair)]

   [("b" "Blank Lines" delete-blank-lines)
    ("w" "Whitespace Cleanup" whitespace-cleanup)
    ("d" "Delete Trailing Whitespace" delete-trailing-whitespace)]

   [("z" "Zap up to…" zap-up-to-char)
    ("Z" "Zap to…" zap-to-char)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-move-text-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-move-text-tmenu ()
  "Menu for ‘Move’ commands.

Commands pertaining to move operations can be accessed here."
  ["Move Text"
   ("w" "Word›"  casual-editkit-move-word-tmenu)
   ("s" "Sentence›"  casual-editkit-move-sentence-tmenu)
   ("b" "Balanced Expression (sexp)›" casual-editkit-move-sexp-tmenu)]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

(transient-define-prefix casual-editkit-move-word-tmenu ()
  "Menu for ‘Move Word’ commands.

Commands pertaining to move word operations can be accessed here."
  ["Move Word"
   :class transient-row
   ("b" "Backward"  casual-editkit-move-word-backward :transient t)
   ("f" "Forward"  casual-editkit-move-word-forward :transient t)
   ("RET" "Done" transient-quit-all)]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

(transient-define-prefix casual-editkit-move-sentence-tmenu ()
  "Menu for ‘Move Sentence’ commands.

Commands pertaining to move sentence operations can be accessed here."
  ["Move Sentence"
   :class transient-row
   ("b" "Backward"  casual-editkit-move-sentence-backward :transient t)
   ("f" "Forward"  casual-editkit-move-sentence-forward :transient t)
   ("RET" "Done" transient-quit-all)]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

(transient-define-prefix casual-editkit-move-sexp-tmenu ()
  "Menu for ‘Move Sexp’ commands.

Commands pertaining to move sexp (balanced expression) operations
can be accessed here."
  ["Move Sexp"
   :class transient-row
   ("b" "Backward"  casual-editkit-move-sexp-backward :transient t)
   ("f" "Forward"  casual-editkit-move-sexp-forward :transient t)
   ("RET" "Done" transient-quit-all)]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-windows-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-windows-tmenu ()
  "Menu for ‘Window’ commands.

Commands pertaining to window management operations can be accessed here."
  ["Window"
   ["Navigate"
    :pad-keys t
    ("o" "»" other-window
     :description (lambda () (casual-editkit-unicode-get :other-window))
     :transient t)
    ("p" "↑" windmove-up
     :description (lambda () (casual-editkit-unicode-get :point-up))
     :transient t)
    ("n" "↓" windmove-down
     :description (lambda () (casual-editkit-unicode-get :point-down))
     :transient t)
    ("b" "←" windmove-left
     :description (lambda () (casual-editkit-unicode-get :point-left))
     :transient t)
    ("f" "→" windmove-right
     :description (lambda () (casual-editkit-unicode-get :point-right))
     :transient t)]

   ["Swap"
    :pad-keys t
    ("s" "⇄"
     window-swap-states
     :description (lambda () (casual-editkit-unicode-get :swap)))
    ("M-p" "↑" windmove-swap-states-up
     :description (lambda () (casual-editkit-unicode-get :point-up)))
    ("M-n" "↓" windmove-swap-states-down
     :description (lambda () (casual-editkit-unicode-get :point-down)))
    ("M-b" "←" windmove-swap-states-left
     :description (lambda () (casual-editkit-unicode-get :point-left)))
    ("M-f" "→" windmove-swap-states-right
     :description (lambda () (casual-editkit-unicode-get :point-right)))]

   ["New"
    ("1" "❏" delete-other-windows
     :description (lambda () (casual-editkit-unicode-get :delete-other-windows)))
    ("2" "⇩" split-window-below
     :description (lambda () (casual-editkit-unicode-get :split-window-below)))
    ("3" "⇨" split-window-horizontally
     :description (lambda () (casual-editkit-unicode-get :split-window-horizontally)))]

   ["Misc"
    ("t" "Transpose" casual-editkit-transpose-frame
     :if casual-editkit-package-transpose-frame-installed-p)
    ;; ("T" "Toggle Tab Bar" mac-toggle-tab-bar
    ;;  :if casual-editkit-window-system-mac-p)
    ;;("J" "Jump to Window…" ace-select-window)
    ("d" "Delete›" casual-editkit-windows-delete-tmenu)]]

  ["Resize"
   ["↕︎"
    :description (lambda () (casual-editkit-unicode-get :vertical))
    ("+" "Enlarge" enlarge-window
     :description (lambda () (casual-editkit-unicode-get :enlarge))
     :transient t)
    ("-" "Shrink" shrink-window
     :description (lambda () (casual-editkit-unicode-get :shrink))
     :transient t)]
   ["↔︎"
    :description (lambda () (casual-editkit-unicode-get :horizontal))
    (">" "Enlarge" enlarge-window-horizontally
     :description (lambda () (casual-editkit-unicode-get :enlarge))
     :transient t)
    ("<" "Shrink" shrink-window-horizontally
     :description (lambda () (casual-editkit-unicode-get :shrink))
     :transient t)]]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-windows-delete-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-windows-delete-tmenu ()
    "Menu for ‘Window Delete’ commands.

Commands pertaining to window deletion operations can be
accessed here."
  ["Delete Window"
   ("p" "Above" windmove-delete-up)
   ("n" "Below" windmove-delete-down)
   ("b" "On Left" windmove-delete-left)
   ("f" "On Right" windmove-delete-right)]
  [(casual-lib-quit-all)])

;;;###autoload (autoload 'casual-editkit-bookmarks-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-bookmarks-tmenu ()
    "Menu for ‘Bookmarks’ commands.

Commands pertaining to bookmark operations can be
accessed here."
  ["Bookmarks"
   ("e" "Edit Bookmarks" casual-editkit-list-bookmarks-transient)
   ("a" "Add Bookmark…" bookmark-set-no-overwrite)
   ("J" "Jump to Bookmark…" bookmark-jump)]
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-search-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-search-tmenu ()
  "Menu for ‘Search & Replace’ commands.

Commands pertaining to search and replace operations can be
accessed here."

  ["Arguments ⚙︎"
   :class transient-row
   ("b" "Backward" "--backward")
   ("x" "Regexp" "--regexp")]

  [["Search ⚙︎"
    ("s" "I-Search…" casual-editkit--isearch)
    ("S" "Search…" casual-editkit--search)]

   ["Replace ⚙︎"
    ("r" "Query Replace…" casual-editkit--query-replace)]

   ["Occur"
    ("o" "Occur…" occur)]

   ["Find"
    ("g" "Find in Files (rgrep)…" rgrep)
    ("d" "Files…" find-name-dired)
    ("G" "Files containing text…" find-grep-dired)]]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-tools-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-tools-tmenu ()
    "Menu for ‘Tools’ commands.

Commands pertaining to invoking different tools can be accessed here."
  ["Tools"
   ["Shells & REPLs"
    ("s" "Shell" shell)
    ("!" "Shell Command…" shell-command)
    ("&" "Shell Command &…" async-shell-command)
    ("e" "Eshell" eshell)
    ("i" "IELM" ielm)
    ("t" "term" term)
    ("p" "Python" run-python)]

   ["Utilities"
    ("c" "Calc" calc)
    ("r" "RE-Builder" re-builder)
    ("w" "Word Count" (lambda ()
                        (interactive)
                        (call-interactively #'count-words)))]

   ["Almanac"
    :pad-keys t
    ("a" "Calendar" calendar)
    ("C" "World Clock" world-clock)
    ("S" "Sunrise/Sunset" sunrise-sunset)]

   ["Misc"
    :pad-keys t
    ("E" "erc" erc)
    ("M-e" "eww" eww)]

   ["Fun"
    ("T" "Tetris" tetris)
    ("z" "Zone" zone)]]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-registers-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-registers-tmenu ()
  "Menu for ‘Registers’ commands.

Commands pertaining to register operations can be accessed here."
  ["Registers"
   ["Jump"
    ("j" "Jump…" jump-to-register)]

   ["Store"
    ("p" "Point…" point-to-register)
    ("w" "Window Configuration…" window-configuration-to-register)
    ("m" "Keyboard Macro…" kmacro-to-register)]

   ["Copy Text"
    :inapt-if-not use-region-p
    ("c" "Region…" copy-to-register)
    ("r" "Rectangle…" copy-rectangle-to-register)
    ("a" "Append to Register…" append-to-register)
    ("P" "Prepend to Register…" prepend-to-register)]

   ["Insert"
    ("i" "Insert Text…" insert-register)]]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-rectangle-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-rectangle-tmenu ()
  "Menu for ‘Rectangle’ commands.

Commands pertaining to rectangle operations can be accessed here."
  :refresh-suffixes t
  ["Rectangle"
   ["Mark"
    ("m" "Mark" rectangle-mark-mode :transient t)]

   ["Edit"
    ("k" "Kill" kill-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("c" "Copy" copy-rectangle-as-kill
     :inapt-if-not use-region-p
     :transient t)
    ("y" "Yank" yank-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :transient t)
    ("d" "Delete" delete-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)]

   ["Replace"
    ("s" "String…" string-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("i" "String Insert…" string-insert-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("o" "Open Insert" open-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)]

   ["Misc"
    ("N" "Number" rectangle-number-lines
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("C" "Clear" clear-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("D" "Delete Leading Spaces" delete-whitespace-rectangle
     :if-not casual-editkit-buffer-read-only-p
     :inapt-if-not use-region-p
     :transient t)
    ("RET" "Done" transient-quit-all)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-transform-text-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-transform-text-tmenu ()
  "Menu for ‘Transform’ commands.

Commands pertaining to transformation operations can be accessed here."
  ["Transform"
   [("c" "Capitalize" capitalize-dwim :transient t)
    ("t" "Title Region (Upcase Initials)" upcase-initials-region
     :transient t
     :inapt-if-not use-region-p)]

   [("l" "Make Lower Case" downcase-dwim :transient t)
    ("u" "Make Upper Case" upcase-dwim :transient t)]

   [("RET" "Done" transient-quit-all)]]
  casual-editkit-cursor-navigation-group
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-macro-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-macro-tmenu ()
  "Menu for ‘Macro’ commands.

Commands pertaining to macro operations can be accessed here."
  [["Macro"
    ("x" "Execute last" kmacro-end-and-call-macro)
    ("n" "Name last…" kmacro-name-last-macro)
    ("b" "Bind last…" kmacro-bind-to-key)]

   ["Edit"
    ("e" "Last macro" kmacro-edit-macro)
    ("B" "Macro with binding…" edit-kbd-macro)]

   ["Insert"
    ("i" "Insert with name…" insert-kbd-macro)]]

  [("I" "ⓘ Keyboard Macros" casual-editkit-macro-info)]

  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-reformat-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-reformat-tmenu ()
  "Menu for ‘Reformat’ commands.

Commands pertaining to reformat operations such as refill can be
accessed here."


  ["Reformat"
   ["Fill"
    :pad-keys t
    ("p" "Paragraph" fill-paragraph)
    ("r" "Region" fill-region
     :inapt-if-not use-region-p)
    ("P" "Region as Paragraph" fill-region-as-paragraph
     :inapt-if-not use-region-p)
    ("M-p" "Individual Paragraphs" fill-individual-paragraphs
     :inapt-if-not use-region-p)
    ("n" "Non-Uniform Paragraphs" fill-nonuniform-paragraphs
     :inapt-if-not use-region-p)]

   ["Center"
    ("C-l" "Line" center-line)
    ("C-r" "Region" center-region
     :inapt-if-not use-region-p)
    ("C-p" "Paragraph" center-paragraph)]

   ["Misc"
    ("R" "Repunctuate" repunctuate-sentences)
    ("u" "Untabify" untabify
     :inapt-if-not use-region-p)]]

  ["Configure"
   ("a" "Auto-fill Mode" auto-fill-mode
     :description (lambda ()
                    (casual-lib-checkbox-label auto-fill-function "Auto-fill Mode")))
   ("d" "Double Space" casual-editkit--customize-sentence-end-double-space
     :description (lambda ()
                    (casual-lib-checkbox-label sentence-end-double-space "Double Space Sentences")))

   ("C" "Fill Column" set-fill-column
     :description (lambda ()
                    (format "Fill Column (%d)" fill-column)))]
  casual-editkit-navigation-group)

;;;###autoload (autoload 'casual-editkit-narrow-tmenu "casual-editkit-utils" nil t)
(transient-define-prefix casual-editkit-narrow-tmenu ()
  "Menu for narrow commands."

  ["Narrow"
   ["Programming"
    ("d" "Defun" narrow-to-defun
     :if (lambda () (derived-mode-p 'prog-mode)))]

   ["Org"
    :if (lambda () (derived-mode-p 'org-mode))
    ("s" "Subtree" org-narrow-to-subtree)
    ("b" "Block" org-narrow-to-block)
    ("e" "Element" org-narrow-to-element)]

   ["Region"
    ("r" "Region" narrow-to-region
     :inapt-if-not use-region-p)]]

  casual-editkit-navigation-group)

;; !!!
;; If markdown-mode is installed, then this will add markdown-mode specific
;; narrowing commands to casual-editkit-narrow-tmenu.
;;
;; (transient-append-suffix 'casual-editkit-narrow-tmenu '(0 0)
;;    ["Markdown"
;;     :if (lambda () (derived-mode-p 'markdown-mode))
;;     ("s" "Subtree" markdown-narrow-to-subtree)
;;     ("b" "Block" markdown-narrow-to-block)
;;     ("p" "Page" markdown-narrow-to-page)])

;;; Functions
(defun casual-editkit-macro-info ()
  "Get Info for syntax of regexps."
  (interactive)
  (info "(emacs) Keyboard Macros"))

(defun casual-editkit-list-bookmarks-transient ()
  "Transient supporting version of `bookmark-bmenu-list'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
    (switch-to-buffer buf))
  (bookmark-bmenu-mode)
  (bookmark-bmenu--revert))

(defun casual-editkit-copy-word ()
  "Copy word after point."
  (interactive)
  (mark-word)
  (kill-ring-save (region-beginning) (region-end)))

(defun casual-editkit-copy-sentence ()
  "Copy sentence after point."
  (interactive)
  (save-excursion
    (set-mark (point))
    (forward-sentence)
    (kill-ring-save (region-beginning) (region-end))))

(defun casual-editkit-copy-paragraph ()
  "Copy paragraph point is in."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (kill-ring-save (region-beginning) (region-end))))

(defun casual-editkit-copy-defun ()
  "Copy defun point is in."
  (interactive)
  (save-excursion
    (mark-defun)
    (kill-ring-save (region-beginning) (region-end))))

(defun casual-editkit-copy-sexp ()
  "Copy sexp after point."
  (interactive)
  (mark-sexp)
  (kill-ring-save (region-beginning) (region-end)))

(defun casual-editkit-move-word-backward ()
  "Move word to the right of point backward one word.
Point must be at the beginning of word."
  (interactive)
  (transpose-words 1)
  (forward-word -2))

(defun casual-editkit-move-word-forward ()
  "Move word to the right of point forward one word.
Point must be at the beginning of word."
  (interactive)
  (forward-word 1)
  (transpose-words 1)
  (forward-word -1))

(defun casual-editkit-move-sentence-backward ()
  "Move sentence to the right of point backward one sentence.
Point must be at the beginning of sentence."
  (interactive)
  (transpose-sentences 1)
  (forward-sentence -2))

(defun casual-editkit-move-sentence-forward ()
  "Move sentence to the right of point forward one sentence.
Point must be at the beginning of sentence."
  (interactive)
  (forward-sentence 1)
  (transpose-sentences 1)
  (forward-sentence -1))

(defun casual-editkit-move-sexp-backward ()
  "Move balanced expression (sexp) to the right of point backward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (transpose-sexps 1)
  (forward-sexp -2))

(defun casual-editkit-move-sexp-forward ()
  "Move balanced expression (sexp) to the right of point forward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (forward-sexp 1)
  (transpose-sexps 1)
  (forward-sexp -1))

(defun casual-editkit--query-replace ()
  "Call `query-replace' variant function given Transient prefix arguments.

This function will invoke either `query-replace' or
`query-replace-regexp' depending on if the Transient prefix
argument ‘--regexp’ is enabled.

This function is intended to be called from a Transient prefix
that supports the argument ‘--regexp’."
  (interactive)

  (let* ((current-command (transient-args transient-current-command))
         (regexp (transient-arg-value "--regexp" current-command)))

    (if regexp
        (call-interactively #'query-replace-regexp)
      (call-interactively #'query-replace))))

(defun casual-editkit--search ()
  "Call search variant function given Transient prefix arguments.

This function will invoke a variant of the search* commands given
following the Transient prefix arguments:

‘--backward’
‘--regexp’

The following matrix shows the command that is called given the
argument values:

| --backward | --regexp | command                          |
|------------+----------+----------------------------------|
| nil        | nil      | command `search-forward'         |
| nil        | t        | command `search-forward-regexp'  |
| t          | nil      | command `search-backward'        |
| t          | t        | command `search-backward-regexp' |

This function is intended to be called from a Transient prefix
that supports the arguments ‘--backward’ and ‘--regexp’."
  (interactive)

  (let* ((current-command (transient-args transient-current-command))
         (reverse (transient-arg-value "--backward" current-command))
         (regexp (transient-arg-value "--regexp" current-command)))
    (if reverse
        (if regexp
            (call-interactively #'search-backward-regexp)
          (call-interactively #'search-backward))
      (if regexp
            (call-interactively #'search-forward-regexp)
        (call-interactively #'search-forward)))))


(defun casual-editkit--isearch ()
  "Call isearch variant function given Transient prefix arguments.

This function will invoke a variant of the isearch commands given
following the Transient prefix arguments:

‘--backward’
‘--regexp’

The following matrix shows the command that is called given the
argument values:

| --backward | --regexp | command                           |
|------------+----------+-----------------------------------|
| nil        | nil      | command `isearch-forward'         |
| nil        | t        | command `isearch-forward-regexp'  |
| t          | nil      | command `isearch-backward'        |
| t          | t        | command `isearch-backward-regexp' |

This function is intended to be called from a Transient prefix
that supports the arguments ‘--backward’ and ‘--regexp’."
  (interactive)

  (let* ((current-command (transient-args transient-current-command))
         (reverse (transient-arg-value "--backward" current-command))
         (regexp (transient-arg-value "--regexp" current-command)))
    (if reverse
        (if regexp
            (call-interactively #'isearch-backward-regexp)
          (call-interactively #'isearch-backward))
      (if regexp
            (call-interactively #'isearch-forward-regexp)
        (call-interactively #'isearch-forward)))))


(defun casual-editkit--smart-quote-dwim (open-quote close-quote)
  "Insert or enclose a region with OPEN-QUOTE, CLOSE-QUOTE.

OPEN-QUOTE - string to open quote with.
CLOSE-QUOTE - string to close quote with.

If `use-region-p' is t, then enclose region with OPEN-QUOTE,
CLOSE-QUOTE, otherwise just insert OPEN-QUOTE and CLOSE-QUOTE
with no space between."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (content (string-trim (buffer-substring start end))))
        (delete-region start end)
        (insert open-quote content close-quote))
    (insert open-quote close-quote)))

(defun casual-editkit-smart-single-quote-dwim ()
  "Insert or enclose a region with smart single quotes."
  (interactive)
  (casual-editkit--smart-quote-dwim "‘" "’"))

(defun casual-editkit-smart-double-quote-dwim ()
  "Insert or enclose a region with smart double quotes."
  (interactive)
  (casual-editkit--smart-quote-dwim "“" "”"))

(defun casual-editkit-smart-low-quote-dwim ()
  "Insert or enclose a region with „”."
  (interactive)
  (casual-editkit--smart-quote-dwim "„" "”"))

(defun casual-editkit-smart-comillas-quote-dwim ()
  "Insert or enclose a region with «»."
  (interactive)
  (casual-editkit--smart-quote-dwim "«" "»"))

(defun casual-editkit--current-project-label ()
  "Current project label."
  (let* ((project (project-current)))
    (if project
        (format "Project: %s" (nth 2 project))
      "Project")))

(provide 'casual-editkit-utils)
;;; casual-editkit-utils.el ends here
