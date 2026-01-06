;;; casual-eshell.el --- Transient UI for Eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Charles Y. Choi

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

;; This library provides a Transient-based user interface for `eshell-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-eshell-tmenu' to your
;; key binding of preference.

;; (require 'casual-eshell) ; optional if using autoloaded menu
;; (keymap-set eshell-mode-map "C-o" #'casual-eshell-tmenu)

;;; Code:

(require 'casual-eshell-settings)
(require 'casual-eshell-utils)
(require 'esh-mode)
(require 'esh-arg)
(require 'em-hist)
(require 'em-prompt)

;;;###autoload (autoload 'casual-eshell-tmenu "casual-eshell" nil t)
(transient-define-prefix casual-eshell-tmenu ()
  "Transient menu for Eshell."
  :refresh-suffixes t
  ["Casual Eshell"
   :description (lambda ()
                  (format
                   "Casual Eshell: %s"
                   (casual-eshell-tilde-path default-directory)))
   ["Input"
    :pad-keys t
    :if-not buffer-narrowed-p
    ("B" "#<buffer >…" eshell-insert-buffer-name)
    ("k" "Clear" eshell-kill-input
    :description (lambda () (casual-eshell-unicode-get :clear)))
    ("h" "History" eshell-list-history)]

   ["Argument"
    :if-not buffer-narrowed-p
    ("b" "Backward" eshell-backward-argument
     :description (lambda () (casual-eshell-unicode-get :backward))
     :transient t)
    ("f" "Forward" eshell-forward-argument
     :description (lambda () (casual-eshell-unicode-get :forward))
     :transient t)
    ("y" "Repeat" eshell-repeat-argument
     :description (lambda () (casual-eshell-unicode-get :repeat))
     :transient t)]

   ["Prompt"
    :if-not buffer-narrowed-p
    ("p" "Previous" eshell-previous-prompt
     :description (lambda () (casual-eshell-unicode-get :previous))
     :transient t)
    ("n" "Next" eshell-next-prompt
     :description (lambda () (casual-eshell-unicode-get :next))
     :transient t)
    ("RET" "Clone" eshell-copy-old-input)]

   ["Output"
    :if-not buffer-narrowed-p
    ("s" "Show" eshell-show-output)
    ("." "Show Max" eshell-show-maximum-output)
    ("m" "Mark" eshell-mark-output
     :description (lambda () (if prefix-arg "Narrow" "Mark✦")))
    ("D" "Delete" eshell-delete-output
     :description (lambda () (if prefix-arg "Kill" "Delete✦")))]

   ["Output"
    :if buffer-narrowed-p
    ("w" "Widen" (lambda ()
                   (interactive)
                   (widen)
                   (eshell-show-maximum-output)))]

   ["Misc"
    ("d" "Dired" dired-jump-other-window)
    ("a" "Edit Aliases" casual-eshell-edit-aliases)
    ("J" "Jump to Bookmark…" bookmark-jump)]]

  ["Process"
   :if (lambda () (car eshell-process-list))
   :class transient-row
   ("Pi" "Interrupt" eshell-interrupt-process)
   ("Pk" "Kill" eshell-kill-process)
   ("Pq" "Quit" eshell-quit-process)]

  [:class transient-row
   (casual-lib-quit-one)
   ("i" "ⓘ›" casual-eshell-info-tmenu)
   ("," "Settings›" casual-eshell-settings-tmenu)
   (casual-lib-quit-all)])

(transient-define-prefix casual-eshell-info-tmenu ()
  "Menu for Eshell Info."

  ["Casual Eshell ⓘ"
   ["Info"
    ("i" "Info" casual-eshell-info)]

   ["Commands"
    ("b" "Built-in Commands" casual-eshell-info-builtins)
    ("a" "Aliases" casual-eshell-info-aliases)
    ("r" "Remote Access" casual-eshell-info-remote-access)
    ("c" "Control Flow" casual-eshell-info-control-flow)]

   ["Expansion"
    ("e" "Expansion" casual-eshell-info-expansion)
    ("d" "$" casual-eshell-info-dollars-expansion)]

   ["I/O"
    ("R" "Redirection" casual-eshell-info-redirection)
    ("p" "Pipelines" casual-eshell-info-pipelines)]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(provide 'casual-eshell)
;;; casual-eshell.el ends here
