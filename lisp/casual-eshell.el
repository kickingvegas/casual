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

;; Casual Eshell is a Transient user interface for the Eshell library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Eshell by running the hook
;; function `casual-eshell-init'.

;; Ensure that `casual-eshell-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Eshell Install' for more detail on
;; installation.

;;; Code:

(require 'casual-eshell-settings)
(require 'casual-eshell-utils)
(require 'esh-mode)
(require 'esh-arg)
(require 'em-hist)
(require 'em-prompt)

;;;###autoload (autoload 'casual-eshell-init "casual-eshell" nil t)
(defun casual-eshell-init ()
  "Initialize and configure Casual Eshell.

This hook binds `casual-eshell-tmenu' to `casual-keybinding-primary'.

If `casual-eshell-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-eshell-setup' will be set."
  (add-hook 'eshell-mode-hook #'casual-eshell-setup))

(defun casual-eshell-setup ()
  "Setup `eshell-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set eshell-mode-map casual-keybinding-primary #'casual-eshell-tmenu))

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
    ("c" "Clone" eshell-copy-old-input
     :inapt-if (lambda () (= (point) (point-max))))]

   ["Output"
    :if-not buffer-narrowed-p
    ("s" "Show" eshell-show-output)
    ("." "Show Max" eshell-show-maximum-output)
    ("m" "Mark" eshell-mark-output
     :description (lambda () (if prefix-arg "Narrow" "Mark✦")))
    ("w" "Copy Last" casual-eshell-copy-last-output)
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
   ("C-c" "Interrupt" eshell-interrupt-process)
   ("C-k" "Kill" eshell-kill-process)
   ("C-\\" "Quit" eshell-quit-process)]

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

  casual-lib-navigation-group-plain)

(provide 'casual-eshell)
;;; casual-eshell.el ends here
