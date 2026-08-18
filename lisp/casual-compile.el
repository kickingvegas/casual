;;; casual-compile.el --- Transient UI for Compilation Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Charles Choi

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

;; Casual Compile is a user interface for the output of the `compile' command.
;; The output buffer's major mode is `compilation-mode' whose commands are
;; surfaced by Casual Compile.

;; In similar fashion, output of Emacs-wrapped Grep commands are also supported
;; by Casual Compile. This is because the output of Grep commands use the major
;; mode `grep-mode' which is derived from `compilation-mode'.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Compile and Grep by running the
;; hook functions `casual-compile-init' and `casual-grep-init', respectively.

;; Ensure that `casual-compile-init' and `casual-grep-init' are included in the
;; customizable hook variable `casual-init-hook'.

;; Consult the Info node `(casual) Compile Install' for more detail on
;; installation.

;;; Code:
(require 'bookmark)
(require 'casual-compile-settings)
(require 'casual-compile-utils)

;;;###autoload (autoload 'casual-compile-init "casual-compile" nil t)
(defun casual-compile-init ()
  "Initialize and configure Casual Compile.

This hook binds `casual-compile-tmenu' to `casual-keybinding-primary'.

If `casual-compile-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-compile-setup' will be set."
  (add-hook 'compilation-mode-hook #'casual-compile-setup))

(defun casual-grep-init ()
  "Initialize and configure Casual Compile for `grep-mode'.

This hook binds `casual-compile-tmenu' to `casual-keybinding-primary'.

If `casual-compile-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-grep-setup' will be set."
  (add-hook 'grep-mode-hook #'casual-grep-setup))

(defun casual-compile-setup ()
  "Setup `compilation-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set compilation-mode-map casual-keybinding-primary #'casual-compile-tmenu)

  (when casual-compile-add-extra-keybindings
    (keymap-set compilation-mode-map "k" #'compilation-previous-error)
    (keymap-set compilation-mode-map "j" #'compilation-next-error)
    (keymap-set compilation-mode-map "o" #'compilation-display-error)
    (keymap-set compilation-mode-map "[" #'compilation-previous-file)
    (keymap-set compilation-mode-map "]" #'compilation-next-file)))

(defun casual-grep-setup ()
  "Setup `grep-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set grep-mode-map casual-keybinding-primary #'casual-compile-tmenu)

  (when casual-grep-add-extra-keybindings
    (keymap-set grep-mode-map "k" #'compilation-previous-error)
    (keymap-set grep-mode-map "j" #'compilation-next-error)
    (keymap-set grep-mode-map "o" #'compilation-display-error)
    (keymap-set grep-mode-map "[" #'compilation-previous-file)
    (keymap-set grep-mode-map "]" #'compilation-next-file)))

;;;###autoload (autoload 'casual-compile-tmenu "casual-compile" nil t)
(transient-define-prefix casual-compile-tmenu ()
  "Casual main menu for `compilation-mode'."
  :refresh-suffixes t
  ["Casual Compile"
   :description (lambda ()
                  (format "%s Results"
                          (casual-compile--select-mode-label "Compilation"
                                                             "Grep")))
   ["Follow"
    ("p" "Previous" previous-error-no-select
     :description (lambda () (casual-compile-unicode-get :previous))
     :transient t)
    ("n" "Next" next-error-no-select
     :description (lambda () (casual-compile-unicode-get :next))
     :transient t)]

   ["Item"
    :description (lambda () (casual-compile--select-mode-label "Error"
                                                               "Match"))
    :pad-keys t
    ("k" "Previous" compilation-previous-error
     :description (lambda () (casual-compile-unicode-get :previous))
     :transient t)
    ("j" "Next" compilation-next-error
     :description (lambda () (casual-compile-unicode-get :next))
     :transient t)
    ("o" "Display" compilation-display-error
     :description (lambda () (casual-compile-unicode-get :display))
     :transient t)
    ("RET" "Goto" compile-goto-error
     :description (lambda () (casual-compile-unicode-get :goto)))]

   ["File"
    ("[" "Previous" compilation-previous-file
     :description (lambda () (casual-compile-unicode-get :previous))
     :transient t)
    ("]" "Next" compilation-next-file
     :description (lambda () (casual-compile-unicode-get :next))
     :transient t)]

   ["Compile"
    :description (lambda ()
                   (casual-compile--select-mode-label
                    "Compile"
                    "Refresh"))
    ("g" "Recompile" recompile
     :description (lambda ()
                    (casual-compile--select-mode-label
                     "Recompile"
                     (casual-compile-unicode-get :refresh)))

     :transient t)
    ("c" "Compile" compile
     :if-not (lambda () (derived-mode-p 'grep-mode)))
    ("K" "Kill" kill-compilation
     :description (lambda () (casual-compile-unicode-get :kill))
     :transient t
     :if casual-compile--compilation-running-p)
    ]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("," "Settings›" casual-compile-settings-tmenu)
   ("J" "Jump to Bookmark…" bookmark-jump)
   ("q" "Quit" quit-window)])

(provide 'casual-compile)
;;; casual-compile.el ends here
