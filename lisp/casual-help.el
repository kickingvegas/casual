;;; casual-help.el --- Transient UI for Help -*- lexical-binding: t; -*-

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

;; Casual Help is a Transient user interface for the Help library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Help by running the hook
;; function `casual-help-init'.

;; Ensure that `casual-help-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Help Install' for more detail on
;; installation.

;;; Code:
(require 'bookmark)
(require 'button)
(require 'casual-help-settings)
(require 'casual-help-utils)

;;;###autoload (autoload 'casual-help-init "casual-help" nil t)
(defun casual-help-init ()
  "Initialize and configure Casual Help.

This hook binds `casual-help-tmenu' to `casual-keybinding-primary'.

If `casual-help-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-help-setup' will be set."
  (add-hook 'help-mode-hook #'casual-help-setup))

(defun casual-help-setup ()
  "Setup `help-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set help-mode-map casual-keybinding-primary #'casual-help-tmenu)

  (when casual-help-add-extra-keybindings
    (keymap-set help-mode-map "M-[" #'help-go-back)
    (keymap-set help-mode-map "M-]" #'help-go-forward)
    ;; Bind p and n to paragraph navigation
    (keymap-set help-mode-map "p" #'casual-lib-browse-backward-paragraph)
    (keymap-set help-mode-map "n" #'casual-lib-browse-forward-paragraph)
    (keymap-set help-mode-map "P" #'help-goto-previous-page)
    (keymap-set help-mode-map "N" #'help-goto-next-page)
    (keymap-set help-mode-map "j" #'forward-button)
    (keymap-set help-mode-map "k" #'backward-button)))


;;;###autoload (autoload 'casual-help-tmenu "casual-help" nil t)
(transient-define-prefix casual-help-tmenu ()
  "Casual Help main menu."
  :refresh-suffixes t
  ["Casual Help"
   ["Navigation"
    :pad-keys t
    ("C-p" "Previous" previous-line
     :description (lambda () (casual-help-unicode-get :previous))
     :transient t)
    ("C-n" "Next" next-line
     :description (lambda () (casual-help-unicode-get :next))
     :transient t)
    ("<" "Beginning" beginning-of-buffer
     :description (lambda () (casual-help-unicode-get :beginning-of-buffer))
     :transient t)
    (">" "End" end-of-buffer
     :description (lambda () (casual-help-unicode-get :end-of-buffer))
     :transient t)]

   [""
    ("p" "Backward" casual-lib-browse-backward-paragraph
     :description (lambda () (format "%s %s"
                                (casual-help-unicode-get :previous)
                                (casual-help-unicode-get :paragraph)))
     :transient t)
    ("n" "Forward" casual-lib-browse-forward-paragraph
     :description (lambda () (format "%s %s"
                                (casual-help-unicode-get :next)
                                (casual-help-unicode-get :paragraph)))
     :transient t)
    ("P" "Backward" help-goto-previous-page
     :description (lambda () (format "%s %s"
                                (casual-help-unicode-get :previous)
                                (casual-help-unicode-get :page)))
     :transient t)
    ("N" "Forward" help-goto-next-page
     :description (lambda () (format "%s %s"
                                (casual-help-unicode-get :next)
                                (casual-help-unicode-get :page)))
     :transient t)]

   ["History"
    ("M-[" "Previous" help-go-back
     :description (lambda () (casual-help-unicode-get :previous))
     :transient t)
    ("M-]" "Next" help-go-forward
     :description (lambda () (casual-help-unicode-get :next))
     :transient t)]

   ["Link"
    :pad-keys t
    ("j" "Forward" forward-button
     :description (lambda () (casual-help-unicode-get :forward))
     :transient t)
    ("k" "Backward" backward-button
     :description (lambda () (casual-help-unicode-get :backward))
     :transient t)
    ("RET" "Open" push-button)]]

  [["Describe"
    ("ds" "Symbol…" describe-symbol)
    ("dv" "Variable…" describe-variable)]
   [""
    ("dc" "Command…" describe-command)
    ("df" "Function…" describe-function)]

   ["Info"
    :if casual-help--current-data-p
    ("i" "Goto" help-goto-info)
    ("I" "Elisp" help-goto-lispref-info)]

   ["Source"
    ("s" "Source" help-view-source)
    ("c" "Customize" help-customize
     :if casual-help--symbolp)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-help-settings-tmenu)
   ("J" "Jump to Bookmark…" bookmark-jump)
   ("q" "Quit" quit-window)
   (casual-lib-quit-all)])

(provide 'casual-help)
;;; casual-help.el ends here
