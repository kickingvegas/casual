;;; casual-re-builder.el --- Transient UI for RE-Builder -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

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

;; Casual RE-Builder is a Transient user interface for the RE-Builder library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual RE-Builder by running the hook
;; function `casual-re-builder-init'.

;; Ensure that `casual-re-builder-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) RE-Builder Install' for more detail on
;; installation.

;;; Code:
(require 're-builder)
(require 'rx)
(require 'transient)
(require 'casual-lib)
(require 'casual-re-builder-utils)
(require 'casual-re-builder-settings)

;;;###autoload (autoload 'casual-re-builder-init "casual-re-builder" nil t)
(defun casual-re-builder-init ()
  "Initialize and configure Casual RE-Builder.

This hook binds `casual-re-builder-tmenu' to `casual-keybinding-primary'.

If `casual-re-builder-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-re-builder-setup' will be set."
  (add-hook 'reb-mode-hook #'casual-re-builder-setup))

(defun casual-re-builder-setup ()
  "Setup `reb-mode' (RE-Builder) for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set reb-mode-map casual-keybinding-primary #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map casual-keybinding-primary #'casual-re-builder-tmenu))

;;;###autoload (autoload 'casual-re-builder-tmenu "casual-re-builder" nil t)
(transient-define-prefix casual-re-builder-tmenu ()
  "Transient menu for RE-Builder commands.

Menu for RE-Builder (`re-builder'), a tool to construct a
regexp interactively.

* References
- Info node `(elisp) Regular Expressions'"
  :refresh-suffixes t
  ["RE-Builder"
   :description (lambda () (format "RE-Builder (%s)" reb-target-buffer))
   ["Copy Regexp"
    ("w" "Interactive" casual-re-builder-copy
     :if casual-re-builder-interactive-export-p
     :transient t)
    ("g" "Interactive grep" casual-re-builder-grep-copy
     :if casual-re-builder-interactive-export-p
     :transient t)
    ("c" "Code" reb-copy :transient t)]

   ["Match"
    ("p" "Previous" reb-prev-match
     :description (lambda () (casual-re-builder-unicode-get :previous))
     :transient t)
    ("n" "Next" reb-next-match
     :description (lambda () (casual-re-builder-unicode-get :next))
     :transient t)]

   ["Change"
    ("x" "Syntax" reb-change-syntax
     :description (lambda () (format "Syntax (%s)" reb-re-syntax))
     :transient t)
    ("b" "Target buffer" reb-change-target-buffer
     :transient t)
    ("t" "Case sensitivity" reb-toggle-case :transient t)]

   ["Display"
    ("s" "Subexp mode" reb-enter-subexp-mode)
    ("f" "Force update" reb-force-update :transient t)]]

  ["Misc"
   ("o" "Occur" casual-reb-occur)]

  [:class transient-row
   (casual-lib-quit-one)
   ("i" "ⓘ Regexp Syntax" casual-re-builder-regexp-info
    :if (lambda () (derived-mode-p 'reb-mode)))
   ("i" "ⓘ Rx Notation" casual-re-builder-rx-info
    :if (lambda () (derived-mode-p 'reb-lisp-mode)))
   ("," "Settings" casual-re-builder-settings-tmenu)
   ("q" "Quit" reb-quit)
   (casual-lib-quit-all)])

(provide 'casual-re-builder)
;;; casual-re-builder.el ends here
