;;; casual-make.el --- Transient UI for Make -*- lexical-binding: t; -*-

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

;; Casual Make is a Transient user interface for the Make library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Make by running the hook
;; function `casual-make-init'.

;; Ensure that `casual-make-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Make Install' for more detail on
;; installation.

;;; Code:
(require 'casual-make-utils)
(require 'casual-make-settings)

;;;###autoload (autoload 'casual-make-init "casual-make" nil t)
(defun casual-make-init ()
  "Initialize and configure Casual Make.

This hook binds `casual-make-tmenu' to `casual-keybinding-secondary'.

If `casual-make-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-make-setup' will be set."
  (add-hook 'makefile-mode-hook #'casual-make-setup))

(defun casual-make-setup ()
  "Setup `makefile-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set makefile-mode-map casual-keybinding-secondary #'casual-make-tmenu))

;;;###autoload (autoload 'casual-make-tmenu "casual-make" nil t)
(transient-define-prefix casual-make-tmenu ()
  "Main menu for Casual Make.

This menu provides a user interface for the commands in `make-mode'."

  ["Casual Make"
   :description (lambda () (format "Casual Make (%s)" (casual-make-mode-label major-mode)))
   ["Edit"
    :inapt-if (lambda () (if buffer-read-only t nil))
    ("\\" "Backslash region" makefile-backslash-region :inapt-if-not use-region-p)
    (";" "Comment region" comment-region :inapt-if-not use-region-p)
    (":" "Insert target…" makefile-insert-target-ref)
    ("m" "Insert macro…" makefile-insert-macro-ref)
    ("f" "Insert GNU function…" makefile-insert-gmake-function)
    ("a" "Automatic Variables›" casual-make-automatic-variables-tmenu)]

   ["Pickup as targets"
    ("E" "Everything" makefile-pickup-everything)
    ("F" "Filenames" makefile-pickup-filenames-as-targets)]

   ["Misc"
    ("c" "Compile…" compile)
    ("o" "Overview" makefile-create-up-to-date-overview)
    ("t" "Makefile Type›" casual-make-mode-select-tmenu :transient t)
    ("." "Identify Auto Var" casual-make-identify-autovar-region
     :inapt-if-not use-region-p)]

   ["Navigate"
    ("i" "Index Menu…" imenu :transient t)
    ("p" "Previous" makefile-previous-dependency
     :description (lambda ()
                    (format "%s target" (casual-make-unicode-get :previous)))
     :transient t)
    ("n" "Next" makefile-next-dependency
     :description (lambda ()
                    (format "%s target"(casual-make-unicode-get :next)))
     :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-make-settings-tmenu)
   ("I" "ⓘ Make" (lambda () (interactive) (info "(make) Top")))
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])

(provide 'casual-make)
;;; casual-make.el ends here
