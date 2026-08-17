;;; casual-ispell.el --- Transient UI for Ispell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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

;; Casual Ispell is a Transient user interface for the Ispell library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Ispell by running the hook
;; function `casual-ispell-init'.

;; Ensure that `casual-ispell-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Ispell Install' for more detail on
;; installation.

;;; Code:
(require 'casual-ispell-settings)
(require 'casual-ispell-utils)

;;;###autoload (autoload 'casual-ispell-init "casual-ispell" nil t)
(defun casual-ispell-init ()
  "Initialize and configure Casual Ispell.

This hook binds `casual-ispell-tmenu' to `casual-ispell-keybinding' via
the functions `casual-ispell-prog-setup' and `casual-ispell-text-setup'.

If `casual-ispell-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-ispell-bibtex-setup' and
`casual-ispell-conf-setup' will be set."
  (add-hook 'prog-mode-hook #'casual-ispell-prog-setup)
  (add-hook 'text-mode-hook #'casual-ispell-text-setup)

  (when casual-ispell-add-extra-keybindings
    (add-hook 'bibtex-mode-hook #'casual-ispell-bibtex-setup)
    (add-hook 'conf-mode-hook #'casual-ispell-conf-setup)))

(defun casual-ispell-prog-setup ()
  "Setup `prog-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set prog-mode-map casual-ispell-keybinding #'casual-ispell-tmenu))

(defun casual-ispell-text-setup ()
  "Setup `text-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set text-mode-map casual-ispell-keybinding #'casual-ispell-tmenu))

(defun casual-ispell-bibtex-setup ()
  "Setup `bibtex-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set bibtex-mode-map casual-ispell-keybinding #'casual-ispell-tmenu))

(defun casual-ispell-conf-setup ()
  "Setup `conf-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set conf-mode-map casual-ispell-keybinding #'casual-ispell-tmenu))

;;;###autoload (autoload 'casual-ispell-tmenu "casual-ispell" nil t)
(transient-define-prefix casual-ispell-tmenu ()
  "Casual Ispell menu."
  :refresh-suffixes t
  :transient-non-suffix t

  ["Casual Ispell"
   [("w" "Word" ispell-word)
    ("r" "Region" ispell-region
     :inapt-if-not use-region-p)
    ("b" "Buffer" ispell-buffer)]

   [:if (lambda () (funcall casual-ispell-comment-or-string-predicate))
    ("s" "String/Comment" ispell-comment-or-string-at-point)
    ("c" "Comments & Strings" ispell-comments-and-strings)]

   [("TAB" "Complete Word" ispell-complete-word)
    ("SPC" "Complete Word Fragment" ispell-complete-word-interior-frag)]

   [("x" "Kill Ispell" ispell-kill-ispell)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-ispell-settings-tmenu)
   ("I" "ⓘ" casual-ispell-info)
   ("D" "Change Dictionary…" ispell-change-dictionary)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])

(provide 'casual-ispell)
;;; casual-ispell.el ends here
