;;; casual-css.el --- Transient UI for CSS mode -*- lexical-binding: t; -*-

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

;; Casual CSS is a Transient user interface for the CSS library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual CSS by running the hook
;; function `casual-css-init'.

;; Ensure that `casual-css-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) CSS Install' for more detail on
;; installation.

;;; Code:
(require 'casual-css-settings)
(require 'casual-css-utils)
(require 'hl-line)


;;;###autoload (autoload 'casual-css-init "casual-css" nil t)
(defun casual-css-init ()
  "Initialize and configure Casual CSS.

This hook binds `casual-css-tmenu' to `casual-keybinding-secondary'.

If `casual-css-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-css-setup' will be set."
  (add-hook 'css-mode-hook #'casual-css-setup)
  (add-hook 'css-ts-mode-hook #'casual-css-ts-setup))

(defun casual-css-setup ()
  "Setup `css-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set css-mode-map casual-keybinding-secondary #'casual-css-tmenu))


(defun casual-css-ts-setup ()
  "Setup `css-ts-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set css-ts-mode-map casual-keybinding-secondary #'casual-css-tmenu))


;;;###autoload (autoload 'casual-css-tmenu "casual-css" nil t)
(transient-define-prefix casual-css-tmenu ()
  "Casual menu for `css-mode'.

Transient menu to commands provided by `css-mode'."
  :refresh-suffixes t

  ["Casual CSS"
   ["CSS"
    ("l" "Lookup Symbol" css-lookup-symbol)
    ("c" "Cycle Color" css-cycle-color-format
     :inapt-if (lambda () buffer-read-only)
     :transient t)]

   ["Edit"
    ("f" "Indent CSS Rule" fill-paragraph
     :inapt-if (lambda () buffer-read-only))]

   ["Misc"
    ("h" "Toggle Highlight-line" hl-line-mode
     :description (lambda () (casual-lib-checkbox-label hl-line-mode "Highlight Line"))
     :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-css-settings-tmenu)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])

(provide 'casual-css)
;;; casual-css.el ends here
