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

;; This library provides a Transient-based user interface for the Emacs spell
;; checker `ispell'.

;; INSTALLATION

;; If you have `casual-editkit' installed, then access to `casual-ispell-tmenu'
;; is available from `casual-editkit-main-tmenu'.

;; `casual-ispell-tmenu' can also be bound to a mode of preference. Shown below
;; is a recommended configuration to use in an initialization file.

;; (require 'casual-ispell) ; optional if using autoloaded menu
;; (keymap-set prog-mode-map "C-c s" #'casual-ispell-tmenu)
;; (keymap-set text-mode-map "C-c s" #'casual-ispell-tmenu)
;; (keymap-set bibtex-mode-map "C-c s" #'casual-ispell-tmenu)
;; (keymap-set conf-mode-map "C-c s" #'casual-ispell-tmenu)

;;; Code:
(require 'casual-ispell-settings)
(require 'casual-ispell-utils)

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
