;;; casual-editkit.el --- Transient user interface library for editing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-editkit
;; Keywords: tools, wp
;; Version: 1.0.14-rc.1
;; Package-Requires: ((emacs "29.1") (casual-lib "1.1.0") (casual-symbol-overlay "1.0.1") (magit "4.0.0") (transpose-frame "0.2.1"))

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

;; Casual EditKit is a Transient user interface toolkit for Emacs editing.

;; INSTALLATION
;; (require 'casual-editkit) ;; optional
;; (keymap-global-set "C-o" #'casual-editkit-main-tmenu)

;; Alternately with `use-package':
;; (use-package casual-editkit
;;   :ensure nil
;;   :bind (("C-o" . casual-editkit-main-tmenu)))

;; Alternate bindings to consider are "M-o" and "F10". Choose whatever binding
;; best suits you.

;; NOTE: This package requires `casual-lib' which in turn requires an update of
;; the built-in package `transient' ≥ 0.6.0. Please customize the variable
;; `package-install-upgrade-built-in' to t to allow for `transient' to be
;; updated. For further details, consult the INSTALL section of this package's
;; README.

;;; Code:
(require 'casual-editkit-utils)
(require 'casual-editkit-settings)

;;;###autoload (autoload 'casual-editkit-main-tmenu "casual-editkit" nil t)
(transient-define-prefix casual-editkit-main-tmenu ()
  "Reference main menu for Casual EditKit.

This prefix is intended to be a reference model that employs the
different Transient prefixes (menus) provided by Casual EditKit.
It can be used as-is or serve as a template for building a
user-customized menu."
  [["File"
    ("o" "Open›" casual-editkit-open-tmenu)
    ("f" "Open file…" find-file)
    ("d" "Open in Dired" dired-jump-other-window
     :if (lambda () (buffer-file-name)))
    ("b" "List Buffers" ibuffer)
    ("R" "Recent Files" recentf-open-files)
    ("s" "Save" save-buffer)]

   ["Edit"
    :pad-keys t
    ("e" "Edit›" casual-editkit-edit-tmenu)
    ("p" "Fill Paragraph" fill-paragraph
     :if-not casual-editkit-buffer-read-only-p)
    ("l" "Join line" join-line
     :transient nil
     :if-not casual-editkit-buffer-read-only-p)
    ("C-o" "Open line" open-line
     :transient t
     :if-not casual-editkit-buffer-read-only-p)
    ("E" "Emoji & Symbols›" casual-editkit-emoji-symbols-tmenu
     :if-not casual-editkit-buffer-read-only-p)]

   ["Sexp"
    ("m" "Mark" mark-sexp)
    ("c" "Copy" casual-editkit-copy-sexp)
    ("k" "Kill (Cut)" kill-sexp
     :if-not casual-editkit-buffer-read-only-p)
    ("t" "Transpose" transpose-sexps
     :if-not casual-editkit-buffer-read-only-p)]

   ["Tools"
    ("T" "Tools›" casual-editkit-tools-tmenu)
    ("a" "Org Agenda" org-agenda)
    ("C" "Compile…" compile)
    ("g" "Magit Status" casual-editkit-select-magit-command
     :description casual-editkit-select-magit-command-description
     :if casual-editkit-version-controlled-p)
    ("h" "Highlight Symbol" symbol-overlay-put)]]

  [[;;"Bookmarks"
    ("B" "Bookmarks›" casual-editkit-bookmarks-tmenu)
    ("J" "Jump to Bookmark…" bookmark-jump)]

   [;;"Window"
    ("w" "Window›" casual-editkit-windows-tmenu)
    ("M-n" "New Frame" make-frame-command)]

   [;;"Search/Replace"
    ("/" "Search/Replace›" casual-editkit-search-tmenu)
    ("P" "Project›" casual-editkit-project-tmenu)]

   [("M" "Macros›" casual-editkit-macro-tmenu)]]

  ;; casual-editkit-cursor-navigation-group

  [:class transient-row
   (casual-lib-quit-one)
   ("r" "Registers›" casual-editkit-registers-tmenu)
   ("U" "Undo" undo :transient t)
   ("," "Settings›" casual-editkit-settings-tmenu)
   (casual-lib-quit-all)

   ("x" "Exit Emacs" save-buffers-kill-emacs)])

(provide 'casual-editkit)
;;; casual-editkit.el ends here
