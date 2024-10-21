;;; casual-dired.el --- Transient UI for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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

;; Casual Dired is an opinionated Transient-based user interface for Emacs Dired.

;; INSTALLATION
;; (require 'casual-dired) ; optional if using autoloaded menu
;; (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
;; (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu) ; optional
;; (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu) ; optional

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'dired)
(require 'dired-x)
(require 'wdired)
(require 'image-dired)
(require 'casual-lib)
(require 'casual-dired-sort-by)

(require 'casual-dired-variables)
(require 'casual-dired-settings)
(require 'casual-dired-utils)

;;; Menus
;;;###autoload (autoload 'casual-dired-tmenu "casual-dired" nil t)
(transient-define-prefix casual-dired-tmenu ()
  "Transient menu for Dired."
  [["File"
    ("o" "Open Other" dired-find-file-other-window :transient nil)
    ("C" "Copy to…" dired-do-copy :transient t)
    ("R" "Rename…" dired-do-rename :transient t)
    ("D" "Delete…" dired-do-delete :transient t)
    ("S" "Symlink…" dired-do-symlink :transient nil)
    ("c" "Change›" casual-dired-change-tmenu :transient nil)
    ("y" "Type" dired-show-file-type :transient t)
    ("w" "Copy Name" dired-copy-filename-as-kill :transient nil)
    ("!" "Shell…" dired-do-shell-command :transient nil)
    ("&" "Shell &… " dired-do-async-shell-command :transient nil)
    (";" "Thumbnail" image-dired-dired-toggle-marked-thumbs
     :if display-graphic-p
     :transient t)
    ("W" "Browse" browse-url-of-dired-file :transient nil)]

   ["Directory"
    ("s" "Sort By›" casual-dired-sort-by-tmenu
     :if casual-dired-show-sort-by-tmenu-p
     :transient t)
    ("h" "Hide Details" dired-hide-details-mode
     :description
     (lambda ()
       (casual-lib-checkbox-label dired-hide-details-mode "Hide Details"))
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("O" "Omit Mode" dired-omit-mode
     :description
     (lambda () (casual-lib-checkbox-label dired-omit-mode "Omit Mode"))
     :transient t)
    ("i" "Insert Subdir" dired-maybe-insert-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("$" "Hide/Unhide Subdir" dired-hide-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("k" "Kill (Hide) Line(s)" dired-do-kill-lines :transient t)
    ("g" "Revert" revert-buffer :transient t)
    ("f" "Filter…" casual-dired-find-dired-regexp :transient nil)
    ("E" "Edit (wdired)" wdired-change-to-wdired-mode :transient nil)
    ("T" "Thumbnails…" image-dired :if display-graphic-p :transient n)
    ("d" "Dired…" dired :transient t)]

   ["Mark"
    ("m" "Mark" dired-mark :transient t)
    ("u" "Unmark" dired-unmark :transient t)
    ("U" "Unmark All" dired-unmark-all-marks :transient t)
    ("t" "Toggle Marks" dired-toggle-marks :transient t)
    ("~" "Flag Backups" dired-flag-backup-files :transient t)
    ("x" "Delete Flagged" dired-do-flagged-delete :transient t)
    ("r" "Regexp›" casual-dired-regexp-tmenu :transient nil)
    ("#" "Utils›" casual-dired-utils-tmenu :transient nil)]

   ["Navigation"
    :pad-keys t
    ("^" ".." dired-up-directory
     :description (lambda ()
                    (format ".. %s" (casual-dired-unicode-get :directory)))
     :transient t)

    ("p" " ↑ 📄" dired-previous-line
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :file)))
     :transient t)

    ("n" " ↓ 📄" dired-next-line
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :file)))
     :transient t)
    ("M-p" " ↑ 📁" dired-prev-dirline
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :directory)))
     :transient t)
    ("M-n" " ↓ 📁" dired-next-dirline
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :directory)))
     :transient t)
    ("[" " ↑ 🗂️" dired-prev-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :subdir)))
     :transient t)
    ("]" " ↓ 🗂️" dired-next-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :subdir)))
     :transient t)
    ("j" " → 📄…" dired-goto-file
     :description (lambda ()
                    (format "%s %s…"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :goto)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :file)))
     :transient t)
    ("M-j" " → 🗂️…" dired-goto-subdir
     :description (lambda ()
                    (format "%s %s…"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :goto)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :subdir)))
     :transient t)]]

  [["Quick"
    ("J" "Jump to Bookmark…" bookmark-jump :transient nil)
    ("B" "Add Bookmark…" bookmark-set-no-overwrite :transient nil)
    ("b" "List Buffers" ibuffer :transient nil)]

   ["Search"
    :pad-keys t
    ("C-s" "I-Search…" dired-isearch-filenames :transient nil)
    ("M-s" "I-Search Regexp…" dired-isearch-filenames-regexp :transient nil)
    ("/" "Search & Replace›" casual-dired-search-replace-tmenu :transient nil)]

   ["New"
    ("+" "Directory" dired-create-directory :transient t)
    ("F" "File" dired-create-empty-file :transient t)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("RET" "Open" dired-find-file :transient nil)
          ("," "Settings›" casual-dired-settings-tmenu :transient nil)
          ("q" "Quit Dired" quit-window)])

(transient-define-prefix casual-dired-regexp-tmenu ()
  "Transient menu for Dired mark regexp functions."
  ["Regexp Mark"
   ("m" "Files…" dired-mark-files-regexp :transient nil)
   ("c" "Files Containing…" dired-mark-files-containing-regexp :transient nil)
   ("d" "Files For Deletion…" dired-flag-files-regexp :transient nil)
   ("C" "Files To Copy…" dired-do-copy-regexp :transient nil)
   ("r" "Files To Rename…" dired-do-rename-regexp :transient nil)]
  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(transient-define-prefix casual-dired-change-tmenu ()
  ["Change"
   [("M" "Mode…" dired-do-chmod :transient t)
    ("G" "Group…" dired-do-chgrp :transient t)
    ("O" "Owner…" dired-do-chown :transient t)]
   [("T" "Touch" dired-do-touch :transient t)]]
  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

;;; Functions
(defun casual-dired-image-file-p ()
  "Predicate if current file in Dired is an image file."
  (string-match-p (image-dired--file-name-regexp) (dired-get-filename)))

(defun casual-dired-lisp-dired-buffer-p ()
  "Predicate if buffer name is “*Find Lisp Dired*”.

This buffer is created by the command `find-lisp-find-dired'."
  (and (derived-mode-p 'dired-mode)
       (string-equal (buffer-name) "*Find Lisp Dired*")))

(defun casual-dired-show-sort-by-tmenu-p ()
  "Predicate to show `casual-dired-sort-by-tmenu'."
  (and (equal dired-use-ls-dired t)
       (not (casual-dired-lisp-dired-buffer-p))))

(defun casual-dired-find-dired-regexp (REGEXP)
  "Find files in current directory whose names match REGEXP."
  (interactive "sFind filenames with regex: ")
  (find-lisp-find-dired default-directory REGEXP))

;;; Labels
(defun casual-dired-format-arrow (buf typeset)
  "If TYPESET is non-nil, then format BUF string to have space."
  (if typeset
      (format " %s" buf)
    buf))

(provide 'casual-dired)
;;; casual-dired.el ends here
