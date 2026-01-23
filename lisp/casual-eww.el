;;; casual-eww.el --- Transient UI for EWW -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for EWW, a web browser
;; for GNU Emacs.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-eww-tmenu' to your
;; key binding of preference.

;; (require 'casual-eww) ; optional if using autoloaded menu
;; (keymap-set eww-mode-map "C-o" #'casual-eww-tmenu)

;; Do so similarly to access `casual-eww-bookmarks-tmenu' in the EWW bookmarks
;; list.

;; (keymap-set eww-bookmark-mode-map "C-o" #'casual-eww-bookmarks-tmenu)

;; While not mandatory, the following bindings can make the EWW keymaps
;; consistent with those used by Casual.

;; (keymap-set eww-mode-map "C-o" #'casual-eww-tmenu)
;; (keymap-set eww-mode-map "C-c C-o" #'eww-browse-with-external-browser)
;; (keymap-set eww-mode-map "j" #'shr-next-link)
;; (keymap-set eww-mode-map "k" #'shr-previous-link)
;; (keymap-set eww-mode-map "[" #'eww-previous-url)
;; (keymap-set eww-mode-map "]" #'eww-next-url)
;; (keymap-set eww-mode-map "M-]" #'eww-forward-url)
;; (keymap-set eww-mode-map "M-[" #'eww-back-url)
;; (keymap-set eww-mode-map "n" #'casual-lib-browse-forward-paragraph)
;; (keymap-set eww-mode-map "p" #'casual-lib-browse-backward-paragraph)
;; (keymap-set eww-mode-map "P" #'casual-eww-backward-paragraph-link)
;; (keymap-set eww-mode-map "N" #'casual-eww-forward-paragraph-link)
;; (keymap-set eww-mode-map "M-l" #'eww)
;; (keymap-set eww-bookmark-mode-map "C-o" #'casual-eww-bookmarks-tmenu)
;; (keymap-set eww-bookmark-mode-map "p" #'previous-line)
;; (keymap-set eww-bookmark-mode-map "n" #'next-line)
;; (keymap-set eww-bookmark-mode-map "<double-mouse-1>" #'eww-bookmark-browse)

;;; Code:
(require 'bookmark)
(require 'casual-eww-settings)
(require 'casual-eww-utils)

;;;###autoload (autoload 'casual-eww-tmenu "casual-eww" nil t)
(transient-define-prefix casual-eww-tmenu ()
  "Transient menu for EWW.

EWW (Emacs Web Wowser) is a web browser for GNU Emacs.

See Info node `(eww)Top' for more information on it."
   :refresh-suffixes t
   ["Casual EWW"
    ["History"
     :pad-keys t
     ("M-[" "❬" eww-back-url
      :description (lambda () (casual-eww-unicode-get :history-back))
      :transient t)
     ("M-]" "❭" eww-forward-url
      :description (lambda () (casual-eww-unicode-get :history-forward))
      :transient t)
     ("H" "History" eww-list-histories
      :description (lambda () (casual-eww-unicode-get :history))
      :transient nil)]

    ["Document"
     ("[" "←" eww-previous-url
      :description (lambda () (casual-eww-unicode-get :back))
      :transient t)
     ("]" "→" eww-next-url
      :description (lambda () (casual-eww-unicode-get :forward))
      :transient t)
     ("^" "↑" eww-up-url
      :description (lambda () (casual-eww-unicode-get :up))
      :transient t)
     ("t" "⤒" eww-top-url
      :description (lambda () (casual-eww-unicode-get :top))
      :transient t)]

    ["Navigate"
     :pad-keys t
     ("p" "↑ ¶" casual-lib-browse-backward-paragraph
      :description (lambda () (casual-eww-unicode-get :backward-paragraph))
      :transient t)
     ("n" "↓ ¶" casual-lib-browse-forward-paragraph
      :description (lambda () (casual-eww-unicode-get :forward-paragraph))
      :transient t)
     ("SPC" "↓ 📄" scroll-up-command
      :description (lambda () (casual-eww-unicode-get :scroll-up))
      :transient t)
     ("S-SPC" "↑ 📄" scroll-down-command
      :description (lambda () (casual-eww-unicode-get :scroll-down))
      :transient t)]

    ["🔗"
     :description (lambda () (casual-eww-unicode-get :link))
     :pad-keys t
     ("k" "↑" shr-previous-link
      :description (lambda () (casual-eww-unicode-get :previous))
      :transient t)
     ("j" "↓" shr-next-link
      :description (lambda () (casual-eww-unicode-get :next))
      :transient t)
     ("RET" "🚀" eww-follow-link
      :description (lambda () (casual-eww-unicode-get :follow))
      :transient t)]

    ["Misc"
     :pad-keys t
     ("D" "Display›" casual-eww-display-tmenu)
     ("R" "Readable" eww-readable)]]

   ["URL"
    :pad-keys t
    [("M-l" "Open…" eww)
     ("&" "Open External…" eww-browse-with-external-browser)]
    [("c" "Copy" eww-copy-page-url)
     ("A" "Copy Alt" eww-copy-alternate-url)]
    [("d" "Download" eww-download)
     ("g" "Reload" eww-reload
      :description (lambda () (casual-eww-unicode-get :reload)))]]

   ["EWW Bookmarks"
    :class transient-row
    ("b" "Add" eww-add-bookmark)
    ("B" "List" eww-list-bookmarks)
    ("M-n" "Next" eww-next-bookmark :transient t)
    ("M-p" "Previous" eww-previous-bookmark :transient t)]

   [:class transient-row
    (casual-lib-quit-one)
    ("," "Settings›" casual-eww-settings-tmenu)
    ("I" "ⓘ" casual-eww-info)
    ("J" "Jump to Bookmark…" bookmark-jump)
    ("q" "Quit" quit-window)
    (casual-lib-quit-all)])

;;;###autoload (autoload 'casual-eww-bookmarks-tmenu "casual-eww" nil t)
(transient-define-prefix casual-eww-bookmarks-tmenu ()
  "Transient menu for EWW bookmarks."
  :refresh-suffixes t

  ["Casual EWW Bookmarks"
   ["Bookmark"
    :pad-keys t
    ("k" "Kill" eww-bookmark-kill :transient t)
    ("y" "Yank" eww-bookmark-yank :transient t)
    ("RET" "Browse" eww-bookmark-browse)]

   ["Navigate"
    ("p" "Previous" previous-line :transient t)
    ("n" "Next" next-line :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("q" "Quit" quit-window)
   (casual-lib-quit-all)])

(provide 'casual-eww)
;;; casual-eww.el ends here
