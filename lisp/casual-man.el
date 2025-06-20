;;; casual-man.el --- Transient UI for Man -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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

;; This library provides a Transient-based user interface for `Man-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-man-tmenu' to your
;; key binding of preference.

;; (require 'casual-man) ; optional if using autoloaded menu
;; (keymap-set Man-mode-map "C-o" #'casual-man-tmenu)

;; `casual-man-tmenu' uses bindings that are consistent with Casual Dired and
;; IBuffer by using "[" and "]" for section navigation.

;; The following keybindings are recommended to support consistent behavior
;; between `Man-mode' and `casual-man-tmenu'.

;; (keymap-set Man-mode-map "n" #'casual-lib-browse-forward-paragraph)
;; (keymap-set Man-mode-map "p" #'casual-lib-browse-backward-paragraph)
;; (keymap-set Man-mode-map "[" #'Man-previous-section)
;; (keymap-set Man-mode-map "]" #'Man-next-section)
;; (keymap-set Man-mode-map "j" #'next-line)
;; (keymap-set Man-mode-map "k" #'previous-line)
;; (keymap-set Man-mode-map "K" #'Man-kill)
;; (keymap-set Man-mode-map "o" #'casual-man-occur-options)

;;; Code:
(require 'bookmark)
(require 'casual-man-settings)
(require 'casual-man-utils)

;;;###autoload (autoload 'casual-man-tmenu "casual-man" nil t)
(transient-define-prefix casual-man-tmenu ()
  "Casual Man page main menu."
  ["Casual Man"
   :description (lambda () (format "Casual Man: %s" Man-page-mode-string))
   ["Navigation"
    :pad-keys t
    ("." "Beginning" beginning-of-buffer
     :description (lambda () (casual-man-unicode-get :beginning-of-buffer))
     :transient t)
    (">" "End" end-of-buffer
     :description (lambda () (casual-man-unicode-get :end-of-buffer))
     :transient t)
    ("C-p" "Previous" previous-line
     :description (lambda () (casual-man-unicode-get :previous))
     :transient t)
    ("C-n" "Next" next-line
     :description (lambda () (casual-man-unicode-get :next))
     :transient t)
    ("o" "Options" casual-man-occur-options)]

   ["Paragraph"
    :description (lambda () (casual-man-unicode-get :paragraph))
    ("p" "Backward" casual-lib-browse-backward-paragraph
     :description (lambda () (casual-man-unicode-get :previous))
     :transient t)
    ("n" "Forward" casual-lib-browse-forward-paragraph
     :description (lambda () (casual-man-unicode-get :next))
     :transient t)]

   ["Section"
    ("[" "Previous" Man-previous-section
     :description (lambda () (casual-man-unicode-get :previous))
     :transient t)
    ("]" "Next" Man-next-section
     :description (lambda () (casual-man-unicode-get :next))
     :transient t)
    ("g" "Goto…" Man-goto-section
     :description (lambda () (casual-man-unicode-get :goto))
     :transient t)
    ("s" "See Also" Man-goto-see-also-section
     :description (lambda () (casual-man-unicode-get :see-also))
     :transient t)]

   ["Link"
    ("r" "Follow" Man-follow-manual-reference
     :description (lambda () (casual-man-unicode-get :follow)))]

   ["Page"
    :pad-keys t
    :if (lambda () (> (length Man-page-list) 1))
    ("M-p" "Previous" Man-previous-manpage
     :description (lambda () (casual-man-unicode-get :previous))
     :transient t)
    ("M-n" "Next" Man-next-manpage
     :description (lambda () (casual-man-unicode-get :next))
     :transient t)
    ("j" "Goto…" Man-goto-page
     :description (lambda () (casual-man-unicode-get :goto))
     :transient t)]]

  ["Bookmarks"
   :class transient-row
   ("B" "Add…" bookmark-set-no-overwrite)
   ("J" "Jump…" bookmark-jump)]

  [:class transient-row
   (casual-lib-quit-one)
   ("m" "Man…" man)
   ("u" "Update" Man-update-manpage
    :description (lambda () (casual-man-unicode-get :update)))
   ("," "Settings" casual-man-settings-tmenu)
   ("I" "ⓘ" casual-man-info)
   ("K" "Close" Man-kill
    :description (lambda () (casual-man-unicode-get :kill)))
   ("q" "Quit" quit-window)
   (casual-lib-quit-all)])

(provide 'casual-man)
;;; casual-man.el ends here
