;;; casual-calc-trail.el --- Casual Trail Menu            -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'calc)
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-trail-tmenu ()
  "Casual trail menu."
  ["Trail Commands"
   ["Display"
    ("d" "Toggle Display" calc-trail-display :transient t)
    ("t" "Focus to Trail" calc-trail-in :transient t)
    ("c" "Focus to Calc" calc-trail-out :transient t)
    ("<" "Scroll Left" calc-trail-scroll-left :transient t)
    (">" "Scroll Right" calc-trail-scroll-right :transient t)]

   ["Navigation"
    ("p" "Previous" calc-trail-previous :transient t)
    ("n" "Next" calc-trail-next :transient t)
    ("[" "First" calc-trail-first :transient t)
    ("]" "Last" calc-trail-last :transient t)]

   ["Search & Editing"
    ("m" "Insert Marker…" calc-trail-marker :transient t)
    ("C-r" "isearch-backward" calc-trail-isearch-backward :transient t)
    ("C-s" "isearch-forward" calc-trail-isearch-forward :transient t)
    ("y" "Yank to Stack" calc-trail-yank :transient t)
    ("k" "Kill Selected" calc-trail-kill :transient t)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("s" "Save Settings" calc-save-modes :transient t)
          (casual-lib-quit-all)])

(provide 'casual-calc-trail)
;;; casual-calc-trail.el ends here
