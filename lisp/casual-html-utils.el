;;; casual-html-utils.el --- Casual HTML Utils -*- lexical-binding: t; -*-

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
;;

;;; Code:
(require 'sgml-mode)
(require 'casual-lib)

(defconst casual-html-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next")))
  "Unicode symbol DB to use for HTML Transient menus.")

(defun casual-html-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-html-unicode-db))

(defun casual-html-info ()
  "Open Info for Emacs HTML mode."
  (interactive) (info "(emacs) HTML Mode"))


;; -------------------------------------------------------------------
;; Transients
;; TODO: move to support autoload
(transient-define-prefix casual-html-tags-tmenu ()
  "Transient menu for HTML tags.

This menu provides an interface to HTML-specific commands provided by
`html-mode' and `mhtml-mode' (see Info node `(emacs) HTML Mode')."
  :refresh-suffixes t
  ["Casual HTML Tags"
   :inapt-if (lambda () (if buffer-read-only t nil))
   ["Content"
    :pad-keys t
    ("p" "Paragraph" html-paragraph)
    ("i" "Image…" html-image)
    ("b" "Line Break" html-line)
    ("hr" "Horizontal Rule" html-horizontal-rule)
    ("d" "div" html-div)]

   ["Anchor"
    ("aa" "Anchor…" html-href-anchor)
    ("af" "File…" html-href-anchor-file)
    ("ai" "ID…" html-id-anchor)
    ("an" "Name…" html-name-anchor)]

   ["Format"
    ("fb" "Bold" facemenu-set-bold)
    ("fi" "Italic" facemenu-set-italic)
    ("fl" "Bold Italic" facemenu-set-bold-italic)
    ("fu" "Underline" facemenu-set-underline)
    ("fd" "Default" facemenu-set-default)
    ("ff" "Face…" facemenu-set-face)]

   ["List"
    ("u" "Unordered" html-unordered-list)
    ("o" "Ordered" html-ordered-list)
    ("l" "List Item" html-list-item)
    ("r" "Radio Buttons…" html-radio-buttons)
    ("c" "Checkboxes…" html-checkboxes)]

   ["Navigation"
    ("[" "|< >" sgml-skip-tag-backward :transient t)
    ("]" " </>|" sgml-skip-tag-forward :transient t)
    ("C-b" "←" backward-char :transient t)
    ("C-f" "→" forward-char :transient t)
    ("C-p" "↑" previous-line :transient t)
    ("C-n" "↓" next-line :transient t)]]

  ["Headline"
   :inapt-if (lambda () (if buffer-read-only t nil))
   :class transient-row
   ("h1" "1" html-headline-1)
   ("h2" "2" html-headline-2)
   ("h3" "3" html-headline-3)
   ("h4" "4" html-headline-4)
   ("h5" "5" html-headline-5)
   ("h6" "6" html-headline-6)]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   (casual-lib-quit-all)])

(provide 'casual-html-utils)
;;; casual-html-utils.el ends here
