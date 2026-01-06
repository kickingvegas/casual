;;; casual-html.el --- Transient UI for HTML mode -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for `html-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-html-tmenu' to your
;; key binding of preference.

;; (require 'casual-html) ; optional if using autoloaded menu
;; (keymap-set html-mode-map "M-m" #'casual-html-tmenu)

;; A menu dedicated to HTML tags, `casual-html-tags-tmenu', can also be bound.

;; (keymap-set html-mode-map "C-c m" #'casual-html-tags-tmenu)

;; Users desiring to use Tree-sitter HTML mode should use the keymap
;; `html-ts-mode-map' instead of `html-mode-map'.

;; Note that the menu item for `sgml-delete-tag' is omitted when `html-ts-mode'
;; is enabled.

;;; Code:
(require 'casual-html-settings)
(require 'casual-html-utils)

;;;###autoload (autoload 'casual-html-tmenu "casual-html" nil t)
(transient-define-prefix casual-html-tmenu ()
  "Main Transient menu for Casual HTML.

This menu serves as the entry point for accessing commands provided by
`sgml-mode' and modes which derive from it (`html-mode', `mhtml-mode',
`html-ts-mode').

This menu supports Tree-sitter for HTML. If `html-ts-mode' is enabled,
the menu item for `smgl-delete-tag' is omitted as it does not have
support for Tree-sitter.

For more documentation, refer to the following links:
- Info node `(casual) HTML'
- Info node `(emacs) HTML Mode'"
  :refresh-suffixes t
  ["Casual HTML"
   :description (lambda () (if (derived-mode-p 'html-ts-mode)
                          "Casual HTML (Tree-sitter)"
                        "Casual HTML"))

   ["</>"
    :inapt-if (lambda () (if buffer-read-only t nil))
    ("i" "Insert…" sgml-tag)
    ("a" "Attribute(s)…" sgml-attributes)
    ("c" "Close" sgml-close-tag)
    ("d" "Delete" sgml-delete-tag
     :if-not (lambda () (derived-mode-p 'html-ts-mode)))]

   ["HTML"
    :if (lambda () (derived-mode-p 'html-mode))
    ("h" "HTML›" casual-html-tags-tmenu)]

   ["Misc"
    :pad-keys t
    ("b" "Browse" browse-url-of-buffer
     :if (lambda () (derived-mode-p 'html-mode)))
    ("v" "Validate" sgml-validate)
    ("A" "Autoview" html-autoview-mode
     :description (lambda () (casual-lib-checkbox-label html-autoview-mode
                                                   "HTML Autoview"))
     :if (lambda () (derived-mode-p 'html-mode)))
    ("TAB" "Toggle Invisible Tags" sgml-tags-invisible
     :description (lambda () (casual-lib-checkbox-label sgml-tags-invisible
                                             "Invisible Tags"))
     :transient t)]

   ["Navigation"
    ("[" "|< >" sgml-skip-tag-backward :transient t)
    ("]" " </>|" sgml-skip-tag-forward :transient t)]
   [""
    ("C-b" "←" backward-char :transient t)
    ("C-f" "→" forward-char :transient t)]
   [""
    ("C-p" "↑" previous-line :transient t)
    ("C-n" "↓" next-line :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-html-settings-tmenu)
   ("I" "ⓘ" casual-html-info)
   ("RET" "Dismiss" transient-quit-all)
   (casual-lib-quit-all)])


;;;###autoload (autoload 'casual-html-tags-tmenu "casual-html" nil t)
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

(provide 'casual-html)
;;; casual-html.el ends here
