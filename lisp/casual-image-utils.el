;;; casual-image-utils.el --- Casual Image Utils     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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

(require 'image)
(require 'image-crop)
(require 'casual-lib)

(defconst casual-image-unicode-db
  '((:rotate . '("Rotate ⟳90°𝑥…" "Rotate Clockwise 90°x…"))
    (:scroll-up . '("↑" "Up"))
    (:scroll-down . '("↓" "Down"))
    (:scroll-left . '("←" "Left"))
    (:scroll-right . '("→" "Right"))
    (:edge-left . '("⇤" "Left"))
    (:edge-right . '("⇥" "Right"))
    (:top-left . '("⇱" "Top-left"))
    (:bottom-right . '("⇲" "Bottom-right"))
    (:previous-image . '("↑🌇" "Previous Image"))
    (:next-image . '("↓🌇" "Next Image"))
    (:dired . '("🗄️" "Dired"))
    (:mark-image . '("Mark 🌇" "Mark Image"))
    (:unmark-image . '("Unmark 🌇" "Unmark Image")))
  "Unicode symbol DB to use for Image Transient menus.")

(defun casual-image-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode' is
non-nil, then the Unicode symbol is returned, otherwise a plain
ASCII-range string."
  (casual-lib-unicode-db-get key casual-image-unicode-db))

(defun casual-image--resize ()
  "Resize image to specified geometry.

This function resizes an image to a specified geometry.
ImageMagick (man page `magick') is used to implement the resizing
of an image.

This function is intended to only be used as part of a Transient suffix.

Transient infix arguments supported by this function include:

 --geometry=<value>    ImageMagick geometry specifier
 --as                  If enabled, output to another file
 --type=<value>        Resize type (legal values: nil, adaptive, interpolative)

Refer to the
URL `https://imagemagick.org/script/command-line-processing.php#geometry'
for details on ImageMagick geometry specification.

If the argument ‘--as’ is enabled, the user will be prompted to
specify a file to store the resized output. Otherwise, the
original file itself will be *irreversibly* modified with the
resized version. Please take note.

If ‘--type=’ is not defined, then the basic resize feature of
ImageMagick will be invoked. Two other options are to use
‘adaptive’ or ‘interpolative’ resizing. Refer to the ImageMagick
documentation for more details on

References
• URL `https://imagemagick.org/script/command-line-options.php#resize'
• URL `https://imagemagick.org/script/command-line-options.php#adaptive-resize'
• URL `https://imagemagick.org/script/command-line-options.php#interpolative-resize'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (geometry (transient-arg-value "--geometry=" current-command))
         (resize-type (if (transient-arg-value "--type=" current-command)
                          (transient-arg-value "--type=" current-command)
                        "resize"))
         (as (if (transient-arg-value "--as" current-command) t nil))
         (target (if as
                     (format
                      "'%s'"
                      (file-truename
                       (read-file-name
                        "Target File: " nil nil nil
                        (casual-image--resized-filename
                         (buffer-file-name) resize-type geometry))))
                   (format "'%s'" (buffer-file-name))))
         (source (if as
                     (format "'%s'" (buffer-file-name))
                   nil))
         (cmd-list (list)))

    ;; 6: convert source -resize geometry target
    ;; 7: magick source -resize geometry target
    ;; 6: mogrify -resize geometry target
    ;; 7: magick mogrify -resize geometry target

    (if (executable-find "magick")
        (push "magick" cmd-list)
      (if as
          (push "convert" cmd-list)))

    (if as
        (push source cmd-list)
      (push "mogrify" cmd-list))

    (cond
     ((string= resize-type "adaptive") (push "-adaptive-resize" cmd-list))
     ((string= resize-type "interpolative") (push "-interpolative-resize" cmd-list))
     (t (push "-resize" cmd-list)))

    (push geometry cmd-list)
    (push target cmd-list)

    (let ((cmd (string-join (reverse cmd-list) " ")))
      (async-shell-command cmd)
      (message "%s" cmd))))


(defun casual-image--reset-point ()
  "Reset point to point to image.

Use this command for the condition where an `image-mode' command
fails to work on an image due to improper point focus. This occurs when
the message “No recognizable image under point” is raised."
  (interactive)
  (goto-char (point-min)))

(defun casual-image--indentify-verbose ()
  "Identify image verbosely.

Invokes ImageMagick command ‘identify -verbose’ to show details
of current image."
  (interactive)
  (let* ((cmd-list (list)))
    ;;(push "magick" cmd-list)
    (push "identify" cmd-list)
    (push "-verbose" cmd-list)
    (push (format "'%s'" (buffer-file-name)) cmd-list)

    (let ((cmd (string-join (reverse cmd-list) " ")))
      (async-shell-command cmd)
      (message "%s" cmd))))

(defun casual-image--resized-filename (filename resize-type modifier)
  "Generate target FILENAME with RESIZE-TYPE and MODIFIER."
  ;; TODO: sanitize geometry specifier
  (let* ((base (file-name-base filename))
         (extension (file-name-extension filename))
         (modifier (string-replace "%" "pct" modifier)))
    (concat base "_" resize-type "_" modifier "." extension)))

(defun casual-image--customize-image-cut-color ()
  "Customize variable `image-cut-color'.

This variable is poorly named as it applies to a fill operation."
  (interactive)
  (customize-variable 'image-cut-color))

(defun casual-image--identify-label ()
  "Generate label string with ImageMagick identify information."
  (let ((cmd-list (list)))
    (push "identify" cmd-list)
    (push "-format" cmd-list)
    ;; %W×%H%X%Y
    (push "'[%f] %m %w×%h %BB %[bit-depth]-bit %[colorspace]'" cmd-list)
    (push (format "'%s'" (buffer-file-name)) cmd-list)

    (shell-command-to-string (string-join (reverse cmd-list) " "))))

;; Transients
(transient-define-prefix casual-image-resize-tmenu ()
  "Casual Image Resize Menu.

Menu resizing an image."
  :value '("--geometry=100%" "--as")
  ["Resize"
   :description (lambda () (format "Resize: %s" (casual-image--identify-label)))

   ["Options"
    ("g" "Geometry" "--geometry="
    :always-read t
    :allow-empty nil
    :summary "ImageMagick geometry specifier."
    :prompt "Geometry: ")
    ("o" "Output to another file" "--as"
     :summary "If enabled, then specify output file.")

    ("t" "Type" "--type="
     :summary "Select resize type. If not set, uses standard resize."
     :choices ("adaptive" "interpolative"))]]

  ["Command"
   ("r" "Resize" casual-image--resize :transient t)]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(provide 'casual-image-utils)
;;; casual-image-utils.el ends here
