;;; test-casual-image.el --- Casual Image Tests      -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'casual-image-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-image)

(ert-deftest test-casual-image-tmenu ()
  (let ((tmpfile "casual-image-tmenu.txt"))
    (casualt-image-setup)
    (cl-letf (((symbol-function #'casual-image--identify-label) (lambda () "some image info"))
              (casualt-mock #'image-increase-size)
              (casualt-mock #'image-decrease-size)
              (casualt-mock #'image-transform-reset-to-original)
              (casualt-mock #'image-transform-fit-to-window)
              (casualt-mock #'image-transform-set-rotation)
              (casualt-mock #'image-transform-set-percent)
              (casualt-mock #'casual-image--reset-point)

              (casualt-mock #'image-crop)
              (casualt-mock #'image-cut)
              (casualt-mock #'save-buffer)
              (casualt-mock #'image-save)
              (casualt-mock #'rename-visited-file)
              (casualt-mock #'revert-buffer)

              (casualt-mock #'image-previous-line)
              (casualt-mock #'image-next-line)
              (casualt-mock #'image-backward-hscroll)
              (casualt-mock #'image-forward-hscroll)

              (casualt-mock #'image-bol)
              (casualt-mock #'image-eol)
              (casualt-mock #'image-bob)
              (casualt-mock #'image-eob)

              (casualt-mock #'image-previous-file)
              (casualt-mock #'image-next-file)
              (casualt-mock #'dired-jump-other-window)

              (casualt-mock #'image-mode-mark-file)
              (casualt-mock #'image-mode-unmark-file)
              (casualt-mock #'image-mode-copy-file-name-as-kill))

      (let ((test-vectors
             '((:binding "+" :command image-increase-size)
               (:binding "-" :command image-decrease-size)
               (:binding "o" :command image-transform-reset-to-original)
               (:binding "=" :command image-transform-fit-to-window)
               (:binding "R0" :command image-transform-set-rotation)
               (:binding "%50" :command image-transform-set-percent)
               (:binding "." :command casual-image--reset-point)

               (:binding "c" :command image-crop)
               (:binding "f" :command image-cut)
               (:binding "F" :command casual-image--customize-image-cut-color)
               (:binding "r" :command casual-image-resize-tmenu)
               ;; (:binding "s" :command save-buffer) ; TODO: handle buffer-modified-p
               (:binding "C-s" :command image-save)
               (:binding "M-r" :command rename-visited-file)
               (:binding "g" :command revert-buffer)


               (:binding "<up>" :command image-previous-line)
               (:binding "<down>" :command image-next-line)
               (:binding "<left>" :command image-backward-hscroll)
               (:binding "<right>" :command image-forward-hscroll)

               (:binding "a" :command image-bol)
               (:binding "e" :command image-eol)
               (:binding "<" :command image-bob)
               (:binding ">" :command image-eob)

               (:binding "p" :command image-previous-file)
               (:binding "n" :command image-next-file)
               (:binding "d" :command dired-jump-other-window)

               (:binding "m" :command image-mode-mark-file)
               (:binding "u" :command image-mode-unmark-file)

               (:binding "w" :command image-mode-copy-file-name-as-kill)

               (:binding "I" :command casual-image--indentify-verbose)
               (:binding "," :command casual-image-settings-tmenu))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-image-tmenu
                                        '(lambda () (random 5000)))))

    (cl-letf (((symbol-function #'casual-image--identify-label) (lambda () "some image info"))
              ((symbol-function #'buffer-modified-p) (lambda () t))
              (casualt-mock #'save-buffer))

      (let ((test-vectors
             '((:binding "s" :command save-buffer))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-image-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-image-breakdown)))



(provide 'test-casual-image)
;;; test-casual-image.el ends here
