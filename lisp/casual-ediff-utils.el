;;; casual-ediff-utils.el --- Casual Eshell Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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

(require 'ediff)
(require 'casual-lib)

;; these defvars are here to let cc-ediff-mode.el compile clean
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-merge-job)
(defvar ediff-ancestor-buffer)

;; CC: I set my Ediff variables in `custom-set-variables'
;; Use your own preference.
;; '(ediff-keep-variants nil)
;; '(ediff-split-window-function 'split-window-horizontally)
;; '(ediff-window-setup-function 'ediff-setup-windows-plain)

(defconst casual-ediff-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:scroll-to-right . '("↤" "Scroll to right"))
    (:scroll-to-left . '("↦" "Scroll to left"))
    (:refresh . '("⟲" "Refresh")))

  "Unicode symbol DB to use for Ediff Transient menus.")

(defun casual-ediff-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-ediff-unicode-db))


;; -------------------------------------------------------------------
;;; Format

(defun casual-ediff--buffer-description (ebuf slot &optional extend)
  "Format name of buffer EBUF in SLOT with option to EXTEND."
  (let ((bufname (buffer-name ebuf))
        (fwidth (frame-width)))
    (casual-ediff--display-filename bufname slot extend fwidth)))

(defun casual-ediff--display-filename (filename slot extend fwidth)
  "Reformat FILENAME for Ediff display using SLOT, EXTEND, and FWIDTH."
  (if (and (string-match "~\\([[:xdigit:]]*\\)~$" filename) (< fwidth 95))
      (let* ((commit-hash (match-string 1 filename))
             (short-hash (truncate-string-to-width commit-hash 7 0 nil t))
             (filename (string-replace commit-hash short-hash filename)))
        (format "%s: %s" slot filename))
    (if extend
        (let* ((bwidth (- (/ fwidth 2) 6))
               (ext-fmt (concat "%s: " (format "%%-%ss" bwidth))))
          (format ext-fmt slot filename))
      (format "%s: %s" slot filename))))


;; -------------------------------------------------------------------
;;; Ediff Functions
(defun casual-ediff-info ()
  "Open Info for Ediff."
  (interactive) (info "(ediff) Top"))

(defvar casual-ediff--revision-session-p nil
  "If t then `casual-ediff--internal-last-revision' has been called.
This state variable is used to insert added behavior to the advised
function `ediff-janitor'.")

(defvar casual-ediff--installed-p nil
  "If t then Casual Ediff is initialized.")

(defun casual-ediff-revision-from-menu (e)
  "Invoke `casual-ediff-revision' on E with variable `buffer-file-name'."
  (interactive "e")
  (ignore e)
  (casual-ediff-revision))

(defun casual-ediff-revision ()
  "Run Ediff comparing current file with last committed version.

This function handles the interactive concerns found in
`ediff-revision'. This function will also test if a diff should apply to
the current buffer."
  (interactive)
  (when (and (bound-and-true-p buffer-file-name)
             (vc-registered (buffer-file-name)))
    (if (and (buffer-modified-p)
	     (y-or-n-p (format "Buffer %s is modified.  Save buffer? "
                               (buffer-name))))
      (save-buffer (current-buffer)))
    (message buffer-file-name)
    (casual-ediff--internal-last-revision))

  (cond ((not (bound-and-true-p buffer-file-name))
         (message (concat (buffer-name) " is not a file that can be diffed.")))
        ((not (vc-registered buffer-file-name))
         (message (concat buffer-file-name " is not under version control.")))))

(defun casual-ediff--internal-last-revision ()
  "Implementation of Ediff comparing current file with last committed version.

This function handles the actual diff behavior called by `ediff-revision'."
  (let ((rev1 "")
        (rev2 ""))
    (setq casual-ediff--revision-session-p t)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 nil)))

(defun casual-ediff-janitor (ask keep-variants)
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is not deleted here, but rather according to
`ediff-quit-merge-hook'.
ASK non-nil means ask the user whether to keep each unmodified buffer, unless
KEEP-VARIANTS is non-nil, in which case buffers are never killed.
A side effect of cleaning up may be that you should be careful when comparing
the same buffer in two separate Ediff sessions: quitting one of them might
delete this buffer in another session as well.

!!!: This method overrides the original Ediff function."
  (let ((ask (if (and (boundp 'casual-ediff--revision-session-p)
                      casual-ediff--revision-session-p)
                 nil
               ask)))
    (ediff-dispose-of-variant-according-to-user
     ediff-buffer-A 'A ask keep-variants)
    ;; !!!: Test global state variable `casual-ediff--revision-session-p' to
    ;; determine if the modified repo file should be kept.
    ;; Guarding in place to hopefully avoid side-effects when `ediff-janitor' is
    ;; called from other Ediff functions. Informal testing has not revealed any
    ;; side-effects but YOLO.
    (if (and (boundp 'casual-ediff--revision-session-p)
             casual-ediff--revision-session-p)
        (ediff-dispose-of-variant-according-to-user
         ;; CC Note: keep-variants argument is hard-coded to t to keep
         ;; buffer holding modified repo file around.
         ediff-buffer-B 'B t t)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-B 'B ask keep-variants))
    (if ediff-merge-job  ; don't del buf C if merging--del ancestor buf instead
        (ediff-dispose-of-variant-according-to-user
         ediff-ancestor-buffer 'Ancestor ask keep-variants)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-C 'C ask keep-variants))
    ;; CC Note: Reset global state variable `casual-ediff--revision-session-p'.
    (if (and (boundp 'casual-ediff--revision-session-p)
             casual-ediff--revision-session-p)
        (setq casual-ediff--revision-session-p nil))))


(defun casual-ediff--stash-window-configuration-for-ediff ()
  "Store window configuration to register 🧊.

Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (window-configuration-to-register ?🧊))

(defun casual-ediff--restore-window-configuration-for-ediff ()
  "Restore window configuration from register 🧊.

Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (jump-to-register ?🧊))

(defun casual-ediff--restore-and-save-diff (key)
  "Restore diff and save Ediff buffer referenced by KEY."
  (ediff-restore-diff nil key)
  (casual-ediff--save-buffer key))

(defun casual-ediff--save-buffer (key)
  "Save Ediff buffer referenced by KEY."
  (setq last-command-event key)
  (ediff-save-buffer nil))

(defun casual-ediff--buffer-read-only-p (buf)
  "Predicate to test if BUF is read-only."
  (with-current-buffer buf
    (if buffer-read-only t nil)))

(defun casual-ediff--split-window-vertically-description ()
  "Provide string label for state of `split-window-vertically'."
  (if (eq ediff-split-window-function 'split-window-vertically)
      "Side by side"
    "On top"))

;; The implementations of `casual-ediff-copy-AB-to-C' and
;; `casual-ediff-copy-BA-to-C' are adapted from code written by killdash9 from
;; the following Stack Overflow post.
;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750

(defun casual-ediff-copy-AB-to-C ()
  "Resolve merge conflict by inserting difference from buffer A then buffer B."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents
     ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents
     ediff-current-difference 'B ediff-control-buffer))))

(defun casual-ediff-copy-BA-to-C ()
  "Resolve merge conflict by inserting difference from buffer B then buffer A."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents
     ediff-current-difference 'B ediff-control-buffer)
    (ediff-get-region-contents
     ediff-current-difference 'A ediff-control-buffer))))

(provide 'casual-ediff-utils)
;;; casual-ediff-utils.el ends here
