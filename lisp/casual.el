;;; casual.el --- Transient user interfaces for various modes -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools, wp
;; Version: 2.10.2-rc.1
;; Package-Requires: ((emacs "29.1") (transient "0.9.0"))

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

;; Casual is a collection of opinionated Transient-based keyboard driven user
;; interfaces for various built-in modes.

;; Casual is organized into different user interface (UI) libraries tuned for
;; different modes. Different user interfaces for the following modes are
;; supported:

;; - Agenda (Elisp library: `casual-agenda.el')
;;   An interface for Org Agenda to help you plan your day.

;; - BibTeX (Elisp library: `casual-bibtex.el')
;;   An interface for editing your BibTeX file.

;; - Bookmarks (Elisp library: `casual-bookmarks.el')
;;   An interface for editing your bookmark collection.

;; - Calc (Elisp library: `casual-calc.el')
;;   An interface for Emacs Calc, an embarrasingly feature-rich calculator.

;; - Calendar (Elisp library: `casual-calendar.el')
;;   An interface for the built-in calendar and diary of Emacs.

;; - Compile (Elisp library: `casual-compile.el')
;;   An interface for the output of the `compile' and Grep commands.

;; - Dired (Elisp library: `casual-dired.el')
;;   An interface for the venerable file manager Dired.

;; - Ediff (Elisp library: `casual-ediff.el')
;;   An interface for Ediff, a visual interface for Unix diff.

;; - EditKit (Elisp library: `casual-editkit.el')
;;   A cornucopia of interfaces for the different editing features (e.g.
;;   marking, copying, killing, duplicating, transforming, deleting) of Emacs.
;;   Included are interfaces for rectangle, register, macro, and project
;;   commands.

;; - Elisp (Elisp library: `casual-elisp.el')
;;   An interface for `emacs-lisp-mode'. It provides a menu for commands useful
;;   for Elisp development.

;; - Eshell (Elisp library: `casual-eshell.el')
;;   An interface for Eshell, a shell-like command interpreter implemented in
;;   Emacs Lisp.

;; - Help (Elisp library: `casual-help.el')
;;   An interface for `help-mode', a major mode for viewing help text and
;;   navigating references in it.

;; - IBuffer (Elisp library: `casual-ibuffer.el')
;;   An interface to Emacs IBuffer, a mode designed for managing buffers.

;; - Image (Elisp library: `casual-image.el')
;;   An interface for viewing an image file with `image-mode'.
;;   Resizing an image is supported if ImageMagick 6 or 7 is installed. This
;;   interface deviates significantly with naming conventions used by
;;   `image-mode' to be more in alignment with conventional image editing tools.

;; - Info (Elisp library: `casual-info.el')
;;   An interface for the Info documentation system.

;; - I-Search (Elisp library: `casual-isearch.el')
;;   An interface for the many commands supported by I-Search.

;; - Make (Elisp library: `casual-make.el')
;;   An interface to `make-mode'.

;; - Man (Elisp library: `casual-man.el')
;;   An interface to `Man-mode', the Emacs Man page reader.

;; - Re-Builder (Elisp library: `casual-re-builder.el')
;;   An interface for the Emacs regular expression tool.

;; - Timezone (Elisp library: `casual-timezone.el')
;;   A library of commands to work with different time zones.

;; INSTALLATION

;; Users can choose any or all of the user interfaces made available by Casual
;; at their pleasure.

;; Configuration of a particular Casual user interface is performed per mode.
;; For details, refer to the Info node `(casual) Install'.

;; Casual relies on the latest stable release of `transient' which may differ
;; from the version that is preinstalled as a built-in. By b default, `package.el'
;; will not upgrade a built-in package. Set the customizable variable
;; `package-install-upgrade-built-in' to `t' to override this. For more details,
;; please refer to the "Install" section on this project's repository web page.

;;; Code:
(require 'package)
(require 'casual-lib)

(defun casual-upgrade-base-to-version-2 (enable)
  "Upgrade base Casual packages to version 2 if ENABLE is t.

Use this command to migrate your current Casual version 1.x
packages to the consolidated organization of version 2.x.

This will delete the following packages:

casual-agenda, casual-bookmarks, casual-calc, casual-dired,
casual-editkit, casual-ibuffer, casual-info, casual-isearch,
casual-re-builder, casual-lib.

Note that the package casual-lib will not be deleted if any of the packages
casual-suite, casual-avy, or casual-symbol-overlay is installed."
  (interactive
   (list (y-or-n-p "Upgrade Casual to version 2?")))

  (when enable
    (let ((pkglist (list
                    'casual-agenda
                    'casual-bookmarks
                    'casual-calc
                    'casual-dired
                    'casual-editkit
                    'casual-ibuffer
                    'casual-info
                    'casual-isearch
                    'casual-re-builder
                    'casual-lib)))
      (mapc (lambda (pkg)
              (when (package-installed-p pkg)
                (display-warning
                 :warning
                 (format
                  "Casual 2.0 Migration: Deleting obsolete package %s"
                  (symbol-name pkg)))
                (package-delete (package-get-descriptor pkg) t)
                (package-refresh-contents)))
            pkglist))))

(defun casual-get-package-version (pkg)
  "Get package version of symbol PKG."
  (let* ((pkg-name (symbol-name pkg))
         (pkg-buf (find-library pkg-name))
         (buflist (list pkg-name)))
    (with-current-buffer pkg-buf
      (push (package-get-version) buflist))
    (kill-buffer pkg-buf)
    (string-join (reverse buflist) "-")))

(provide 'casual)
;;; casual.el ends here
