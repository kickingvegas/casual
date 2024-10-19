;;; casual.el --- Transient user interfaces for various modes -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual
;; Keywords: tools, wp
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (transient "0.6.0"))

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

;; INSTALLATION

;; Casual is organized into different user interface (UI) libraries tuned for
;; different modes. Different user interfaces for the following modes are
;; supported:

;; - Agenda (Elisp library: `casual-agenda')
;;   An interface for Org Agenda to help you plan your day.
;;   URL `https://github.com/kickingvegas/casual/docs/agenda.org'

;; - Bookmarks (Elisp library: `casual-bookmarks')
;;   An interface for editing your bookmark collection.
;;   URL `https://github.com/kickingvegas/casual/docs/bookmarks.org'

;; - Calc (Elisp library: `casual-calc')
;;   An interface for Emacs Calc, an embarrasingly feature-rich calculator.
;;   URL `https://github.com/kickingvegas/casual/docs/calc.org'

;; - Dired (Elisp library: `casual-dired')
;;   An interface for the venerable file manager Dired.
;;   URL `https://github.com/kickingvegas/casual/docs/dired.org'

;; - EditKit (Elisp library: `casual-editkit')
;;   A cornucopia of interfaces for the different editing features (e.g.
;;   marking, copying, killing, duplicating, transforming, deleting) of Emacs.
;;   Included are interfaces for rectangle, register, macro, and project
;;   commands.
;;   URL `https://github.com/kickingvegas/casual/docs/editkit.org'

;; - Info (Elisp library: `casual-info')
;;   An interface for the Info documentation system.
;;   URL: `https://github.com/kickingvegas/casual/docs/info.org'

;; - I-Search (Elisp library: `casual-isearch')
;;   An interface for the many commands supported by I-Search.
;;   URL: `https://github.com/kickingvegas/casual/docs/isearch.org'

;; - Re-Builder (Elisp library: `casual-re-builder')
;;   An interface for the Emacs regular expression tool.
;;   URL: `https://github.com/kickingvegas/casual/docs/re-builder.org'

;; Users can choose any or all of the user interfaces made available by Casual
;; at their pleasure.

;; UPGRADING to Casual 2.x

;; If you have installed any Casual package that is version 1.x, you should
;; immediately run the following command upon installation of casual.

;; M-x casual-upgrade-base-to-version-2

;; This command will uninstall any Casual v1.x packages that have been
;; superseded by this package.

;;; Code:
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
                    'casual-re-builder)))
      (mapc (lambda (pkg)
              (when (package-installed-p pkg)
                (display-warning
                 :warning
                 (format
                  "Casual 2.0 Migration: Deleting obsolete package %s"
                  (symbol-name pkg)))
                (package-delete (package-get-descriptor pkg) t)
                (package-refresh-contents)))
            pkglist))

    (let* ((pkglist (list
                     'casual-suite
                     'casual-avy
                     'casual-symbol-overlay))
           (test (seq-reduce (lambda (a b) (or a b))
                             (mapcar #'package-installed-p pkglist)
                             nil)))
      ;; TODO: add this logic to upgrade existing 3rd party packages when they are ready.
      ;; (mapc (lambda (pkg)
      ;;         (when (package-installed-p pkg)
      ;;           (display-warning
      ;;            :warning
      ;;            (format
      ;;             "Casual 2.0 Migration: Upgrading package %s"
      ;;             (symbol-name pkg)))
      ;;           (package-upgrade pkg)))
      ;;       pkglist)

      ;; TODO: change logic to delete casual-lib when 3rd party packages are updated.
      (when (and (not test) (package-installed-p 'casual-lib))
        (display-warning
                 :warning
                 (format
                  "Casual 2.0 Migration: Deleting obsolete package %s"
                  (symbol-name 'casual-lib)))
        (package-delete (package-get-descriptor 'casual-lib) t)
        (package-refresh-contents)))))

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
