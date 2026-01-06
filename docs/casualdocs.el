;;; casualdocs.el --- Casual Documentation Configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Choi

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

(require 'org)
(require 'ox)
(require 'ox-texinfo)

(defun cc/reconfig-org-smart-quotes-lang (lang)
  "Reconfigure Org smart quotes to use utf-8 per LANG."
  (let* ((db-entry (assoc-string lang org-export-smart-quotes-alist))
         (utf8-primary-opening (plist-get (assoc-default 'primary-opening db-entry) :utf-8))
         (utf8-primary-closing (plist-get (assoc-default 'primary-closing db-entry) :utf-8))
         (utf8-secondary-opening (plist-get (assoc-default 'secondary-opening db-entry) :utf-8))
         (utf8-secondary-closing (plist-get (assoc-default 'secondary-closing db-entry) :utf-8))
         (utf8-apostrophe (plist-get (assoc-default 'apostrophe db-entry) :utf-8))
         )

    (setf (plist-get
           (assoc-default 'primary-opening
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-primary-opening)

    (setf (plist-get
           (assoc-default 'primary-closing
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-primary-closing)

    (setf (plist-get
           (assoc-default 'secondary-opening
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-secondary-opening)

    (setf (plist-get
           (assoc-default 'secondary-closing
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-secondary-closing)

    (setf (plist-get
           (assoc-default 'apostrophe
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-apostrophe)))


(add-hook 'org-mode-hook (lambda ()
                           (cc/reconfig-org-smart-quotes-lang "en")))


(setopt org-latex-classes
        '(("article" "\\documentclass[11pt]{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("simpleresumecv" "\\documentclass[11pt]{simpleresumecv}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("letter" "\\documentclass[11pt]{letter}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\\\subsection{%s}" . "\\\\subsection*{%s}")
           ("\\\\subsubsection{%s}" . "\\\\subsubsection{*%s}"))))

(setopt org-latex-compiler "xelatex")
(setopt org-export-allow-bind-keywords t)
(setopt org-export-backends '(ascii html icalendar latex md odt texinfo))
(setopt org-export-with-smart-quotes t)
(setopt org-export-with-sub-superscripts '{})
(setopt org-export-with-toc nil)
(setopt org-latex-pdf-process
        '("%latex -interaction nonstopmode --shell-escape -output-directory %o %f"
          "%latex -interaction nonstopmode --shell-escape -output-directory %o %f"
          "%latex -interaction nonstopmode --shell-escape -output-directory %o %f"))

(setopt org-texinfo-classes
        '(("info" "@documentencoding AUTO\12@documentlanguage AUTO"
           ("@chapter %s" "@unnumbered %s" "@chapheading %s" "@appendix %s")
           ("@section %s" "@unnumberedsec %s" "@heading %s" "@appendixsec %s")
           ("@subsection %s" "@unnumberedsubsec %s" "@subheading %s"
            "@appendixsubsec %s")
           ("@subsubsection %s" "@unnumberedsubsubsec %s" "@subsubheading %s"
            "@appendixsubsubsec %s"))
          ("casual" "@documentencoding AUTO\12@documentlanguage AUTO"
           ("@chapter %s" "@unnumbered %s" "@chapheading %s" "@appendix %s")
           ("@section %s" "@unnumberedsec %s" "@subheading %s" "@appendixsec %s")
           ("@subsection %s" "@unnumberedsubsec %s" "@subheading %s"
            "@appendixsubsec %s")
           ("@subsubsection %s" "@unnumberedsubsubsec %s" "@subsubheading %s"
            "@appendixsubsubsec %s"))))

(setopt org-src-lang-modes
        '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
          ("asymptote" . asy) ("dot" . graphviz-dot) ("sqlite" . sql)
          ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
          ("screen" . shell-script) ("shell" . sh) ("bash" . sh)
          ("plantuml" . plantuml) ("swift" . swift) ("swiftui" . swift)
          ("graphviz" . graphviz) ("mscgen" . mscgen)))

(setopt org-latex-prefer-user-labels t)

(provide 'casualdocs)
;;; casualdocs.el ends here
