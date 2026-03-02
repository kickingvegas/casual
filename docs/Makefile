##
# Copyright 2024-2025 Charles Y. Choi
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

ifdef EMACS_APP_EXEC
  EXEC_NAME=$(EMACS_APP_EXEC)
else
  EXEC_NAME=emacs
endif

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
PKG_NAME=casual
ORG_FILES :=					\
agenda.org					\
bibtex.org					\
bookmarks.org					\
calc.org					\
calendar.org					\
casual.org					\
compile.org					\
css.org						\
csv.org						\
dired.org					\
ediff.org					\
editkit.org					\
elisp.org					\
eshell.org					\
eww.org						\
help.org					\
html.org					\
ibuffer.org					\
image.org					\
info.org					\
isearch.org					\
make-mode.org					\
man.org						\
org.org						\
re-builder.org					\
sponsorship.org					\
timezone.org

$(PKG_NAME).info: $(PKG_NAME).texi
	texi2any $<

.PHONY: run
run: $(PKG_NAME).info

.PHONY: clean
clean: clean-html
	- rm $(PKG_NAME).texi $(PKG_NAME).info

${PKG_NAME}.texi: $(ORG_FILES)
	$(EXEC_NAME) -Q --batch -l casualdocs.el ${PKG_NAME}.org \
	             --eval "(org-texinfo-export-to-texinfo)"

html:
	mkdir html

.PHONY: gen-html
gen-html: ${PKG_NAME}.texi html html/images html/main.css sync-images
	makeinfo				\
	--html					\
	--css-ref=main.css			\
	--output=html				\
	${PKG_NAME}.texi

html/images:
	mkdir -p $@

.PHONY: sync-images
sync-images:
	rsync					\
	--archive				\
	--verbose				\
	--delete				\
	images/ html/images

html/main.css: main.css
	cp main.css html

.PHONY: clean-html
clean-html:
	- rm -rf html

.PHONY: open-html
open-html:
	open html/index.html

.PHONY: open-localhost
open-localhost:
	open "http://localhost:8000"
