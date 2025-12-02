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

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
PKG_NAME=casual

$(PKG_NAME).info: $(PKG_NAME).texi
	texi2any $<

.PHONY: run
run: $(PKG_NAME).info

.PHONY: clean
clean:
	- rm $(PKG_NAME).texi

html:
	mkdir html

.PHONY: gen-html
gen-html: html html/images html/main.css sync-images
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
	rm -rf html

.PHONY: open-html
open-html:
	open html/index.html

.PHONY: open-localhost
open-localhost:
	open "http://localhost:8000"
