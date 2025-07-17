##
# Copyright 2024 Charles Y. Choi
#

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

# --init-file=texinfo.cfg

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
