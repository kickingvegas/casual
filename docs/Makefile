##
# Copyright 2024 Charles Y. Choi
#

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
PKG_NAME=casual


$(PKG_NAME).info: $(PKG_NAME).texi
	texi2any $<

run: $(PKG_NAME).info

clean:
	- rm $(PKG_NAME).texi
