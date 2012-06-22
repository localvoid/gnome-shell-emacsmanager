EXT_DIR=$(HOME)/.local/share/gnome-shell/extensions/emacsmanager@localvoid.gmail.com/

all: dist

dist:
	zip -j emacsmanager.zip metadata.json extension.js

update:
	cp -f extension.js $(EXT_DIR)
	cp -f metadata.json $(EXT_DIR)
