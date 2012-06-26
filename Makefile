localprefix = $(HOME)/.local/share/gnome-shell/extensions
extprefix=$(localprefix)/emacsmanager@localvoid.gmail.com/

all:

zip-file:
	rm -rf $(CURDIR)/_build
	rm -rf $(CURDIR)/zip-files
	mkdir $(CURDIR)/_build
	mkdir $(CURDIR)/zip-files
	mkdir $(CURDIR)/_build/schemas
	cp $(CURDIR)/org.gnome.shell.extensions.emacs-manager.gschema.xml $(CURDIR)/_build/schemas/
	cp $(CURDIR)/metadata.json $(CURDIR)/_build
	cp $(CURDIR)/stylesheet.css $(CURDIR)/_build
	cp $(CURDIR)/extension.js $(CURDIR)/_build
	cp $(CURDIR)/prefs.js $(CURDIR)/_build
	cp $(CURDIR)/convenience.js $(CURDIR)/_build
	glib-compile-schemas $(CURDIR)/_build/schemas
	(cd "$(CURDIR)/_build"; \
		zip -qr "$(CURDIR)/zip-files/emacsmanager.shell-extension.zip" .; \
	);
	rm -rf $(CURDIR)/_build

install: zip-file
	zip_file="$(CURDIR)/zip-files/emacsmanager.shell-extension.zip"; \
	if [ -d "$(extprefix)" ] ; then \
		rm -rf "$(extprefix)"; \
	fi; \
	mkdir $(extprefix); \
	(cd $(extprefix); \
		unzip -q $${zip_file}; \
	);

