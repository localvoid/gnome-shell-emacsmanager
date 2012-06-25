=========================
 Emacs Manager extension
=========================

Gnome shell extension for working with local emacs servers.

https://extensions.gnome.org/extension/361/emacs-manager/

Virtual Environment
-------------------
It is useful for working in virtual envionments, for example in python
virtualenvs.

Just activate your virtual environment with::

    $ . ./my-venv/bin/activate

or if you are using virtualenvwrapper::

    $ workon my-venv

and launch emacs server::

    $ emacs --daemon=my-venv

desktop-save-mode
-----------------
Saving and restoring emacs sessions.

Create new directory in your emacs folder::

    $ mkdir ~/.emacs.d/desktop

and add this to your init.el file::

    (when (daemonp)
      (setq desktop-dirname             "~/.emacs.d/desktop/"
            desktop-base-file-name      (concat (daemonp) ".desktop")
            desktop-base-lock-name      (concat (daemonp) ".lock")
            desktop-path                (list desktop-dirname)
            desktop-save                t
            desktop-files-not-to-save   "^$" ;reload tramp paths
            desktop-load-locked-desktop nil)
    (desktop-save-mode 1))

