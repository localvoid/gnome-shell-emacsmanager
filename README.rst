=========================
 Emacs Manager extension
=========================

Gnome shell extension for working with local emacs servers.
`https://extensions.gnome.org/extension/361/emacs-manager/`_

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

