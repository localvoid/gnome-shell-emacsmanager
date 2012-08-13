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

Virtual Environment hooks
^^^^^^^^^^^^^^^^^^^^^^^^^

You can automaticaly initialize your virtual environment when starting
emacs servers.

Create file my-venv.sh in Virtual Environment Directory (default:
~/.emacs.d/virtualenv) with initialization of your environment::

    . ~/my-venv/bin/activate
    export EXAMPLE_ENVIRONMENT_VAR=1

And next time when you start emacs server with the name 'my-venv', it
will preload your virtual environment.

desktop-save-mode
-----------------
Saving and restoring emacs sessions.

Create new directory in your emacs folder::

    $ mkdir ~/.emacs.d/desktop

and add this to your init.el file::

    (when (daemonp)
      (defadvice desktop-restore-file-buffer
        (around my-desktop-restore-file-buffer-advice)
        "Be non-interactive while starting a daemon."
        (let ((noninteractive t))
          ad-do-it))
      (ad-activate 'desktop-restore-file-buffer)

      (setq desktop-dirname             "~/.emacs.d/desktop/"
            desktop-base-file-name      (concat (daemonp) ".desktop")
            desktop-base-lock-name      (concat (daemonp) ".lock")
            desktop-path                (list desktop-dirname)
            desktop-save                t
            desktop-files-not-to-save   "^$" ;reload tramp paths
            desktop-load-locked-desktop nil)
      (desktop-save-mode 1))

DBus API
--------

com.localvoid.EmacsManager
^^^^^^^^^^^^^^^^^^^^^^^^^^
Methods:

- ObjectPath[] GetServers()
- int StartServer(string name)

Signals:

- ServerCreated(ObjectPath server_id)
- ServerDeleted(ObjectPath server_id)

com.localvoid.EmacsManager.Server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Properties:

- string Name
- string State {"RUNNING", "KILLING", "ERROR"}

Methods:

- void Kill()
- void StartClient()
- void Execute(string cmd)
