=========================
 Emacs Manager extension
=========================

https://extensions.gnome.org/extension/361/emacs-manager/

Virtual Environment hooks
-------------------------
You can automaticaly initialize your virtual environment when starting
emacs servers.

Create file my-venv.sh in Virtual Environment Directory (~/.emacs.d/virtualenv)
with initialization of your environment::

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
            desktop-load-locked-desktop t)
      (desktop-save-mode 1))

Graceful Shutdown
-----------------

To gracefuly kill emacs servers on shutdown you need to install
`Emacs Session Manager <https://github.com/localvoid/el-session-manager>`_

::

    (when (daemonp)
      (require 'session-manager)
      (session-manager-init (daemonp)))
