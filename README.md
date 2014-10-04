# Emacs Manager [Gnome Shell](https://wiki.gnome.org/Projects/GnomeShell) [extension](https://extensions.gnome.org/extension/361/emacs-manager/)

## Virtual Environment hooks

Sometimes it is useful to launch emacs with a predefined environment
variables (python
[virtual environments](http://virtualenv.readthedocs.org/en/latest/),
go [workspaces](https://golang.org/doc/code.html), etc).

To preload environment variables, create a file `my-venv.sh` in the
Virtual Environments directory `~/.emacs.d/virtualenv`, where you can
put all initialization in a simple shell script.

For example, to activate python virtualenv, you can just use the
`source` function.

```sh
source ~/my-python-venv/bin/activate
export EXAMPLE_ENVIRONMENT_VAR=1
```

And next time when you start emacs server with the name `my-venv`, it
will preload your virtual environment.

## desktop-save-mode

Saving and restoring emacs sessions.

Create a new directory in your emacs folder

```sh
$ mkdir ~/.emacs.d/desktop
```

and add this to your `init.el` file

```lisp
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
```

## Debugging

If there are problems with launching or killing emacs servers, you can
look at the log files, using journalctl.

```sh
$ journalctl _UID=`id -u`
```
