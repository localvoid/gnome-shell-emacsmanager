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

## Connecting to remote servers

To connect to the remote server, you will need to copy server file to
the local `~/.emacs.d/server/` directory. This extension will
automatically detect the presence of server file and add button to
launch emacsclient to the remote server.

> When you start a TCP Emacs server, Emacs creates a server file
> containing the TCP information to be used by emacsclient to connect
> to the server. The variable server-auth-dir specifies the directory
> containing the server file; by default, this is ~/.emacs.d/server/.
>
> [emacsclient Options](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html)

## Debugging

If there are problems with launching or killing emacs servers, you can
look at the log files, using journalctl.

```sh
$ journalctl _UID=`id -u`
```
