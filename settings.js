const GLib = imports.gi.GLib;
const Polkit = imports.gi.Polkit;

const UID = Polkit.UnixUser.new_for_name(GLib.get_user_name()).get_uid().toString();
const EMACS_DIR = GLib.build_filenamev([GLib.get_home_dir(), '.emacs.d']);
const EMACS_DESKTOP_DIR = GLib.build_filenamev([EMACS_DIR, 'desktop']);
const EMACS_SOCKETS_DIR = GLib.build_filenamev(['/', 'tmp', 'emacs' + UID]);
const EMACS_SERVERS_DIR = GLib.build_filenamev([EMACS_DIR, 'server']);
const EMACS_VIRTUAL_DIR = GLib.build_filenamev([EMACS_DIR, 'virtualenv']);
