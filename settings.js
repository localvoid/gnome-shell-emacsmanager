const GLib = imports.gi.GLib;

let UID;

(function() {
    let ret = GLib.spawn_sync(null, ['/usr/bin/id', '-u'], null, 0, null);
    UID = new String(ret[1]).trim();
}());

const EMACS_DIR = GLib.build_filenamev([GLib.get_home_dir(), '.emacs.d']);
const EMACS_DESKTOP_DIR = GLib.build_filenamev([EMACS_DIR, 'desktop']);
const EMACS_SOCKETS_DIR = GLib.build_filenamev(['/', 'tmp', 'emacs' + UID]);
const EMACS_SERVERS_DIR = GLib.build_filenamev([EMACS_DIR, 'server']);
const EMACS_VIRTUAL_DIR = GLib.build_filenamev([EMACS_DIR, 'virtualenv']);
