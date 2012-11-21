// -*- mode: js; flymake-mode: -1; js-indent-level: 4; indent-tabs-mode: nil -*-
const GLib = imports.gi.GLib;

let UID;

(function() {
  let ret = GLib.spawn_sync(null, ['/usr/bin/id', '-u'], null, 0, null);
  UID = (''+ret[1]).replace(/\s+$/, '');
}());

const EMACS_DIR = GLib.build_filenamev([GLib.get_home_dir(), '.emacs.d'])
    , EMACS_DESKTOP_DIR = GLib.build_filenamev([EMACS_DIR, 'desktop'])
    , EMACS_SOCKETS_DIR = GLib.build_filenamev(['/', 'tmp', 'emacs' + UID])
    , EMACS_SERVERS_DIR = GLib.build_filenamev([EMACS_DIR, 'server'])
    , EMACS_VIRTUAL_DIR = GLib.build_filenamev([EMACS_DIR, 'virtualenv']);
