const Lang = imports.lang;
const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;

const ExtUtils = imports.misc.extensionUtils;
const Ext = ExtUtils.getCurrentExtension();
const Settings = Ext.imports.settings;


function _run(argv) {
    try {
        let [success, pid] =
            GLib.spawn_async(GLib.get_home_dir(),
                             argv,
                             null,
                             GLib.SpawnFlags.SEARCH_PATH | GLib.SpawnFlags.DO_NOT_REAP_CHILD,
                             null);

        GLib.child_watch_add(GLib.PRIORITY_DEFAULT, pid, function () {}, null);
    } catch (e) {
        logError(e, 'failed to run ' + argv);
    }
}

function startServer(name) {
    let argv;
    let venv_path = GLib.build_filenamev([Settings.EMACS_VIRTUAL_DIR,
                                          name + '.sh']);
    let venv_file = Gio.File.new_for_path(venv_path);

    if (venv_file.query_exists(null)) {
        argv = ['bash', '-c',
                'source ' + venv_path + ' && emacs --daemon=' + name];
    } else {
        argv = ['emacs', '--daemon=' + name];
    }

    _run(argv);
}

function startClient(name) {
    _run(['emacsclient',
          '-s', name,
          '-c',
          '-n']);
}

function startRemoteClient(name) {
    _run(['emacsclient',
          '-f', name,
          '-c',
          '-n']);
}

function killServer(name) {
    _run(['emacsclient',
          '-s', name,
          '-n',
          '-e', '(kill-emacs)']);
}
