const Signals = imports.signals;
const Lang = imports.lang;
const GLib = imports.gi.GLib;
const Clutter = imports.gi.Clutter;
const St = imports.gi.St;
const Gio = imports.gi.Gio;
const Util = imports.misc.util;
const Main = imports.ui.main;
const Tweener = imports.ui.tweener;
const PanelMenu = imports.ui.panelMenu;
const ModalDialog = imports.ui.modalDialog;
const PopupMenu = imports.ui.popupMenu;

const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();
const Convenience = Me.imports.convenience;

const DIALOG_GROW_TIME = 0.1;

const SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY = 'custom-socket-dir-enabled';
const SETTINGS_CUSTOM_SOCKET_DIR_KEY = 'custom-socket-dir';
const SETTINGS_VIRTUAL_ENVIRONMENT_DIR_KEY = 'virtual-environment-dir';
const SETTINGS_DESKTOP_DIR_KEY = 'desktop-dir';
const SETTINGS_REMOTE_DIR_KEY = 'remote-dir';


function _expandPath(path) {
    if (path[0] === '~') {
        return GLib.get_home_dir() + path.slice(1);
    }
    return path;
}

function _pathExists(f) {
    return Gio.file_new_for_path(f).query_exists(null);
}

function _getPath(key) {
    return _expandPath(settings.get_string(key));
}


const DirMonitor = new Lang.Class({
    Name: 'EmacsManager.DirMonitor',

    _init: function(path, filter) {
        this.path = path
        this._snapshot = {};
        this._filter = filter;
    },

    enable: function() {
        this._file = Gio.file_new_for_path(this.path);
        this._monitor = this._file.monitor_directory(Gio.FileMonitorFlags.NONE,
                                                     null);
        this._monitor.connect('changed', Lang.bind(this, this._onChanged));
    },

    disable: function() {
        if (this._monitor !== undefined) {
            this._monitor.cancel();
            this._monitor = undefined;
        }
    },

    _onChanged: function() {
        this.update();
    },

    update: function() {
        let f = this._file;

        if (f.query_exists(null)) {
            let en = f.enumerate_children('standard::*',
                                          Gio.FileQueryInfoFlags.NONE,
                                          null);
            let currentSnapshot = {};
            let info;

            while ((info = en.next_file(null)) != null) {
                let name = info.get_name();
                if (this._filter !== undefined) {
                    if (!name.match(this._filter)) {
                        continue;
                    }
                }
                currentSnapshot[name] = true;
                if (this._snapshot[name] === undefined) {
                    this._snapshot[name] = info;
                    this.emit('file-added', info, GLib.build_filenamev([this.path, name]));
                }
            }
            en.close(null);

            for (k in this._snapshot) {
                if (currentSnapshot[k] === undefined) {
                    this.emit('file-removed', this._snapshot[k], GLib.build_filenamev([this.path, name]));
                    delete this._snapshot[k];
                }
            }
        }
    }
});
Signals.addSignalMethods(DirMonitor.prototype);


const ServerList = new Lang.Class({
    Name: 'EmacsManager.ServerList',

    _init: function() {
        this.servers = {}
    },

    add: function(s) {
        this.servers[s.name] = s;
        this.emit('added', s);
    },
    remove: function(s) {
        delete this.servers[s];
        this.emit('removed', s);
    },
    destroy: function() {
    }
});
Signals.addSignalMethods(ServerList.prototype);


const Server = new Lang.Class({
    Name: 'EmacsManager.Server',

    _init: function(name) {
        this.name = name;
        this.state = 'RUNNING';
    }
});
Signals.addSignalMethods(Server.prototype);


const LocalServer = new Lang.Class({
    Name: 'EmacsManager.LocalServer',
    Extends: Server,

    _init: function(name) {
        this.parent(name);
    },

    kill: function() {
        if (this.state == 'RUNNING') {
            Util.spawn(['emacsclient',
                        '-s', this.name,
                        '-e', '(kill-emacs)']);
            this.state = 'KILLING';
        }
    },

    startClient: function() {
        if (this.state == 'RUNNING') {
            Util.spawn(['emacsclient',
                        '-c',
                        '-n',
                        '-s', this.name]);
        }
    }
});


const RemoteServer = new Lang.Class({
    Name: 'EmacsManager.RemoteServer',
    Extends: Server,

    _init: function(name, host) {
        this.parent(name);
        this.host = host;
    },

    startClient: function() {
        if (this.state == 'RUNNING') {
            Util.spawn(['emacsclient',
                        '-c',
                        '-n',
                        '-f', e.name]);
        }
    }
});


const RunCompleter = new Lang.Class({
    Name: 'EmacsManager.RunCompleter',

    _init: function() {
        this._re = new RegExp('([^\\.]+)\\.desktop');
        this._desktops = [];

        let m = this._monitor = new DirMonitor(_getPath(SETTINGS_DESKTOP_DIR_KEY),
                                               this._re);
        m.connect('file-added',
                  Lang.bind(this, this._desktopAdded));
        m.connect('file-removed',
                  Lang.bind(this, this._desktopRemoved));
        m.enable();
        m.update();
    },

    destroy: function() {
        this._monitor.disable();
    },

    _desktopAdded: function(source, info) {
        let name = info.get_name(),
            match = this._re.exec(name);

        this._desktops.push(match[1]);
    },
    _desktopRemoved: function(source, n) {
        delete this._desktops[this._desktops.indexOf(n)];
    },

    getCompletion: function(text) {
        let common = '',
            notInit = true,
            items = this._desktops,
            itemsLen = items.length;

        for (let i = 0; i < itemsLen; i++) {
            if (items[i].indexOf(text) != 0) {
                continue;
            }
            if (notInit) {
                common = items[i];
                notInit = false;
            }
            common = this._getCommon(common, items[i]);
        }
        if (common.length)
            return common.substr(text.length);
        return common;
    },

    _getCommon: function(s1, s2) {
        let k = 0,
            s1Len = s1.length,
            s2Len = s2.length;
        for (; k < s1Len && k < s2Len; k++) {
            if (s1[k] != s2[k])
                break;
        }
        if (k == 0)
            return '';
        return s1.substr(0, k);
    }
});


/* <view> */
const MenuItem = new Lang.Class({
    Name: 'EmacsManager.MenuItem',
    Extends: PopupMenu.PopupBaseMenuItem,

    _init: function(server) {
        this.parent();

        this.server = server;
        this.name = server.name;

        this.addActor(new St.Label({
            text: server.name,
            style_class: 'emacs-manager-menu-item-name'
        }));

        this.connect('activate', Lang.bind(this, this._onActivate));
    },

    _onActivate: function(e, c) {
        this.server.startClient();
    }
});

const LocalMenuItem = new Lang.Class({
    Name: 'EmacsManager.LocalMenuItem',
    Extends: MenuItem,

    _init: function(server) {
        this.parent(server);

        let b = new St.Button({
            child: new St.Icon({
                icon_name: 'window-close',
                icon_type: St.IconType.SYMBOLIC,
                icon_size: 16,
                style_class: 'emacs-manager-menu-item-kill-server-icon'
            })
        });
        b.connect('clicked', Lang.bind(this, this._onKill));
        this.addActor(b, { align: St.Align.END });
    },

    _onKill: function(e) {
        this.server.kill();
    }
});

const RemoteMenuItem = new Lang.Class({
    Name: 'EmacsManager.RemoteMenuItem',
    Extends: MenuItem,

    _init: function(server) {
        this.parent(server);

        this.addActor(new St.Label({
            text: server.host,
            style_class: 'emacs-manager-menu-item-remote-host'
        }));
    }
});

const StatusButton = new Lang.Class({
    Name: 'EmacsManager.StatusButton',
    Extends: PanelMenu.SystemStatusButton,

    _init: function(serverList, runDialog) {
        this.parent('accessories-text-editor');

        this._serverList = serverList;
        this._runDialog = runDialog;

        this._contentSection = new PopupMenu.PopupMenuSection();
        this.menu.addMenuItem(this._contentSection);

        this.menu.addAction(_("Start emacs server"),
                            Lang.bind(this, this._onStartServer));
    },

    _onStartServer: function() {
        this._runDialog.open();
    },

    update: function() {
        let servers = this._serverList.servers,
            counter = 0;

        this._contentSection.removeAll();

        for (name in servers) {
            let server = servers[name],
                i;

            if (server instanceof LocalServer) {
                i = new LocalMenuItem(server);
            } else if (server instanceof RemoteServer) {
                i = new RemoteMenuItem(server);
            }

            this._contentSection.addMenuItem(i);
            counter += 1;
        }

        if (counter > 0) {
            if (!this._separator) {
                this._separator = new PopupMenu.PopupSeparatorMenuItem();
                this.menu.addMenuItem(this._separator, 1);
            }
        } else {
            if (this._separator) {
                this._separator.destroy();
                this._separator = undefined;
            }
        }
    }
});


const RunDialog = new Lang.Class({
    Name: 'EmacsManager.RunDialog',
    Extends: ModalDialog.ModalDialog,

    _init: function() {
        let label,
            entry,
            errorIcon;

        this.parent({ styleClass: 'run-dialog' });

        this._completer = new RunCompleter();

        label = new St.Label({
            style_class: 'run-dialog-label',
            text: _("Please enter emacs server name:")
        });
        this.contentLayout.add(label, { y_align: St.Align.START });

        entry = new St.Entry({ style_class: 'run-dialog-entry' });
        entry.label_actor = label;
        this._entryText = entry.clutter_text;
        this.contentLayout.add(entry, { y_align: St.Align.START });
        this.setInitialKeyFocus(this._entryText);

        this._errorBox = new St.BoxLayout({
            style_class: 'run-dialog-error-box'
        });

        this.contentLayout.add(this._errorBox, { expand: true });

        errorIcon = new St.Icon({
            icon_name: 'dialog-error',
            icon_size: 24,
            style_class: 'run-dialog-error-icon'
        });

        this._errorBox.add(errorIcon, { y_align: St.Align.MIDDLE });

        this._commandError = false;

        this._errorMessage = new St.Label({
            style_class: 'run-dialog-error-label'
        });
        this._errorMessage.clutter_text.line_wrap = true;

        this._errorBox.add(this._errorMessage, {
            expand: true,
            y_align: St.Align.MIDDLE,
            y_fill: false
        });

        this._errorBox.hide();

        this._entryText.connect('key-press-event',
                                Lang.bind(this, this._onKeyPress));
    },

    _onKeyPress: function(o, e) {
        let sym = e.get_key_symbol();

        if (sym == Clutter.Return || sym == Clutter.KP_Enter) {
            this.popModal();
            this._run(o.get_text());
            if (!this._commandError) {
                this.close();
            } else {
                if (!this.pushModal())
                    this.close();
            }
            return true;
        } else if (sym == Clutter.Escape) {
            this.close();
            return true;
        } else if (sym == Clutter.Tab) {
            let text = o.get_text(),
                postfix = this._getCompletion(text);

            if (postfix != null && postfix.length > 0) {
                o.insert_text(postfix, -1);
                o.set_cursor_position(text.length + postfix.length);
            }
            return true;
        }
        return false;
    },

    _getCompletion: function(text) {
        return this._completer.getCompletion(text);
    },

    _run : function(input) {
        this._commandError = false;

        if (input) {
            try{
                ext.startServer(input);
            } catch (e) {
                this._showError(e.message);
            }
        }
    },

    _showError : function(message) {
        this._commandError = true;
        this._errorMessage.set_text(message);

        if (!this._errorBox.visible) {
            let [errorBoxMinHeight, errorBoxNaturalHeight] = this._errorBox.get_preferred_height(-1);

            let parentActor = this._errorBox.get_parent();
            Tweener.addTween(parentActor, {
                height: parentActor.height + errorBoxNaturalHeight,
                time: DIALOG_GROW_TIME,
                transition: 'easeOutQuad',
                onComplete: Lang.bind(this,
                                      function() {
                                          parentActor.set_height(-1);
                                          this._errorBox.show();
                                      })
            });
        }
    },

    open: function() {
        this._errorBox.hide();
        this._entryText.set_text('');
        this._commandError = false;
        this.parent();
    },

    destroy: function() {
        this._completer.destroy();
        this.parent();
    }
});


const View = new Lang.Class({
    Name: 'EmacsManager.View',

    _init: function(serverList) {
        this._runDialog = new RunDialog();
        this._statusButton = new StatusButton(serverList, this._runDialog);
        this._serverList = serverList;
        Main.panel.addToStatusArea('emacs-manager', this._statusButton)
    },

    destroy: function() {
        this._statusButton.destroy();
        this._runDialog.destroy();
    },

    update: function() {
        this._statusButton.update();
    }
});
/* </view> */


const Extension = new Lang.Class({
    Name: 'EmacsManager.Extension',

    _init: function() {
        let ret = GLib.spawn_sync(null, ['/usr/bin/id', '-u'], null, 0, null),
            uid = (''+ret[1]).replace(/\s+$/, '');

        this._defaultSocketDir = '/tmp/emacs' + uid;

        let sl = this._serverList = new ServerList();
        let v = this._view = new View(sl);
        this._monitors = [];

        sl.connect('added', Lang.bind(v, v.update));
        sl.connect('removed', Lang.bind(v, v.update));
        sl.connect('changed', Lang.bind(v, v.update));

        this._initMonitors();
    },

    _initMonitors: function() {
        let local = new DirMonitor(this._getSocketDir());
        local.connect('file-added',
                      Lang.bind(this, this._localServerAdded));
        local.connect('file-removed',
                      Lang.bind(this, this._localServerRemoved));
        local.enable();
        local.update();

        this._monitors.push(local);


        let remote = new DirMonitor(_getPath(SETTINGS_REMOTE_DIR_KEY));
        remote.connect('file-added',
                       Lang.bind(this, this._remoteServerAdded));
        remote.connect('file-removed',
                       Lang.bind(this, this._remoteServerRemoved));
        remote.enable();
        remote.update();

        this._monitors.push(remote);
    },

    _localServerAdded: function(source, info, path) {
        let s = new LocalServer(info.get_name());
        this._serverList.add(s);
    },

    _localServerRemoved: function(source, info, path) {
        this._serverList.remove(info.get_name());
    },

    _remoteServerAdded: function(source, info, path) {
        let [result, contents] = Gio.file_new_for_path(path).load_contents(null);
        contents = new String(contents);
        contents = contents.match(/^([^\s]+)/)[1];

        let s = new RemoteServer(info.get_name(), contents);
        this._serverList.add(s);
    },

    _remoteServerRemoved: function(source, info, path) {
        this._serverList.remove(info.get_name());
    },

    _getSocketDir: function() {
        if (settings.get_boolean(SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY)) {
            return _getPath(SETTINGS_CUSTOM_SOCKET_DIR_KEY);
        } else {
            return this._defaultSocketDir;
        }
    },

    startServer: function(name) {
        if (this._serverList.servers[name] !== undefined) {
             throw Error('Emacs server "' + name + '" is already running');
        }

        let venvDir = _getPath(SETTINGS_VIRTUAL_ENVIRONMENT_DIR_KEY),
            venvFile = GLib.build_filenamev([venvDir, name + '.sh']),
            argv;

        // Virtual Environment hooks
        if (_pathExists(venvFile)) {
            argv = ['bash', '-c',
                    'source ' + venvFile + '; emacs --daemon=' + name +''];
        } else {
            argv = ['emacs', '--daemon=' + name];
        }

        Util.spawn(argv);
    },

    destroy: function() {
        this._view.destroy();
        this._serverList.destroy();
        for (i in this._monitors) {
            this._monitors[i].disable();
        }
    },

    shutdown: function() {
    }
});


let settings;
let ext;

function enable() {
    ext = new Extension();
}

function disable() {
    ext.destroy();
    ext = undefined;
}

function init() {
    settings = Convenience.getSettings();
}
