const Signals = imports.signals;
const Lang = imports.lang;
const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;

const ExtUtils = imports.misc.extensionUtils;
const Ext = ExtUtils.getCurrentExtension();

const Settings = Ext.imports.settings;
const Utils = Ext.imports.utils;
const Monitor = Ext.imports.monitor;
const Tasks = Ext.imports.tasks;


const SERVER_NAME_REGEXP = /^[\w-]+$/;

function _isServerNameValid(name) {
    return SERVER_NAME_REGEXP.test(name);
}


const Server = new Lang.Class({
    Name: 'EmacsManager.Server',

    _init: function(name, state) {
        this.name = name;
        this._state = state || 'LAUNCHING';
    },

    set state(v) {
        if (this._state !== v) {
            this._state = v;
            this.emit('state-changed', v);
        }
    },
    get state() {
        return this._state;
    },

    kill: function() {
        if (this._state === 'RUNNING') {
            this.state = 'KILLING';
            Tasks.killServer(this.name);
        }
    },

    startClient: function() {
        if (this._state === 'RUNNING')
            Tasks.startClient(this.name);
    }
});
Signals.addSignalMethods(Server.prototype);


const RemoteServer = new Lang.Class({
    Name: 'EmacsManager.RemoteServer',
    _init: function(name) {
        let [result, contents] =
                Gio.file_new_for_path(GLib.build_filenamev(
                    [Settings.EMACS_SERVERS_DIR, name])).load_contents(null);
        contents = new String(contents);

        this.host = contents.match(/^([^\s]+)/)[1];
        this.name = name;
    },

    startClient: function() {
        Tasks.startRemoteClient(this.name);
    }
});


const EmacsManager = new Lang.Class({
    Name: 'EmacsManager.EmacsManager',

    _init: function() {
        this._servers = {};
        this._remoteServers = {};

        let m = this._socketsMonitor = new Monitor.DirectoryMonitor(Settings.EMACS_SOCKETS_DIR);
        m.connect('directory-created', this._syncLocalServers.bind(this));
        m.connect('created', this._onSocketFileCreated.bind(this));
        m.connect('deleted', this._onSocketFileDeleted.bind(this));
        m.enable();

        m = this._serversMonitor = new Monitor.DirectoryMonitor(Settings.EMACS_SERVERS_DIR);
        m.connect('created', this._onRemoteSocketFileCreated.bind(this));
        m.connect('deleted', this._onRemoteSocketFileDeleted.bind(this));
        m.enable();

        this._syncLocalServers();

        Utils.eachFile(Settings.EMACS_SERVERS_DIR, function(f) {
            let name = f.get_name();

            if (_isServerNameValid(name)) {
                this._createRemoteServer(name);
            }
        }, this);
    },

    _syncLocalServers: function() {
        Utils.eachFile(Settings.EMACS_SOCKETS_DIR, function(f) {
            let name = f.get_name();

            if (!this._servers[name] && _isServerNameValid(name)) {
                this._createServer(name);
            }
        }, this);
    },

    destroy: function() {
        this._socketsMonitor.disable();
        this._serversMonitor.disable();
    },

    _createServer: function(name) {
        let s = new Server(name, 'RUNNING');
        this._servers[name] = s;
        this.emit('server-created', s);
    },
    _createRemoteServer: function(name) {
        let s = new RemoteServer(name);
        this._remoteServers[name] = s;
        this.emit('remote-server-created', s);
    },

    _onSocketFileCreated: function(source, info) {
        let name = info.get_basename();
        let s = this._servers[name];

        if (s) {
            s.state = 'running';
        } else {
            if (_isServerNameValid(name)) {
                this._createServer(name);
            }
        }
    },

    _onSocketFileDeleted: function(source, info) {
        let name = info.get_basename();
        let s = this._servers[name];

        if (s) {
            this.emit('server-deleted', s);
            delete this._servers[name];
        }
    },

    _onRemoteSocketFileCreated: function(source, info) {
        let name = info.get_basename();
        if (_isServerNameValid(name)) {
            this._createRemoteServer(name);
        }
    },

    _onRemoteSocketFileDeleted: function(source, info) {
        let name = info.get_basename();
        let s = this._remoteServers[name];

        if (s) {
            this.emit('remote-server-deleted', s);
            delete this._remoteServers[name];
        }
    },

    startServer: function(name) {
        if (!_isServerNameValid(name)) {
            throw new Error('Invalid server name');
        }

        if (this._servers[name]) {
            throw new Error('Server with this name already exist');
        }

        Tasks.startServer(name);
    }
});
Signals.addSignalMethods(EmacsManager.prototype);
