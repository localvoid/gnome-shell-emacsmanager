const Signals = imports.signals;
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


const Server = class {

    constructor(name, state) {
        this.name = name;
        this._state = state || 'LAUNCHING';
    }

    set state(v) {
        if (this._state !== v) {
            this._state = v;
            this.emit('state-changed', v);
        }
    }
    get state() {
        return this._state;
    }

    kill() {
        if (this._state === 'RUNNING') {
            this.state = 'KILLING';
            Tasks.killServer(this.name);
        }
    }

    startClient() {
        if (this._state === 'RUNNING')
            Tasks.startClient(this.name);
    }
};
Signals.addSignalMethods(Server.prototype);


const RemoteServer = class {

    constructor(name) {
        let [result, contents] =
                Gio.file_new_for_path(GLib.build_filenamev(
                    [Settings.EMACS_SERVERS_DIR, name])).load_contents(null);
        contents = new String(contents);

        this.host = contents.match(/^([^\s]+)/)[1];
        this.name = name;
    }

    startClient() {
        Tasks.startRemoteClient(this.name);
    }
};


const EmacsManager = class {

    constructor() {
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
    }

    _syncLocalServers() {
        Utils.eachFile(Settings.EMACS_SOCKETS_DIR, function(f) {
            let name = f.get_name();

            if (!this._servers[name] && _isServerNameValid(name)) {
                this._createServer(name);
            }
        }, this);
    }

    destroy() {
        this._socketsMonitor.disable();
        this._serversMonitor.disable();
    }

    _createServer(name) {
        let s = new Server(name, 'RUNNING');
        this._servers[name] = s;
        this.emit('server-created', s);
    }
    _createRemoteServer(name) {
        let s = new RemoteServer(name);
        this._remoteServers[name] = s;
        this.emit('remote-server-created', s);
    }

    _onSocketFileCreated(source, info) {
        let name = info.get_basename();
        let s = this._servers[name];

        if (s) {
            s.state = 'running';
        } else {
            if (_isServerNameValid(name)) {
                this._createServer(name);
            }
        }
    }

    _onSocketFileDeleted(source, info) {
        let name = info.get_basename();
        let s = this._servers[name];

        if (s) {
            this.emit('server-deleted', s);
            delete this._servers[name];
        }
    }

    _onRemoteSocketFileCreated(source, info) {
        let name = info.get_basename();
        if (_isServerNameValid(name)) {
            this._createRemoteServer(name);
        }
    }

    _onRemoteSocketFileDeleted(source, info) {
        let name = info.get_basename();
        let s = this._remoteServers[name];

        if (s) {
            this.emit('remote-server-deleted', s);
            delete this._remoteServers[name];
        }
    }

    startServer(name) {
        if (!_isServerNameValid(name)) {
            throw new Error('Invalid server name');
        }

        if (this._servers[name]) {
            throw new Error('Server with this name already exist');
        }

        Tasks.startServer(name);
    }
};
Signals.addSignalMethods(EmacsManager.prototype);
