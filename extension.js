const Signals = imports.signals;
const Lang = imports.lang;
const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;

const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();
const Views = Me.imports.views;
const Autocomplete = Me.imports.autocomplete;

const EMACS_DIR = GLib.build_filenamev([GLib.get_home_dir(), '.emacs.d']);
const EMACS_DESKTOP_DIR = GLib.build_filenamev([EMACS_DIR, 'desktop']);

const EmacsManagerIface = <interface name="com.localvoid.EmacsManager">
  <method name="StartServer">
    <arg type="s" direction="in"/>
    <arg type="i" direction="out"/>
  </method>
  <method name="GetServers">
    <arg type="ao" direction="out"/>
  </method>
  <signal name="ServerCreated">
    <arg type="o"/>
  </signal>
  <signal name="ServerKilled">
    <arg type="o"/>
  </signal>
</interface>;

const ServerIface = <interface name="com.localvoid.EmacsManager.Server">
  <method name="Kill"/>
  <method name="Execute"/>
  <method name="StartClient"/>
  <property name="Name" type="s" access="read"/>
  <property name="State" type="s" access="read"/>
</interface>;


const EmacsManagerProxy = Gio.DBusProxy.makeProxyWrapper(EmacsManagerIface);
const ServerProxy = Gio.DBusProxy.makeProxyWrapper(ServerIface);


const Server = new Lang.Class({
    Name: 'EmacsManager.Server',

    _init: function(path) {
        this._proxy = new ServerProxy(Gio.DBus.session,
                                      'com.localvoid.EmacsManager',
                                      path);
        this._proxy.connect('g-properties-changed', this._onPropertiesChanged.bind(this));

        this.name = this._proxy.Name;
        this.state = this._proxy.State;
    },

    destroy: function() {
    },

    kill: function() {
        this._proxy.KillSync();
    },

    startClient: function() {
        this._proxy.StartClientSync();
    },

    _onPropertiesChanged: function(proxy, properties, invalidated) {
        properties = properties.deep_unpack();
        if (properties.state) {
            this.state = properties.state.deep_unpack();
        }
        this.emit('changed');
    }
});
Signals.addSignalMethods(Server.prototype);


const EmacsManager = new Lang.Class({
    Name: 'EmacsManager.EmacsManager',

    _init: function() {
        this.state = 'unavailable';
        this.servers = {};
        this._proxy = new EmacsManagerProxy(Gio.DBus.session,
                                            'com.localvoid.EmacsManager',
                                            '/com/localvoid/EmacsManager');

        this._proxy.connectSignal('ServerCreated',
                                  this._onServerCreated.bind(this),
                                  'com.localvoid.EmacsManager');
        this._proxy.connectSignal('ServerDeleted',
                                  this._onServerDeleted.bind(this),
                                  'com.localvoid.EmacsManager');

        let [servers, error] = this._proxy.GetServersSync();
        servers.forEach(function(s) {
            this.servers[s] = new Server(s);
        }, this);
    },

    _onServerCreated: function(object, senderName, [object_path]) {
        let srv = new Server(object_path);
        this.servers[object_path] = srv;
        this.emit('server-created', srv);
    },

    _onServerDeleted: function(object, senderName, [object_path]) {
        let srv = this.servers[object_path];
        delete this.servers[object_path];
        this.emit('server-deleted', srv);
    },

    destroy: function() {
        for (let s in this.servers) {
            this.servers[s].destroy();
        }
    },

    startServer: function(name) {
        let [result, error] = this._proxy.StartServerSync(name);
        if (error) {
            throw error;
        }
    }
});
Signals.addSignalMethods(EmacsManager.prototype);


const Extension = new Lang.Class({
    Name: 'EmacsManager.Extension',

    _init: function() {
        this._appeared = false;
        this._watcherId = Gio.bus_watch_name(Gio.BusType.SESSION,
                                             'com.localvoid.EmacsManager',
                                             Gio.BusNameWatcherFlags.AUTO_START,
                                             this._onAppeared.bind(this),
                                             this._onVanished.bind(this));
    },

    _onAppeared: function() {
        if (!this._appeared) {
            this._appeared = true;
            this._emacsManager = new EmacsManager();
            this._runCompleter = new Autocomplete.RunCompleter(EMACS_DESKTOP_DIR);
            this._view = new Views.View(this._emacsManager, this._runCompleter);
        }
    },
    _onVanished: function() {
        if (this._appeared) {
            this._appeared = false;
            this._view.destroy();
            this._runCompleter.destroy();
            this._emacsManager.destroy();
        }
    },

    destroy: function() {
        Gio.bus_unwatch_name(this._watcherId);
        this._onVanished();
    }
});


let ext;

function enable() {
    ext = new Extension();
}

function disable() {
    ext.destroy();
    ext = undefined;
}

function init() {
}
