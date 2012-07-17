const Signals = imports.signals;
const Lang = imports.lang;
const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;

const DirectoryMonitor = new Lang.Class({
    Name: 'EmacsManager.DirectoryMonitor',

    _init: function(path) {
        this.path = path
    },

    enable: function() {
        if (this._monitor === undefined) {
            this._file = Gio.file_new_for_path(this.path);
            try {
                this._monitor = this._file.monitor_directory(Gio.FileMonitorFlags.NONE,
                                                             null);
                this._monitor.connect('changed', Lang.bind(this, this._onChanged));
            } catch (e) {
                log('DirectoryMonitor error');
            }
        }
    },

    disable: function() {
        if (this._monitor !== undefined) {
            this._monitor.cancel();
            this._monitor = undefined;
        }
    },

    _onChanged: function(monitor, file, other_file, event_type) {
        switch (event_type) {
        case Gio.FileMonitorEvent.CREATED:
            this.emit('created', file);
            break;
        case Gio.FileMonitorEvent.DELETED:
            this.emit('deleted', file);
            break;
        }
    },

});
Signals.addSignalMethods(DirectoryMonitor.prototype);

const RunCompleter = new Lang.Class({
    Name: 'EmacsManager.RunCompleter',

    _init: function(path) {
        this._path = path;
        this._re = new RegExp('([^\\.]+)\\.desktop');
        this._desktops = [];

        let m = this._monitor = new DirectoryMonitor(path);
        m.connect('created',
                  this._onFileCreated.bind(this));
        m.connect('deleted',
                  this._onFileDeleted.bind(this));
        m.enable();
        this._initFiles();
    },

    _initFiles: function() {
        let file = Gio.file_new_for_path(this._path);
        if (file.query_exists(null)) {
            let enumerator = file.enumerate_children('standard::name', Gio.FileQueryInfoFlags.NONE, null);
            let fileInfo;
            while ((fileInfo = enumerator.next_file(null)) != null) {
                let name = fileInfo.get_name();
                let match = this._re.exec(name);

                if (match) {
                    this._desktops.push(match[1]);
                }
            }
        }
    },

    destroy: function() {
        this._monitor.disable();
    },

    _onFileCreated: function(source, info) {
        let name = info.get_basename();
        let match = this._re.exec(name);

        if (match) {
            this._desktops.push(match[1]);
        }
    },

    _onFileDeleted: function(source, info) {
        delete this._desktops[this._desktops.indexOf(info.get_basename())];
    },

    getCompletion: function(text) {
        let common = '';
        let notInit = true;
        let items = this._desktops;
        let itemsLen = items.length;

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
        let k = 0;
        let s1Len = s1.length;
        let s2Len = s2.length;

        for (; k < s1Len && k < s2Len; k++) {
            if (s1[k] != s2[k])
                break;
        }
        if (k == 0)
            return '';
        return s1.substr(0, k);
    }
});
