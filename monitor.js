// -*- mode: js; flymake-mode: -1; js-indent-level: 4; indent-tabs-mode: nil -*-
const Gio = imports.gi.Gio
    , Signals = imports.signals
    , Lang = imports.lang;


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
                this._monitor.connect('changed', this._onChanged.bind(this));
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
    }
});
Signals.addSignalMethods(DirectoryMonitor.prototype);
