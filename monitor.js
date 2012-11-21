// -*- mode: js; flymake-mode: -1; js-indent-level: 4; indent-tabs-mode: nil -*-
const Gio = imports.gi.Gio
    , Signals = imports.signals
    , Lang = imports.lang;


const DirectoryMonitor = new Lang.Class({
    Name: 'EmacsManager.DirectoryMonitor',

    _init: function(path) {
        this.path = path
        this._monitor = null;
    },

    enable: function() {
        if (!this._monitor) {
            this._file = Gio.file_new_for_path(this.path);
            try {
                this._monitor = this._file.monitor_directory(Gio.FileMonitorFlags.NONE,
                                                             null);
                this._monitor.connect('changed', this._onChanged.bind(this));
            } catch (e) {
                logError(e, 'failed to register file monitor');
            }
        }
    },

    disable: function() {
        if (this._monitor) {
            this._monitor.cancel();
            this._monitor = null;
        }
    },

    _onChanged: function(monitor, file, otherFile, eventType) {
        switch (eventType) {
        case Gio.FileMonitorEvent.CREATED:
            if (file.get_path() === this.path)
                this.emit('directory-created');
            else
                this.emit('created', file);
            break;
        case Gio.FileMonitorEvent.DELETED:
            this.emit('deleted', file);
            break;
        }
    }
});
Signals.addSignalMethods(DirectoryMonitor.prototype);
