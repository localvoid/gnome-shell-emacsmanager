// -*- mode: js; flymake-mode: -1; js-indent-level: 4; indent-tabs-mode: nil -*-
const Lang = imports.lang
    , Signals = imports.signals

    , GLib = imports.gi.GLib
    , Gio = imports.gi.Gio

    , ExtUtils = imports.misc.extensionUtils
    , Ext = ExtUtils.getCurrentExtension()
    , Settings = Ext.imports.settings
    , Utils = Ext.imports.utils
    , Monitor = Ext.imports.monitor;


const DESKTOP_FILENAME_REGEXP = /^([^\.]+)\.desktop$/;

const RunCompleter = new Lang.Class({
    Name: 'EmacsManager.RunCompleter',

    _init: function() {
        this._desktops = [];

        Utils.eachFile(Settings.EMACS_DESKTOP_DIR, function(f) {
            let name = f.get_name()
              , match = DESKTOP_FILENAME_REGEXP.exec(name);

            if (match)
                this._desktops.push(match[1]);
        }, this);

        let m = this._monitor = new Monitor.DirectoryMonitor(Settings.EMACS_DESKTOP_DIR);
        m.connect('created', this._onFileCreated.bind(this));
        m.connect('deleted', this._onFileDeleted.bind(this));

        m.enable();
    },

    destroy: function() {
        this._monitor.disable();
    },

    _onFileCreated: function(source, info) {
        let name = info.get_basename()
          , match = DESKTOP_FILENAME_REGEXP.exec(name);

        if (match)
            this._desktops.push(match[1]);
    },

    _onFileDeleted: function(source, info) {
        delete this._desktops[this._desktops.indexOf(info.get_basename())];
    },

    getCompletion: function(text) {
        let common = ''
          , notInit = true
          , items = this._desktops
          , itemsLen = items.length;

        for (let i = 0; i < itemsLen; i++) {
            if (items[i].indexOf(text) != 0)
                continue;

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
        let k = 0
          , s1Len = s1.length
          , s2Len = s2.length

        for (; k < s1Len && k < s2Len; k++) {
            if (s1[k] != s2[k])
                break;
        }
        if (k == 0)
            return '';
        return s1.substr(0, k);
    }
});
