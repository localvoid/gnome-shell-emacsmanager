const Signals = imports.signals;

const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;

const ExtUtils = imports.misc.extensionUtils;
const Ext = ExtUtils.getCurrentExtension();
const Settings = Ext.imports.settings;
const Utils = Ext.imports.utils;
const Monitor = Ext.imports.monitor;


const DESKTOP_FILENAME_REGEXP = /^([^\.]+)\.desktop$/;

const RunCompleter = class {

    constructor() {
        this._desktops = [];

        Utils.eachFile(Settings.EMACS_DESKTOP_DIR, function(f) {
            let name = f.get_name();
            let match = DESKTOP_FILENAME_REGEXP.exec(name);

            if (match) {
                this._desktops.push(match[1]);
            }
        }, this);

        let m = this._monitor = new Monitor.DirectoryMonitor(
            Settings.EMACS_DESKTOP_DIR);
        m.connect('created', this._onFileCreated.bind(this));
        m.connect('deleted', this._onFileDeleted.bind(this));

        m.enable();
    }

    destroy() {
        this._monitor.disable();
    }

    _onFileCreated(source, info) {
        let name = info.get_basename();
        let match = DESKTOP_FILENAME_REGEXP.exec(name);

        if (match) {
            this._desktops.push(match[1]);
        }
    }

    _onFileDeleted(source, info) {
        delete this._desktops[this._desktops.indexOf(info.get_basename())];
    }

    getCompletion(text) {
        let common = '';
        let notInit = true;
        let items = this._desktops;
        let itemsLen = items.length;

        for (let i = 0; i < itemsLen; i++) {
            if (items[i].indexOf(text) != 0)
                continue;

            if (notInit) {
                common = items[i];
                notInit = false;
            }
            common = this._getCommon(common, items[i]);
        }
        if (common.length) {
            return common.substr(text.length);
        }

        return common;
    }

    _getCommon(s1, s2) {
        if (s1 === null) {
            return s2;
        }

        let k = 0
        for (; k < s1.length && k < s2.length; k++) {
            if (s1[k] != s2[k]) {
                break;
            }
        }
        if (k === 0) {
            return '';
        }
        return s1.substr(0, k);
    }
};
