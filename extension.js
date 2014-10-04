const Lang = imports.lang;

const ExtUtils = imports.misc.extensionUtils;
const Ext = ExtUtils.getCurrentExtension();

const Core = Ext.imports.core;
const Autocomplete = Ext.imports.autocomplete;
const Views = Ext.imports.views;

const Extension = new Lang.Class({
    Name: 'EmacsManager.Extension',

    enable: function() {
        try {
            this._emacsManager = new Core.EmacsManager();
            this._runCompleter = new Autocomplete.RunCompleter();
            this._view = new Views.View(this._emacsManager, this._runCompleter);
        } catch (e) {
            if (this._runCompleter) {
                this._runCompleter.destroy();
            }
            if (this._emacsManager) {
                this._emacsManager.destroy();
            }

            this._emacsManager = null;
            this._runCompleter = null;
            this._view = null;

            throw e;
        }
    },

    disable: function() {
        this._view.destroy();
        this._runCompleter.destroy();
        this._emacsManager.destroy();

        this._view = null;
        this._runCompleter = null;
        this._emacsManager = null;
    }
});

function init(metadata) {
    return new Extension();
}
