const Lang = imports.lang;
const Gtk = imports.gi.Gtk;
const GObject = imports.gi.GObject;

let ExtensionUtils = imports.misc.extensionUtils;
let Me = ExtensionUtils.getCurrentExtension();
let Convenience = Me.imports.convenience;

const SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY = 'custom-socket-dir-enabled';
const SETTINGS_CUSTOM_SOCKET_DIR_KEY = 'custom-socket-dir';

const EmacsManagerSettingsWidget = new GObject.Class({
    Name: 'EmacsManager.Prefs.EmacsManagerSettingsWidget',
    GTypeName: 'EmacsManagerSettingsWidget',
    Extends: Gtk.Box,

    _init: function(params) {
        this.parent({
            orientation: Gtk.Orientation.VERTICAL,
            border_width: 10
        });

        this._settings = Convenience.getSettings();
        let customDirEnabled = this._settings.get_boolean(SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY),
            customDir = this._settings.get_string(SETTINGS_CUSTOM_SOCKET_DIR_KEY);


        let introLabel = new Gtk.Label({
            label: "Emacs Manager can search server sockets in default"+
                " directory \"/tmp/emacs${ID}\", or in a custom directory",
            wrap: true,
            sensitive: true
        });
        this.pack_start(introLabel, false, false, 5);


        let hbox = new Gtk.Box({
           orientation: Gtk.Orientation.HORIZONTAL
        });
        hbox.pack_start(new Gtk.Label({
           label: "<b>Custom Socket Directory</b>",
           use_markup: true,
           xalign: 0
        }), true, true, 0);
        this._customDirSwitch = new Gtk.Switch({
            active: customDirEnabled
        });
        this._customDirSwitch.connect('notify::active',
                                      Lang.bind(this, this._onCustomDirSwitchToggled));
        hbox.pack_start(this._customDirSwitch, false, false, 0);
        this.pack_start(hbox, false, false, 0);


        this._customDirEntry = new Gtk.Entry({
            sensitive: customDirEnabled,
            text: customDir
        });
        this._customDirEntry.connect('notify::text',
                                     Lang.bind(this, this._onCustomDirEntryChanged));
        this.pack_start(this._customDirEntry, false, false, 5);
    },

    _onCustomDirSwitchToggled: function(sw) {
        this._settings.set_boolean(SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY, sw.active);
        this._customDirEntry.sensitive = sw.active;
    },

    _onCustomDirEntryChanged: function(entry) {
        this._settings.set_string(SETTINGS_CUSTOM_SOCKET_DIR_KEY, entry.text);
    }
});

function buildPrefsWidget() {
    let widget = new EmacsManagerSettingsWidget();
    widget.show_all();
    return widget;
}

function init() {
}
