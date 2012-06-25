const Lang = imports.lang;
const Gtk = imports.gi.Gtk;
const GObject = imports.gi.GObject;

const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();
const Convenience = Me.imports.convenience;

const SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY = 'custom-socket-dir-enabled';
const SETTINGS_CUSTOM_SOCKET_DIR_KEY = 'custom-socket-dir';
const SETTINGS_VIRTUAL_ENVIRONMENT_DIR_KEY = 'virtual-environment-dir';

const CustomDirField = new GObject.Class({
    Name: 'EmacsManager.Prefs.CustomDirField',
    GTypeName: 'CustomDirField',
    Extends: Gtk.Box,
    _init: function(settings) {
        this.parent({
            orientation: Gtk.Orientation.VERTICAL,
        });
        this._settings = settings;

        let customDirEnabled = this._settings.get_boolean(SETTINGS_CUSTOM_SOCKET_DIR_ENABLED_KEY),
            customDir = this._settings.get_string(SETTINGS_CUSTOM_SOCKET_DIR_KEY);

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

const VirtualEnvDirField = new GObject.Class({
    Name: 'EmacsManager.Prefs.VirtualEnvDirField',
    GTypeName: 'VirtualEnvDirField',
    Extends: Gtk.Box,
    _init: function(settings) {
        this.parent({
            orientation: Gtk.Orientation.VERTICAL,
        });
        this._settings = settings;

        let virtualEnvDir = this._settings.get_string(SETTINGS_VIRTUAL_ENVIRONMENT_DIR_KEY);
        this.pack_start(new Gtk.Label({
            label: "<b>Virtual Environments Directory</b>",
            use_markup: true,
            xalign: 0
        }), false, false, 0);


        this._virtualEnvDirEntry = new Gtk.Entry({
            text: virtualEnvDir
        });
        this._virtualEnvDirEntry.connect('notify::text',
                                         Lang.bind(this, this._onVirtualEnvDirEntryChanged));
        this.pack_start(this._virtualEnvDirEntry, false, false, 0);
    },

    _onVirtualEnvDirEntryChanged: function(entry) {
        this._settings.set_string(SETTINGS_VIRTUAL_ENVIRONMENT_DIR_KEY, entry.text);
    }
});

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

        this.pack_start(new CustomDirField(this._settings), false, false, 5);
        this.pack_start(new VirtualEnvDirField(this._settings), false, false, 5);
    }
});

function buildPrefsWidget() {
    let widget = new EmacsManagerSettingsWidget();
    widget.show_all();
    return widget;
}

function init() {
}
