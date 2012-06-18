const Lang = imports.lang;
const GLib = imports.gi.GLib;
const Clutter = imports.gi.Clutter;
const St = imports.gi.St;
const Gio = imports.gi.Gio;
const Util = imports.misc.util;
const Main = imports.ui.main;
const Tweener = imports.ui.tweener;
const PanelMenu = imports.ui.panelMenu;
const ModalDialog = imports.ui.modalDialog;
const PopupMenu = imports.ui.popupMenu;

let em;
let emDialog;
let serversDir;

const EmacsMenuItem = new Lang.Class({
    Name: 'EmacsManager.EmacsMenuItem',
    Extends: PopupMenu.PopupBaseMenuItem,

    _init: function(name) {
        this.parent({
            reactive: false
        });

        this.name = name;

        let a = new PopupMenu.PopupMenuItem(name);
        this.addActor(a.actor, {expand: true});

        let b = new St.Button({
            child: new St.Icon({
                icon_name: 'edit-delete',
                icon_type: St.IconType.SYMBOLIC,
                icon_size: 22
            })
        });
        this.addActor(b);

        a.connect('activate', Lang.bind(this, this._onStartClient));
        b.connect('clicked', Lang.bind(this, this._onKillServer));
    },

    _onStartClient: function(e) {
        this.emit('start-client', { name: this.name });
    },

    _onKillServer: function(e) {
        this.emit('kill-server', { name: this.name });
    }
});


const EmacsStatusButton = new Lang.Class({
    Name: 'EmacsManager.EmacsStatusButton',
    Extends: PanelMenu.SystemStatusButton,

    _init: function() {
        this.parent('accessories-text-editor');

        this._contentSection = new PopupMenu.PopupMenuSection();
        this.menu.addMenuItem(this._contentSection);

        this.menu.addMenuItem(new PopupMenu.PopupSeparatorMenuItem());
        this.menu.addAction(_("Start emacs server"),
                            Lang.bind(this, this._onStartServer));

        this.menu.connect('open-state-changed', Lang.bind(this, this._update));

        this._update();
    },

    _onStartServer: function() {
        emDialog.open();
    },

    _onStartClient: function(e) {
        this.menu.close();
        Util.spawn(['emacsclient',
                    '-c',
                    '-n',
                    '-s', e.name])
    },

    _onKillServer: function(e) {
        this.menu.close();
        Util.spawn(['emacsclient',
                    '-s', e.name,
                    '-e', '(kill-emacs)'])
    },

    _update: function(e) {
        if (e) {
            this._contentSection.removeAll();
            let file,
                info,
                fileEnum;

            try {
                fileEnum = serversDir.enumerate_children('standard::*',
                                                         Gio.FileQueryInfoFlags.NONE,
                                                         null);
            } catch (e) {
                return;
            }

            while ((info = fileEnum.next_file(null)) != null) {
                let name = info.get_name();
                let item = new EmacsMenuItem(name, this);
                item.connect('start-client', Lang.bind(this, this._onStartClient));
                item.connect('kill-server', Lang.bind(this, this._onKillServer));
                this._contentSection.addMenuItem(item);
            }
            fileEnum.close(null);
        }
    }
});


const EmacsRunDialog = new Lang.Class({
    Name: 'EmacsManager.EmacsRunDialog',
    Extends: ModalDialog.ModalDialog,

    _init: function() {
        let label,
            entry,
            errorIcon;

        this.parent({ styleClass: 'run-dialog' });

        label = new St.Label({
            style_class: 'run-dialog-label',
            text: _("Please enter emacs server name:")
        });
        this.contentLayout.add(label, { y_align: St.Align.START });

        entry = new St.Entry({ style_class: 'run-dialog-entry' });
        entry.label_actor = label;
        this._entryText = entry.clutter_text;
        this.contentLayout.add(entry, { y_align: St.Align.START });
        this.setInitialKeyFocus(this._entryText);

        this._errorBox = new St.BoxLayout({
            style_class: 'run-dialog-error-box'
        });

        this.contentLayout.add(this._errorBox, { expand: true });

        errorIcon = new St.Icon({
            icon_name: 'dialog-error',
            icon_size: 24,
            style_class: 'run-dialog-error-icon'
        });

        this._errorBox.add(errorIcon, { y_align: St.Align.MIDDLE });

        this._commandError = false;

        this._errorMessage = new St.Label({
            style_class: 'run-dialog-error-label'
        });
        this._errorMessage.clutter_text.line_wrap = true;

        this._errorBox.add(this._errorMessage, {
            expand: true,
            y_align: St.Align.MIDDLE,
            y_fill: false
        });

        this._errorBox.hide();

        this._entryText.connect('key-press-event',
                                Lang.bind(this, this._onKeyPress));
    },

    _onKeyPress: function(o, e) {
        let sym = e.get_key_symbol();

        if (sym == Clutter.Return || sym == Clutter.KP_Enter) {
            this.popModal();
            this._run(o.get_text());
            if (!this._commandError) {
                this.close();
            } else {
                if (!this.pushModal())
                    this.close();
            }
            return true;
        } else if (sym == Clutter.Escape) {
            this.close();
            return true;
        }
        return false;
    },

    _run : function(input) {
        if (input) {
            try {
                Util.spawn(['emacs', '--daemon=' + input])
            } catch (e) {
                this._showError(e.message);
            }
        }
    },
    _showError : function(message) {
        this._commandError = true;
        this._errorMessage.set_text(message);

        if (!this._errorBox.visible) {
            let [errorBoxMinHeight, errorBoxNaturalHeight] =
                this._errorBox.get_preferred_height(-1);

            let parentActor = this._errorBox.get_parent();
            Tweener.addTween(parentActor, {
                height: parentActor.height + errorBoxNaturalHeight,
                time: DIALOG_GROW_TIME,
                transition: 'easeOutQuad',
                onComplete: Lang.bind(this,
                                      function() {
                                          parentActor.set_height(-1);
                                          this._errorBox.show();
                                      })
            });
        }
    },
    open: function() {
        this._errorBox.hide();
        this._entryText.set_text('');
        this._commandError = false;
        this.parent();
    }
});


function enable() {
    let ret = GLib.spawn_sync(null, ['/usr/bin/id', '-u'], null, 0, null);
    let uid = (''+ret[1]).replace(/\s+$/, '');
    serversDir = Gio.file_new_for_path('/tmp/emacs' + uid)

    em = new EmacsStatusButton();
    emDialog = new EmacsRunDialog();
    Main.panel.addToStatusArea('emacs-launcher', em)
}

function disable() {
    serversDir = undefined;

    em.destroy();
    emDialog.destroy();
}

function init() {
}
