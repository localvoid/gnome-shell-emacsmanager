const Signals = imports.signals;
const Lang = imports.lang;
const Tweener = imports.ui.tweener;
const PanelMenu = imports.ui.panelMenu;
const ModalDialog = imports.ui.modalDialog;
const PopupMenu = imports.ui.popupMenu;
const Main = imports.ui.main;
const Pango = imports.gi.Pango;
const St = imports.gi.St;
const Clutter = imports.gi.Clutter;

const DIALOG_GROW_TIME = 0.1;


const RunDialog = new Lang.Class({
    Name: 'EmacsManager.RunDialog',
    Extends: ModalDialog.ModalDialog,

    _init: function(emacsManager, completer) {
        this.parent({ styleClass: 'run-dialog' });

        this._emacsManager = emacsManager;
        this._completer = completer;

        let label = new St.Label({
            style_class: 'run-dialog-label',
            text: _("Please enter emacs server name:")
        });
        this.contentLayout.add(label, { y_align: St.Align.START });

        let entry = new St.Entry({ style_class: 'run-dialog-entry' });
        entry.label_actor = label;
        this._entryText = entry.clutter_text;
        this.contentLayout.add(entry, { y_align: St.Align.START });
        this.setInitialKeyFocus(this._entryText);

        this._errorBox = new St.BoxLayout({
            style_class: 'run-dialog-error-box'
        });

        this.contentLayout.add(this._errorBox, { expand: true });

        let errorIcon = new St.Icon({
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
                                this._onKeyPress.bind(this));
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
        } else if (sym == Clutter.Tab) {
            let text = o.get_text(),
                postfix = this._getCompletion(text);

            if (postfix != null && postfix.length > 0) {
                o.insert_text(postfix, -1);
                o.set_cursor_position(text.length + postfix.length);
            }
            return true;
        }
        return false;
    },

    _getCompletion: function(text) {
        return this._completer.getCompletion(text);
    },

    _run : function(input) {
        this._commandError = false;

        if (input) {
            try{
                this._emacsManager.startServer(input);
            } catch (e) {
                this._showError(e.message);
            }
        }
    },

    _showError : function(message) {
        this._commandError = true;
        this._errorMessage.set_text(message);

        if (!this._errorBox.visible) {
            let [errorBoxMinHeight, errorBoxNaturalHeight] = this._errorBox.get_preferred_height(-1);

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
    },

    destroy: function() {
        this._completer.destroy();
        this.parent();
    }
});


const ServerView = new Lang.Class({
    Name: 'EmacsManager.ServerView',
    Extends: PopupMenu.PopupBaseMenuItem,

    _init: function(server) {
        this.parent();

        this.server = server;
        server.connect('changed', this._onChanged.bind(this));

        let nameLabel = new St.Label({
            text: server.name,
            style_class: 'emacs-manager-menu-item-name'
        });

        this.addActor(nameLabel, { expand: true });

        let killButton = new St.Button({
            child: new St.Icon({
                icon_name: 'window-close-symbolic',
                icon_size: 16,
                style_class: 'emacs-manager-menu-item-kill-server-icon'
            })
        });
        killButton.connect('clicked', this._onKill.bind(this));
        this.addActor(killButton, { span: -1, align: St.Align.END });

        this.connect('activate', this._onActivate.bind(this));
    },

    _onKill: function(e) {
        this.server.kill();
    },

    _onChanged: function() {
        if (this.server.state == 'RUNNING') {
            this.setSensitive(true);
        } else {
            this.setSensitive(false);
        }
    },

    _onActivate: function(e, c) {
        this.server.startClient();
    }
});

const ServerListView = new Lang.Class({
    Name: 'EmacsManager.ServerListView',
    Extends: PopupMenu.PopupMenuSection,

    _init: function(emacsManager) {
        this.parent();

        this.serverCount = 0;
        this._serverViews = {};

        emacsManager.connect('server-created',
                             this._onServerCreated.bind(this));
        emacsManager.connect('server-deleted',
                             this._onServerDeleted.bind(this));
    },

    _createServerView: function(srv) {
        this.serverCount += 1;
        let v = new ServerView(srv);
        this._serverViews[srv.name] = v;
        this.addMenuItem(v);
        if (this.serverCount === 1) {
            this.emit('empty', false);
        }
    },

    _deleteServerView: function(srv) {
        this.serverCount -= 1;
        this._serverViews[srv.name].destroy();
        delete this._serverViews[srv];
        if (this.serverCount === 0) {
            this.emit('empty', true);
        }
    },

    _onServerCreated: function(source, srv) {
        this._createServerView(srv);
    },

    _onServerDeleted: function(source, srv) {
        this._deleteServerView(srv);
    }
});


const MenuView = new Lang.Class({
    Name: 'EmacsManager.MenuView',
    Extends: PopupMenu.PopupMenuSection,

    _init: function(mainView) {
        this.parent();
        this._mainView = mainView;
        this.addAction('Start server', this._onStartServer.bind(this));
    },

    _onStartServer: function() {
        this._mainView.popupStartServerDialog();
    }
});


const StatusButton = new Lang.Class({
    Name: 'EmacsManager.StatusButton',
    Extends: PanelMenu.SystemStatusButton,

    _init: function(mainView, emacsManager) {
        this.parent('accessories-text-editor-symbolic');

        this._serverListView = new ServerListView(emacsManager);
        this._menuView = new MenuView(mainView);

        this._serverListView.connect('empty', this._onEmpty.bind(this));

        this.menu.addMenuItem(this._serverListView);
        if (this._serverListView.serverCount > 0) {
            this._separator = new PopupMenu.PopupSeparatorMenuItem();
            this.menu.addMenuItem(this._separator, 1);
        }
        this.menu.addMenuItem(this._menuView);
    },

    _onEmpty: function(source, isEmpty) {
        if (isEmpty) {
            if (this._separator) {
                this._separator.destroy();
            }
        } else {
            this._separator = new PopupMenu.PopupSeparatorMenuItem();
            this.menu.addMenuItem(this._separator, 1);
        }
    }
});

const View = new Lang.Class({
    Name: 'EmacsManager.View',

    _init: function(emacsManager, runCompleter) {
        this._runDialog = new RunDialog(emacsManager, runCompleter);
        this._statusButton = new StatusButton(this, emacsManager);

        Main.panel.addToStatusArea('emacs-manager', this._statusButton)
    },

    popupStartServerDialog: function() {
        this._runDialog.open();
    },

    destroy: function() {
        this._statusButton.destroy();
        this._runDialog.destroy();
    }
});
