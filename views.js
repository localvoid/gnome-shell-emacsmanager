const Lang = imports.lang;
const Signals = imports.signals;

const GObject = imports.gi.GObject;
const Pango = imports.gi.Pango;
const Clutter = imports.gi.Clutter;
const St = imports.gi.St;

const Tweener = imports.ui.tweener;
const PanelMenu = imports.ui.panelMenu;
const ModalDialog = imports.ui.modalDialog;
const ShellEntry = imports.ui.shellEntry;
const PopupMenu = imports.ui.popupMenu;
const Main = imports.ui.main;


const DIALOG_GROW_TIME = 0.1;


const RunDialog = class extends ModalDialog.ModalDialog {

    constructor(emacsManager, completer) {
        super({ styleClass: 'run-dialog',
                      destroyOnClose: false });

        this._emacsManager = emacsManager;
        this._completer = completer;

        let label = new St.Label({
            style_class: 'run-dialog-label',
            text: _('Enter emacs server name')
        });

        this.contentLayout.add(label, { x_fill: false,
                                        x_align: St.Align.START,
                                        y_align: St.Align.START });

        let entry = new St.Entry({ style_class: 'run-dialog-entry',
                                   can_focus: true });
        ShellEntry.addContextMenu(entry);
        entry.label_actor = label;
        this._entryText = entry.clutter_text;
        this.contentLayout.add(entry, { y_align: St.Align.START });
        this.setInitialKeyFocus(this._entryText);

        this._errorBox = new St.BoxLayout({
            style_class: 'run-dialog-error-box'
        });

        this.contentLayout.add(this._errorBox, { expand: true });

        let errorIcon = new St.Icon({
            icon_name: 'dialog-error-symbolic',
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
            x_align: St.Align.START,
            x_fill: false,
            y_align: St.Align.MIDDLE,
            y_fill: false,
        });

        this._errorBox.hide();

        this._entryText.connect('key-press-event',
                                this._onKeyPress.bind(this));

        this.setButtons([{ action: this.close.bind(this),
                           label: _('Close'),
                           key: Clutter.Escape }]);
    }

    _onKeyPress(o, e) {
        let symbol = e.get_key_symbol();

        if (symbol == Clutter.Return || symbol == Clutter.KP_Enter) {
            this.popModal();
            this._run(o.get_text());
            if (!this._commandError || !thus.pushModal()) {
                this.close();
            }
            return Clutter.EVENT_STOP;
        } else if (sym == Clutter.Tab) {
            let text = o.get_text();
            let prefix;

            if (text.lastIndexOf(' ') === -1) {
                prefix = text;
            } else {
                prefix = text.substr(text.lastIndexOf(' ') + 1);
            }

            let postfix = this._getCompletion(prefix);
            if (postfix !== null && postfix.length > 0) {
                o.insert_text(postfix, -1);
                o.set_cursor_position(text.length + postfix.length);
            }
            return Clutter.EVENT_STOP;
        }
        return Clutter.EVENT_PROPAGATE;
    }

    _getCompletion(text) {
        return this._completer.getCompletion(text);
    }

    _run(input) {
        this._commandError = false;

        if (input) {
            try {
                this._emacsManager.startServer(input);
            } catch (e) {
                this._showError(e.message);
            }
        }
    }

    _showError(message) {
        this._commandError = true;
        this._errorMessage.set_text(message);

        if (!this._errorBox.visible) {
            let [errorBoxMinHeight, errorBoxNaturalHeight]
                    = this._errorBox.get_preferred_height(-1);

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
    }

    open() {
        this._errorBox.hide();
        this._entryText.set_text('');
        this._commandError = false;
        super.open();
    }
};

const RemoteServerView = class extends PopupMenu.PopupBaseMenuItem {

    constructor(server) {
        super();

        this.server = server;

        this.actor.add(new St.Label({
            text: server.name,
            style_class: 'emacs-manager_remote-server-name'
        }), { expand: true });

        this.actor.add(new St.Label({
            text: server.host,
            style_class: 'emacs-manager_remote-server-host'
        }));

        this.connect('activate', this._onActivate.bind(this));
    }

    _onActivate(e, c) {
        this.server.startClient();
    }
};

const ServerView = class extends PopupMenu.PopupBaseMenuItem {

    constructor(server) {
        super();

        this.server = server;
        server.connect('state-changed', this._onStateChanged.bind(this));
        this._syncState();

        let nameLabel = new St.Label({
            text: server.name,
            style_class: 'emacs-manager_server-name'
        });

        this.actor.add(nameLabel, { expand: true });

        let killButton = new St.Button({
            child: new St.Icon({
                icon_name: 'window-close-symbolic',
                icon_size: 16,
                style_class: 'emacs-manager_kill-server-icon'
            })
        });
        killButton.connect('clicked', this._onKill.bind(this));
        this.actor.add(killButton, { span: -1, align: St.Align.END });

        this.connect('activate', this._onActivate.bind(this));
    }

    _onKill(e) {
        this.server.kill();
    }

    _syncState() {
        if (this.server.state === 'RUNNING')
            this.setSensitive(true);
        else
            this.setSensitive(false);
    }

    _onStateChanged(source, state) {
        this._syncState();
    }

    _onActivate(e, c) {
        this.server.startClient();
    }
};

const RemoteServerListView = class extends PopupMenu.PopupMenuSection {

    constructor(emacsManager) {
        super();

        this.serverCount = 0;
        this._serverViews = {};

        emacsManager.connect('remote-server-created',
                             this._onServerCreated.bind(this));
        emacsManager.connect('remote-server-deleted',
                             this._onServerDeleted.bind(this));

        let servers = emacsManager._remoteServers;
        for (let name in servers) {
            let s = servers[name];
            this._createServer(s);
        }
    }

    _createServer(srv) {
        this.serverCount += 1;
        let v = new RemoteServerView(srv);
        this._serverViews[srv.name] = v;
        this.addMenuItem(v);
        if (this.serverCount === 1)
            this.emit('empty', false);
    }

    _onServerCreated(source, srv) {
        this._createServer(srv);
    }

    _onServerDeleted(source, srv) {
        this.serverCount -= 1;
        this._serverViews[srv.name].destroy();
        delete this._serverViews[srv];
        if (this.serverCount === 0)
            this.emit('empty', true);
    }
};

const ServerListView = class extends PopupMenu.PopupMenuSection {

    constructor(emacsManager) {
        super();

        this.serverCount = 0;
        this._serverViews = {};

        emacsManager.connect('server-created',
                             this._onServerCreated.bind(this));
        emacsManager.connect('server-deleted',
                             this._onServerDeleted.bind(this));

        let servers = emacsManager._servers;
        for (let name in servers) {
            let s = servers[name];
            this._createServer(s);
        }
    }

    _createServer(srv) {
        this.serverCount += 1;
        let v = new ServerView(srv);
        this._serverViews[srv.name] = v;
        this.addMenuItem(v);
        if (this.serverCount === 1)
            this.emit('empty', false);
    }

    _onServerCreated(source, srv) {
        this._createServer(srv);
    }

    _onServerDeleted(source, srv) {
        this.serverCount -= 1;
        this._serverViews[srv.name].destroy();
        delete this._serverViews[srv];
        if (this.serverCount === 0)
            this.emit('empty', true);
    }
};


const MenuView = class extends PopupMenu.PopupMenuSection {

    constructor(mainView) {
        super();
        this._mainView = mainView;
        this.addAction('Start server', this._onStartServer.bind(this));
    }

    _onStartServer() {
        this._mainView.popupStartServerDialog();
    }
};


const StatusButton = GObject.registerClass(class EmacsManagerStatusButton extends PanelMenu.Button {

    _init(mainView, emacsManager) {
        super._init(0.0, _('Emacs Manager'));

        this._hbox = new St.BoxLayout({ style_class: 'panel-status-menu-box' });
        this._hbox.add_child(new St.Icon({ style_class: 'system-status-icon',
                                           icon_name: 'accessories-text-editor-symbolic' }));
        this._hbox.add_child(new St.Label({ text: '\u25BE',
                                            y_expand: true,
                                            y_align: Clutter.ActorAlign.CENTER }));
        this.actor.add_child(this._hbox);

        this._serverListView = new ServerListView(emacsManager);
        this._remoteServerListView = new RemoteServerListView(emacsManager);
        this._menuView = new MenuView(mainView);
        this._separator = null;

        this._serverListView.connect('empty', this._onEmpty.bind(this));
        this._remoteServerListView.connect('empty', this._onEmpty.bind(this));

        this.menu.addMenuItem(this._serverListView);
        this.menu.addMenuItem(this._remoteServerListView);

        this._syncEmpty();

        this.menu.addMenuItem(this._menuView);
    }

    _syncEmpty() {
        if (this._serverListView.serverCount > 0 ||
            this._remoteServerListView.serverCount > 0) {
            if (!this._separator) {
                this._separator = new PopupMenu.PopupSeparatorMenuItem();
                this.menu.addMenuItem(this._separator, 2);
            }
        } else if (this._serverListView.serverCount === 0 &&
                   this._remoteServerListView.serverCount === 0) {
            if (this._separator) {
                this._separator.destroy();
                this._separator = null;
            }
        }
    }

    _onEmpty(source, isEmpty) {
        this._syncEmpty();
    }
});

const View = class {

    constructor(emacsManager, runCompleter) {
        this._runDialog = new RunDialog(emacsManager, runCompleter);
        this._statusButton = new StatusButton(this, emacsManager);

        Main.panel.addToStatusArea('emacs-manager', this._statusButton);
    }

    popupStartServerDialog() {
        this._runDialog.open();
    }

    destroy() {
        this._statusButton.destroy();
        this._runDialog.destroy();
    }
};
