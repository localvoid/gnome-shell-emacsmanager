const Gio = imports.gi.Gio;

function eachFile(path, fn, ctx) {
    let f = Gio.File.new_for_path(path);

    if (f.query_exists(null)) {
        let enumerator = f.enumerate_children('standard::name',
                                              Gio.FileQueryInfoFlags.NONE,
                                              null);

        let finfo = enumerator.next_file(null);

        while (finfo !== null) {
            fn.call(ctx, finfo);
            finfo = enumerator.next_file(null);
        }
    }
}
