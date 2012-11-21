// -*- mode: js; flymake-mode: -1; js-indent-level: 4; indent-tabs-mode: nil -*-
const Gio = imports.gi.Gio

function eachFile(path, fn, ctx) {
    let f = Gio.File.new_for_path(path);

    if (f.query_exists(null)) {
        let enumerator = f.enumerate_children('standard::name',
                                              Gio.FileQueryInfoFlags.NONE,
                                              null)
          , finfo;

        while ((finfo = enumerator.next_file(null)) !== null)
            fn.call(ctx, finfo);
    }
}
