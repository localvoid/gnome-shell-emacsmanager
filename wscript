#!/usr/bin/env python

import os

UUID = 'emacsmanager@localvoid.gmail.com'

def configure(cfg):
    cfg.env['EXTENSIONSDIR'] = os.path.join('share', 'gnome-shell', 'extensions')

def build(bld):
    ext_path = os.path.join(bld.env['EXTENSIONSDIR'], UUID)
    bld.install_files(ext_path, [
            'metadata.json',
            'views.js',
            'autocomplete.js',
            'extension.js',
            ])
