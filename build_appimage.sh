#!/bin/bash -x

# Delete old appdir if exists
rm -r -f ./AppDir

# build appdir
opam exec -- dune install --prefix=usr --destdir=AppDir

if [ $? -ne 0 ]; then
    echo "Could not build libre-ref.exe. Make sure you can build it."
    exit 255
fi

# run linux deploy to collect all libraries
linuxdeploy-x86_64.AppImage --appdir ./AppDir \
                            --desktop-file ./libre-ref.desktop \
                            --icon-file ./resources/libre-ref-logo.png \
                            --icon-filename libre-ref

# package into an appimage
appimagetool-x86_64.AppImage ./AppDir

# clean up
rm -r -f ./AppDir
