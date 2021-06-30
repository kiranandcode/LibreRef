# LibreRef
***NOTE: This is a M$ github mirror of LibreRef's main repository which is hosted on Gitlab. Please see the Gitlab version to keep up to date with the latest version of LibreRef.***

 <img
 src="https://raw.githubusercontent.com/Gopiandcode/LibreRef/master/resources/libre-ref-logo.png"
 width="20%" height="20%">

> LibreRef is a free as in freedom digital referencing tool for artists.

Usage: 
   - Middle-mouse to move view
   - Left click to select images
   - Right click to open menu
   - Drag and drop images into display

# Download

LibreRef is distributed as an AppImage for easy use: 

 - X86_64: [libre-ref-x86_64](https://github.com/Gopiandcode/LibreRef/releases/download/v1.0.0-GH/libre-ref-x86_64.AppImage)

# Screenshots

<img src="https://raw.githubusercontent.com/Gopiandcode/LibreRef/master/images/splash.png" width="30%" height="30%">

<img src="https://raw.githubusercontent.com/Gopiandcode/LibreRef/master/images/options.png" width="30%" height="30%">

<img src="https://raw.githubusercontent.com/Gopiandcode/LibreRef/master/images/scaling.png" width="30%" height="30%">

# Building from Source

To build this project from source, you will need to have the Opam OCaml package manager installed on your system, with a 4.12.0 OCaml compiler switch.

Make sure the following OCaml packages are installed (`opam install <package-name>`):

 - cmdliner 
 - data-encoding 
 - lablgtk3 
 - stb_image 
 - bos 
 - piaf

To build LibreRef from source, simply execute the following command from the project root:
```
opam exec -- dune build ./libre_ref.exe
```

Additionally, to produce a deployable AppImage file for LibreRef, simply run the `build_appimage.sh` script from the project root.

Note: you will have to have the linuxdeploy and appimagetool appimages on your path.

# Project Architecture
The LibreRef project is structured as follows:
```
./
|-- resources                         - application resources
|-- LICENSE                           - LICENSE for source code (AGPL)
|-- libre-ref.opam                    - opam project description
|-- dune                              - build tool (dune) project description
|-- dune-project
|-- build_appimage.sh                 - deployment script
|-- readme.md                         - readme (this document)
|
|                                     - application files:
|-- libre_ref.ml
|-- scene.ml
|-- gui.ml(i)
|-- config.ml
|
|                                     - library files:
|-- camera.ml
|-- error.ml
|-- image.ml
|-- serialized.ml
|-- quadtree.ml
|-- utils.ml
`-- web.ml
```

The source code files in this project can be broadly split into two categories:
  - *Application files* -- these are application specific files that
    form the core of Libre-ref's implementation and functionality
  - *Library files* -- these are small modular reusable (usually
    purely functional) components that provide generic features used
    by LibreRef.

To understand how LibreRef works, when initially exploring the
repository, you should stick to just the core application
implementation files, whereas the library files may be of interest to
be reused in other projects (under copy-left licenses of course).

Below, we provide a brief description of the purpose of each of the core application files:

- *libre_ref.ml* -- The core entry-point for the application. Handles
  parsing the command-line arguments and then starting the GUI
- *scene.ml* -- The functional logical core of libre-ref that
  represents a collection of scalable and movable images
- *gui.ml* -- The imperative~ish glue that hooks the functional core
  of libre-ref to the Cairo GUI library
- *config.ml* -- A small imperative/mutable component that
  encapsulates retrieving and saving config information to and from
  disk
