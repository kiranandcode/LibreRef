# LibreRef

 <img src="https://gitlab.com/gopiandcode/libre-ref/-/raw/master/resources/libre-ref-logo.png" width="20%" height="20%">

> LibreRef is a free as in freedom digital referencing tool for artists.

Usage: 
   - Middle-mouse to move view
   - Left click to select images
   - Right click to open menu
   - Drag and drop images into display

# Download

# Screenshots

# Building from Source

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
