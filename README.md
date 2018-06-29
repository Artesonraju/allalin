# allalin

Presentation framework built with Clojurescript, Rum and Figwheel

## Overview

Needs a recent browser (supporting the fetch API and flexbox)

## Roadmap

### Global
- [x] Load edn conf and data file
- [x] Error management
- [X] Page display
- [x] Controls
- [x] Configuration reload
- [ ] Configuration live edition

### Conf
- [x] Header
- [x] Footer
- [x] Aside
- [x] Icon and title
- [x] Background Image

### Content
- [x] Titles
- [x] Image
- [x] Styled Text
- [x] Code
- [x] Fragments
- [x] Steps
- [x] Page number
- [x] Section
- [ ] Table
- [ ] Lists

### Style
- [x] Responsive CSS
- [x] Transition
- [ ] Default style
- [ ] Custom style

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Copyright © 2018 Étienne Molto

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
