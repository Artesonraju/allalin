# allalin

Presentation framework built with Clojurescript, Rum and Figwheel

## Overview

Needs a recent browser (supporting the fetch API)

## Roadmap

### Global
- [x] Load edn conf and data file
- [ ] Error management
- [ ] Page display
- [ ] Controls
- [ ] Usable as a framework

### Conf
- [ ] Header
- [ ] Footer
- [ ] Icon and title

### Content
- [ ] Title
- [ ] Image
- [ ] Text
- [ ] Code
- [ ] Fragments
- [ ] Steps
- [ ] Table

### Style
- [ ] Responsive CSS
- [ ] Transition
- [ ] Default style
- [ ] Custom style

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Copyright © 2018 Étienne Molto

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
