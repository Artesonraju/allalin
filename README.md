# Allalin

Presentation framework built with Clojurescript, Rum and Figwheel.

[See the presentation demo.](https://artesonraju.github.io/allalin/)

## Usage

Allalin only works on browsers supporting the fetch API and flexbox.

Download and extract the allalin.zip archive.
Open index.html in a browser to see the default presentation.
Edit config.edn to build your presentation.
Once you changed and saved the configuration, press R to load your changes.

## Development

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

## Improvement ideas (in no particular order)

- Document configuration
- Fix fonts not really responsive
- Responsive sections borders
- Better config errors : print readable cause
- In browser config edition
- Touch events
- Print mode
- Hide mode
- Wysiwig editor
- Progress bar
- Speaker view (clock and notes)
- Better slide transitions

## License

Copyright © 2018 Étienne Molto

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
