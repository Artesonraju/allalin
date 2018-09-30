# Allalin

Presentation framework built with Clojurescript, Rum and Figwheel.

[See the presentation demo.](https://artesonraju.github.io/allalin/)

## Usage

Allalin only works on browsers supporting the fetch API and grid layout.

Download and extract the allalin.zip archive.
Open index.html in a browser to see the default presentation.
Edit config.edn to build your presentation.
Once you changed and saved the configuration, press R to load your changes.

## Development

To get an interactive development environment run:

    lein fig:build

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL.
To clean all compiled files:

	lein clean

To create a production build run:

	lein clean
	lein fig:min

## License

Copyright © 2018 Étienne Molto

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
