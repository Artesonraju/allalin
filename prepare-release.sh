#!/bin/bash

rm allalin.zip
rm -rf ./docs
lein do clean, fig:min
cp -r ./resources/public/. ./dist
cp ./target/public/cljs-out/dev-main.js ./dist/min.js
cd ./dist
sed -i -e 's|cljs-out/dev-main.js|min.js|g' ./index.html
rm test.html
zip -r ../allalin.zip ./*
cd ..
cp -r ./dist ./docs
rm -r ./dist