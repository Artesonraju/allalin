#!/bin/bash

rm allalin.zip
rm -rf ./docs

lein do clean, cljsbuild once min

cd ./resources/public/
zip -r ../../allalin.zip ./*
cd ../..

cp -r ./resources/public/. ./docs
sed -i -e 's/; :disable-reload true/:disable-reload true/g' ./docs/config.edn
