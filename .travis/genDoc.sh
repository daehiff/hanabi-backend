#!/bin/sh

generate_doc() {
  npm install -g apidoc
  make doc
}

cd $TRAVIS_BUILD_DIR
generate_doc
