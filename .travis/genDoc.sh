#!/bin/sh

setup_git() {
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis CI"
}

generate_doc() {
  npm install -g apidoc
  cd $TRAVIS_BUILD_DIR
  make doc
}

upload_files() {
  git add static/doc
  git commit -m "Travis build: $TRAVIS_BUILD_NUMBER"
  git remote rm origin
  git remote add origin https://${GH_ACESS_TOKEN}@github.com/daehiff/hanabi-backend.git > /dev/null 2>&1
  git push origin $TRAVIS_BRANCH --quiet
}


setup_git
generate_doc
upload_files