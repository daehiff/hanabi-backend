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
  git push
}


setup_git
commit_website_files
upload_files