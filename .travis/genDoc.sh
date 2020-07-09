#!/bin/sh

setup_git() {
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis CI"
  git checkout -b $TRAVIS_BRANCH 
  git pull origin $TRAVIS_BRANCH 
}

generate_doc() {
  npm install -g apidoc
  make doc
}

upload_files() {
  git add static/doc
  git commit -m "Travis build: $TRAVIS_BUILD_NUMBER"
  git remote add origin https://travis:${GH_ACESS_TOKEN}@github.com/daehiff/hanabi-backend.git > /dev/null 2>&1
  git push origin $TRAVIS_BRANCH
}

cd $TRAVIS_BUILD_DIR
setup_git
generate_doc
upload_files