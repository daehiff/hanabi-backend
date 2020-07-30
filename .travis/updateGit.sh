#!/bin/sh

update_gitignore() {
  mv -f ./.travis/.gitignore .gitignore
}

cd $TRAVIS_BUILD_DIR
update_gitignore
