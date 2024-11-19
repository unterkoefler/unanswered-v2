default:
  @just --list

alias i := install
install:
  npm install

alias s := start
start:
  npm run start

alias b := build
build:
  npm run build
  rm -R docs
  mv dist docs

deploy: build
  git checkout deploy
  git add docs
  git commit -m "deploy"
  git push
  git checkout main

new slug:
    bin/new {{slug}}
