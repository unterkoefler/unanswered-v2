set shell := ["nu", "-c"]

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

deploy: build
  git checkout deploy
  rm -rf docs
  mv dist docs
  git add docs
  git commit -m "deploy"
  git push
  git checkout main

new slug:
    let f = bin/new {{slug}}; vim $f
