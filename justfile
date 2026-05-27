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

deploy:
  jj bookmark set main -r @
  jj new -m "build"
  jj restore --from deploy-base .gitignore
  just build; mv dist docs
  jj bookmark set deploy -r @ --allow-backwards
  jj new main
  rm -rf docs

new slug:
    jj new -m "{{slug}}"
    let f = bin/new {{slug}}; vim $f
