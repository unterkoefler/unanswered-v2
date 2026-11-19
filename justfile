set shell := ["nu", "-c"]

default:
  @just --list

alias s := start
start:
  cd generator; gleam dev

alias b := build
build:
  cd search_tool; just build
  cd tag_browser; just build
  cd random; just build
  cd generator; gleam run


push-to-neocities:
  neocities push dist

new slug:
    jj new -m "{{slug}}" main
    let f = bin/new {{slug}}; vim $f
