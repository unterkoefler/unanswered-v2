import blogatto/post.{type Post, Post}
import gleam/int
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/time/timestamp
import gleam/result
import gleam/json
import gleam/list
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import plinth/browser/document
import plinth/browser/element as plinth_element

pub fn main() -> Nil {
  let assert Ok(el) = document.query_selector("#lustre-data")
  let txt = plinth_element.inner_text(el)
  let assert Ok(posts_by_tag) = json.parse(txt, posts_by_tag_decoder())

  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", posts_by_tag)

  Nil
}

fn posts_by_tag_decoder() -> decode.Decoder(dict.Dict(String, List(Post(Nil)))) {
  decode.dict(decode.string, decode.list(post_decoder()))
}

fn post_decoder() -> decode.Decoder(Post(Nil)) {
  use title <- decode.field("title", decode.string)
  use slug <- decode.field("slug", decode.string)
  use url <- decode.field("url", decode.string)
  use seconds <- decode.field("date", decode.int)
  use description <- decode.field("description", decode.string)
  use excerpt <- decode.field("excerpt", decode.string)
  use language <- decode.field("language", decode.optional(decode.string))
  use featured_image <- decode.field("featured_image", decode.optional(decode.string))
  let contents = [] // TODO: will need full text for search
  let extras = dict.new() // TODO: maybe parse the tags here instead?

  decode.success(
    Post(
      title:,
      slug:,
      url:,
      date: timestamp.from_unix_seconds(seconds),
      description:,
      excerpt:,
      language:,
      featured_image:,
      contents:,
      extras:,
    )
  )
}


type Model {
  Model(
    posts_by_tag: Dict(String, List(Post(Nil))),
  )
}

fn init(posts_by_tag: dict.Dict(String, List(Post(Nil)))) -> Model {
  Model(posts_by_tag:)
}

type Msg {
  NoOp
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    NoOp -> model
  }
}

fn view(model: Model) -> Element(Msg) {
  html.div([
  ], dict.fold(model.posts_by_tag, [], fn(acc: List(Element(Msg)), k: String, v: List(Post(Nil))) {
    [view_tag(k, v), ..acc]
  }))
}

fn view_tag(tag: String, posts: List(Post(Nil))) -> Element(Msg) {
  let titles = list.map(posts, fn(p: Post(Nil)) {
    html.p([], [element.text(p.title)])
  })
  html.div([
  ], [
    html.h1([
    ], [
      element.text(tag),
      element.text(" (" <> int.to_string(list.length(titles)) <> ")"),
    ]),
    ..titles
  ])
}
