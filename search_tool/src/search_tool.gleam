import blogatto/post.{type Post, Post}
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/option
import gleam/time/timestamp
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/effect.{type Effect}
import lustre/event
import plinth/browser/document
import plinth/browser/element as plinth_element

pub fn main() -> Nil {
  let assert Ok(el) = document.query_selector("#lustre-data")
  let txt = plinth_element.inner_text(el)
  let assert Ok(posts) = json.parse(txt, posts_decoder())

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", posts)

  Nil
}

fn posts_decoder() -> decode.Decoder(List(Post(Nil))) {
  decode.list(post_decoder())
}

fn post_decoder() -> decode.Decoder(Post(Nil)) {
  use title <- decode.field("title", decode.string)
  use slug <- decode.field("slug", decode.string)
  use url <- decode.field("url", decode.string)
  use seconds <- decode.field("date", decode.int)
  use description <- decode.field("description", decode.string)
  use excerpt <- decode.field("excerpt", decode.string)
  use language <- decode.field("language", decode.optional(decode.string))
  use featured_image <- decode.field(
    "featured_image",
    decode.optional(decode.string),
  )
  let contents = []
  // TODO: will need full text for search
  let extras = dict.new()
  // TODO: maybe parse the tags here instead?

  decode.success(Post(
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
  ))
}

type Model {
  Model(
    posts: List(Post(Nil)),
    search_term: option.Option(String),
  )
}

fn init(posts: List(Post(Nil))) -> #(Model, Effect(Msg)) {
  #(Model(posts:, search_term: option.None), effect.none())
}

type Msg {
  UpdateSearchTerm(search_term: String)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UpdateSearchTerm(search_term) -> {
      let new_search = case search_term {
        "" -> option.None
        _ -> option.Some(search_term)
      }
      #(Model(..model, search_term: new_search), effect.none())
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  case model.search_term {
    option.None -> element.text("start typing to search")
    option.Some(search_term) -> {
      case find_posts(model, search_term) {
        [] -> no_posts_view(model)
        posts -> posts_view(model, posts)
      }
    }
  }
}

fn find_posts(model, search_term) -> List(Post(Nil)) {
  []
}

fn no_posts_view(model: Model) -> Element(Msg) {
  html.div([
  ], [
    html.p([], [element.text("No posts found.")])
  ])
}

fn posts_view(model: Model, posts: List(Post(Nil))) -> Element(Msg) {
  let posts =
    list.map(posts, fn(p: Post(Nil)) {
      html.div([attribute.class("post-preview")], [
        html.a([attribute.href("/" <> p.slug)], [
          html.div(
            [
              attribute.class("post-preview-title"),
            ],
            [
              element.text(p.title),
            ],
          ),
          html.p(
            [
              attribute.class("post-preview-description"),
            ],
            [
              element.text(p.description),
            ],
          ),
        ]),
      ])
    })
  html.div([
  ], [
    html.div([
      attribute.class("tag-browser-list-container"),
    ], [
      html.div([
        attribute.class("post-list"),
      ], posts
    )])
  ])
}

