import blogatto/post.{type Post, Post}
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/time/timestamp
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import plinth/browser/document
import plinth/browser/element as plinth_element

pub fn main() -> Nil {
  let assert Ok(el) = document.query_selector("#lustre-data")
  let txt = plinth_element.inner_text(el)
  let assert Ok(posts_by_tag) = json.parse(txt, posts_by_tag_decoder())

  let app = lustre.application(init, update, view)
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
    posts_by_tag: Dict(String, List(Post(Nil))),
    selected_tag: option.Option(String),
  )
}

fn init(
  posts_by_tag: dict.Dict(String, List(Post(Nil))),
) -> #(Model, Effect(Msg)) {
  #(Model(posts_by_tag:, selected_tag: option.None), effect.none())
}

type Msg {
  ToggleTag(tag: String)
  ScrollToView
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ToggleTag(tag) -> {
      case model.selected_tag {
        option.Some(curr_tag) if curr_tag == tag -> #(
          Model(..model, selected_tag: option.None),
          effect.none(),
        )
        option.Some(other_tag) -> #(
          Model(..model, selected_tag: option.Some(tag)),
          effect.after_paint(fn(dispatch, _) { dispatch(ScrollToView) }),
        )
        option.None -> #(
          Model(..model, selected_tag: option.Some(tag)),
          effect.after_paint(fn(dispatch, _) { dispatch(ScrollToView) }),
        )
      }
    }
    ScrollToView -> {
      case document.query_selector("#scroll-target") {
        Ok(el) -> {
          plinth_element.scroll_into_view(el)
        }
        Error(_) -> {
          Nil
        }
      }
      #(model, effect.none())
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  case model.selected_tag {
    option.None -> tag_grid_view(model)
    option.Some(tag) -> {
      case dict.get(model.posts_by_tag, tag) {
        Error(Nil) -> no_posts_view(model, tag)
        Ok([]) -> no_posts_view(model, tag)
        Ok(posts) -> posts_for_tag_view(model, tag, posts)
      }
    }
  }
}

fn tag_grid_view(model: Model) -> Element(Msg) {
  html.div(
    [
      attribute.class("tag-grid"),
    ],
    dict.fold(
      model.posts_by_tag,
      [],
      fn(acc: List(Element(Msg)), k: String, v: List(Post(Nil))) {
        [view_tag(k, v, option.Some(k) == model.selected_tag), ..acc]
      },
    ),
  )
}

fn view_tag(
  tag: String,
  posts: List(Post(Nil)),
  selected: Bool,
) -> Element(Msg) {
  let classes = case selected {
    True -> [attribute.class("selected-tag-name")]
    False -> []
  }
  html.div([attribute.class("tag-name"), ..classes], [
    html.button([event.on_click(ToggleTag(tag))], [
      element.text("#" <> tag),
      element.text(" (" <> int.to_string(list.length(posts)) <> ")"),
    ]),
  ])
}

fn no_posts_view(model: Model, tag: String) -> Element(Msg) {
  html.div([], [
    tag_grid_view(model),
    html.p([], [element.text("No posts found for tag #" <> tag)]),
  ])
}

fn posts_for_tag_view(
  model: Model,
  tag: String,
  posts: List(Post(Nil)),
) -> Element(Msg) {
  let posts =
    posts
    |> list.reverse()
    |> list.index_map(fn(p: Post(Nil), i: Int) {
      let attrs = case i {
        0 -> [attribute.id("scroll-target")]
        _ -> []
      }
      html.div([attribute.class("post-preview"), ..attrs], [
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
  html.div([], [
    tag_grid_view(model),
    html.h2([], [
      element.text(tag),
      element.text(" (" <> int.to_string(list.length(posts)) <> ")"),
    ]),
    html.div(
      [
        attribute.class("tag-browser-list-container"),
      ],
      [
        html.div(
          [
            attribute.class("post-list"),
          ],
          posts,
        ),
      ],
    ),
  ])
}
