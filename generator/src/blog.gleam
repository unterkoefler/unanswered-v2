import blogatto/config
import blogatto/config/feed/atom
import blogatto/config/feed/rss
import blogatto/config/post
import blogatto/config/robots
import blogatto/config/sitemap
import blogatto/post.{type Post} as _
import gleam/dict
import gleam/float
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleam/time/timestamp
import tempo
import tempo/datetime
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

const site_url = "https://example.com"

pub fn config() -> config.Config(Nil) {
  // Post config with custom heading component
  let post_config =
    post.default()
    |> post.path("../content/posts")
    |> post.h1(fn(_attributes, id, children) {
      html.h1([attribute.id(id), attribute.class("post-title")], children)
    })
    |> post.img(fn(_attributes, uri, alt, maybe_title) {
      html.img([
        attribute.src("/" <> uri),
        attribute.alt(alt),
      ])
    })
    |> post.template(blog_post_template)

  // RSS feed
  let rss_feed =
    rss.new("My Blog", site_url, "My personal blog")
    |> rss.language("en-us")
    |> rss.generator("Blogatto")

  // Atom feed
  let atom_feed =
    atom.new(
      id: site_url <> "/",
      title: atom.PlainText("My Blog"),
      updated: timestamp.system_time(),
    )
    |> atom.subtitle("My personal blog")

  config.new(site_url)
    |> config.output_dir("../dist")
    |> config.static_dir("../public")
    |> config.post(post_config)
    |> config.route("/", home_view)
    |> config.route("/what-is-this", what_is_this_view)
    |> config.route("/who-am-i", who_am_i_view)
    |> config.route("/contact-me", contact_me_view)
    |> config.route("/subscribe", subscribe_view)
    |> config.route("/tags", tags_view)
    |> config.rss_feed(rss_feed)
    |> config.atom_feed(atom_feed)
    |> config.sitemap(sitemap.new("/sitemap.xml"))
    |> config.robots(robots.RobotsConfig(
      sitemap_url: site_url <> "/sitemap.xml",
      robots: [
        robots.Robot(
          user_agent: "*",
          allowed_routes: ["/"],
          disallowed_routes: [],
        ),
      ],
    ))
}

fn post_to_json(post: Post(Nil)) -> json.Json {
  json.object([
    #("title", json.string(post.title)),
    #("slug", json.string(post.slug)),
    #("url", json.string(post.url)),
    #("date", post.date |> timestamp.to_unix_seconds |> float.round |> json.int),
    #("description", post.description |> json.string),
    #("excerpt", post.excerpt |> json.string),
    #("language", json.nullable(post.language, json.string)),
    #("featured_image", json.nullable(post.featured_image, json.string)),
  ])
}


fn home_view(posts: List(Post(Nil))) -> Element(Nil) {
  let recent =
    posts
    |> list.take(15)

  static_page_view("Unanswered.blog", home_post_list(recent))
}

fn what_is_this_view(_posts: List(Post(Nil))) -> Element(Nil) {
  static_page_view(
    "Unanswered.blog - About",
    html.div([
    ], [
      post_content(
        "What is this?",
        option.None,
        [html.p([], [
          element.text("This is, to reiterate the subheader, where I scream into the void. I used to do this on my notes app, but then I realized that I could copy and paste the things on my notes app into a file on my computer, format the file into a webpage, and bother you, dear reader, with my content. Enjoy!")
        ])],
      ),
    ])
  )
}

fn who_am_i_view(_posts: List(Post(Nil))) -> Element(Nil) {
  static_page_view(
    "Unanswered.blog - WhoAmI",
    html.div([
    ], [
      post_content(
        "Who am I?",
        option.None,
        [html.p([], [
          element.text("I am Willy Unterkoefler. Unless otherwise noted, I write the things on here, which explains their high quality. I also designed this website, which explains its... quality. If you want to know more about me, please read everything I post here and psychoanalyze it. To help you get started, the giraffe is really a metaphor for my healthy relationship with my father and the lightning is a stand-in for my three horrible years of speech therapy in elementary school. Despite my years of speech therapy, you should know that I still pronounce water as \"woodder\". The rest of my life and personality can be derived.")
        ])],
      ),
    ])
  )
}

fn contact_me_view(_posts: List(Post(Nil))) -> Element(Nil) {
  static_page_view(
    "Unanswered.blog - Contact",
    html.div([
    ], [
      post_content(
        "Contact me",
        option.None,
        [html.p([], [
          element.text("Please don't.")
        ])],
      ),
    ])
  )
}

fn tags_view(posts: List(Post(Nil))) -> Element(Nil) {
  let data =
    collect_posts_by_tag(posts)
    |> json.dict(fn(k) { k }, fn(v) { json.array(v, post_to_json)})
  lustre_page_view(
    "Unanswered.blog - Tags",
    "tag_browser",
    html.div([
    ], [
      post_content(
        "Posts by Tag",
        option.None,
        [html.div([attribute.id("app")], [])],
      ),
    ]),
    data,
  )
}

fn collect_posts_by_tag(posts: List(Post(Nil))) -> dict.Dict(String, List(Post(Nil))) {
  posts
  |> list.fold(dict.new(), fn(acc: dict.Dict(String, List(Post(Nil))), post: Post(Nil)) {
    let assert Ok(tag_string) = post.extras |> dict.get("tags")
    let tags = tag_string |> string.split(",")
    list.fold(tags, acc, fn(acc2: dict.Dict(String, List(Post(Nil))), tag: String) {
      dict.upsert(acc2, tag |> string.trim(), fn(post_list: option.Option(List(Post(Nil)))) {
        case post_list {
          option.None -> [post]
          option.Some(l) -> [post, ..l]
        }
      })
    })
  })
}

fn subscribe_view(_posts: List(Post(Nil))) -> Element(Nil) {
  static_page_view(
    "Unanswered.blog - Subscribe",
    html.div([
    ], [
      post_content(
        "Subscribe",
        option.None,
        [
          html.p([], [element.text("Become a member of the void today! It's free!")]),
          subscribe_form(),
        ]
      ),
    ])
  )
}

fn subscribe_form() -> Element(Nil) {
  html.form([
    attribute.action("https://buttondown.com/api/emails/embed-subscribe/unterkoefler"),
    attribute.method("post"),
    attribute.class("subscribe-form"),
    attribute.target("_blank"),
  ], [
    html.label([
      attribute.for("email"),
    ], [
      element.text("Enter your email"),
    ]),
    html.input([
      attribute.type_("email"),
      attribute.name("email"),
      attribute.id("email"),
      attribute.placeholder("your@email.com"),
      attribute.autofocus(True),
      attribute.required(True),
    ]),
    html.input([
      attribute.type_("submit"),
      attribute.value("Subscribe"),
    ]),
    html.p([
    ], [
      html.a([
        attribute.href("https://buttondown.com/refer/unterkoefler"),
        attribute.target("_blank"),
      ], [
        element.text("I use and recommend Buttondown for subscriptions"),
      ])
    ]),
  ])
}



fn static_page_view(title: String, content: Element(Nil)) -> Element(Nil) {
  html.html([], [
    html.head([], [
      html.meta([attribute.name("viewport"), attribute.content("width=device-width, initial-scale=1.0")]),
      html.title([], title),
      html.link([attribute.rel("stylesheet"), attribute.href("/css/style.css")]),
    ]),
    html.body([], [
      header(),
      html.div([
        attribute.class("subheader"),
      ], [
        html.h2([], [element.text("Where I type and scream my thoughts into the void, unanswered")]),
      ]),
      html.div([
        attribute.class("content"),
      ], [
        content
      ]),
    ]),
  ])
}

fn lustre_page_view(title: String, app_name: String, content: Element(Nil), data: json.Json) -> Element(Nil) {
  html.html([], [
    html.head([], [
      html.meta([attribute.name("viewport"), attribute.content("width=device-width, initial-scale=1.0")]),
      html.title([], title),
      html.link([attribute.rel("stylesheet"), attribute.href("/css/style.css")]),
      html.script([attribute.src("/assets/" <> app_name <> ".js"), attribute.type_("module")], ""),
      html.script([attribute.type_("application/json"), attribute.id("lustre-data")],
        data |> json.to_string
      )
    ]),
    html.body([], [
      header(),
      html.div([
        attribute.class("subheader"),
      ], [
        html.h2([], [element.text("Where I type and scream my thoughts into the void, unanswered")]),
      ]),
      html.div([
        attribute.class("content"),
      ], [
        content
      ]),
    ]),
  ])
}

fn header() -> Element(Nil) {
  html.div([
    attribute.class("header"),
  ], [
    html.h1([
    ], [
      html.a([attribute.href("/")], [element.text("Unanswered")]),
    ]),
    html.button([
      attribute.class("menu-button"),
      attribute.popovertarget("menu"),
    ], [
      element.text("menu")
    ]),
    html.div([
      attribute.id("menu"),
      attribute.popover("auto"),
    ], [
      html.a([
        attribute.class("menu-item"),
        attribute.href("/what-is-this"),
      ],[
        element.text("What is this?")
      ]),
      html.a([
        attribute.class("menu-item"),
        attribute.href("/who-am-i"),
      ],[
        element.text("Who am I?")
      ]),
      html.a([
        attribute.class("menu-item"),
        attribute.href("/contact-me"),
      ],[
        element.text("Contact me")
      ]),
      html.a([
        attribute.class("menu-item"),
        attribute.href("/tags"),
      ],[
        element.text("Posts by tag")
      ]),
      html.a([
        attribute.class("menu-item"),
      ],[
        element.text("Search")
      ]),
      html.a([
        attribute.class("menu-item"),
        attribute.href("/subscribe"),
      ],[
        element.text("Subscribe")
      ]),
    ]),
  ])
}

fn home_post_list(posts: List(Post(Nil))) -> Element(Nil) {
  let posts =
    list.map(posts, fn(p: Post(Nil)) {
          html.div([attribute.class("post-preview")], [
              html.a([attribute.href(p.slug)], [
                html.div([
                  attribute.class("post-preview-title"),
                ], [
                element.text(p.title),
                ]),
                html.p([
                  attribute.class("post-preview-description"),
                ], [
                element.text(p.description),
                ]),
              ]),
          ])
        })
  html.div([
    attribute.class("post-list"),
  ], [
    html.div([], posts),
    html.p([
      attribute.class("end-of-list"),
    ], [
      element.text("Looking for more? Try "),
      html.a([
        attribute.href("/tags"),
      ], [
        element.text("Posts by Tag"),
      ]),
      element.text(" or "),
      html.a([
      ], [
        element.text("Search.")
      ]),
    ])
  ])
}

fn find_neighbors(p: Post(Nil), all_posts: List(Post(Nil))) -> #(option.Option(Post(Nil)), option.Option(Post(Nil))) {
  find_neighbors_acc(p, all_posts, option.None)
}

fn find_neighbors_acc(p: Post(Nil), posts: List(Post(Nil)), prev: option.Option(Post(Nil))) -> #(option.Option(Post(Nil)), option.Option(Post(Nil))) {
  case posts {
    [] -> #(prev, option.None)
    [head, ..rest] -> {
      case head == p {
        True -> #(prev, list.first(rest) |> option.from_result)
        False -> find_neighbors_acc(p, rest, option.Some(head))
      }
    }
  }
}

fn blog_post_template(
  p: Post(Nil),
  all_posts: List(Post(Nil)),
) -> Element(Nil) {
  let lang = option.unwrap(p.language, "en")
  let formatted_date = p.date
    |> datetime.from_timestamp()
    |> datetime.format(tempo.Custom(format: "dddd, MMMM d, YYYY"))

  let #(prev, next) = find_neighbors(p, all_posts)

  html.html([attribute.lang(lang)], [
    html.head([], [
      html.meta([attribute.charset("UTF-8")]),
      html.meta([
        attribute.name("viewport"),
        attribute.content("width=device-width, initial-scale=1"),
      ]),
      html.title([], "Unanswered.blog - " <> p.title),
      html.link([attribute.rel("stylesheet"), attribute.href("/css/style.css")]),
      html.meta([
        attribute.name("description"),
        attribute.content(p.description),
      ]),
    ]),
    html.body([], [
      header(),
      html.div([attribute.class("post-container")], [
        post_content(
          p.title,
          option.Some(formatted_date),
          p.contents
        ),
      ]),
    ]),
    footer(prev, next),
  ])
}

fn post_content(
  title: String,
  formatted_date: option.Option(String),
  contents: List(Element(Nil)),
) -> Element(Nil) {
  let date = case formatted_date {
    option.None -> element.none()
    option.Some(d) -> html.p([attribute.class("post-date")], [element.text(d)])
  }
  html.div([attribute.class("post-content")], [
    html.h3([attribute.class("post-title")], [element.text(title)]),
    date,
    html.div([], contents),
  ])
}


fn footer(prev: option.Option(Post(Nil)), next: option.Option(Post(Nil))) -> Element(Nil) {
  let previous = case prev {
    option.None -> element.text("there is no previous")
    option.Some(prev_post) -> {
      html.a([
        attribute.href("/" <> prev_post.slug),
      ], [
        element.text("previous")
      ])
    }
  }

  let next_el = case next {
    option.None -> element.text("there is no next")
    option.Some(next_post) -> {
      html.a([
        attribute.href("/" <> next_post.slug),
      ], [
        element.text("next")
      ])
    }
  }

  html.div([
    attribute.class("footer"),
  ], [
    previous,
    html.button([
    ], [ // TODO: generate random post
      element.text("Random"),
    ]),
    html.a([
      attribute.href("/subscribe"),
    ], [
      element.text("Subscribe")
    ]),
    html.a([
      attribute.href("/"),
    ], [
      element.text("Home"),
    ]),
    next_el
  ])
}


