import blogatto/config
import blogatto/config/feed/atom
import blogatto/config/feed/rss
import blogatto/config/post
import blogatto/config/robots
import blogatto/config/sitemap
import blogatto/post.{type Post} as _
import gleam/list
import gleam/option
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
    |> post.path("./content/posts")
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
    |> config.output_dir("./dist")
    |> config.static_dir("./public")
    |> config.post(post_config)
    |> config.route("/", home_view)
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

fn home_view(posts: List(Post(Nil))) -> Element(Nil) {
  let sorted =
    list.sort(posts, fn(a: Post(Nil), b: Post(Nil)) { timestamp.compare(b.date, a.date) })

  html.html([], [
    html.head([], [html.title([], "My Blog")]),
    html.body([], [
      html.h1([], [element.text("My Blog")]),
      html.ul(
        [],
        list.map(sorted, fn(p: Post(Nil)) {
          html.li([], [
            html.a([attribute.href(p.slug)], [
              element.text(p.title),
            ]),
          ])
        }),
      ),
    ]),
  ])
}


fn blog_post_template(
  p: Post(Nil),
  _all_posts: List(Post(Nil)),
) -> Element(Nil) {
  let lang = option.unwrap(p.language, "en")
  let formatted_date = p.date
    |> datetime.from_timestamp()
    |> datetime.format(tempo.Custom(format: "dddd, MMMM d, YYYY"))

  html.html([attribute.lang(lang)], [
    html.head([], [
      html.meta([attribute.charset("UTF-8")]),
      html.meta([
        attribute.name("viewport"),
        attribute.content("width=device-width, initial-scale=1"),
      ]),
      html.title([], p.title),
      html.meta([
        attribute.name("description"),
        attribute.content(p.description),
      ]),
    ]),
    html.body([], [
      html.header([], [
        html.nav([], [
          html.a([attribute.href("/")], [element.text("← Home")]),
        ]),
      ]),
      html.main([], [
        html.article([], [
          html.h1([], [element.text(p.title)]),
          html.p([], [html.em([], [element.text(formatted_date)])]),
          html.div([], p.contents),
        ]),
      ]),
      html.footer([], [
        html.p([], [
          element.text("Built with "),
          html.a([attribute.href("https://github.com/veeso/blogatto")], [
            element.text("Blogatto"),
          ]),
        ]),
      ]),
    ]),
  ]) 
}
