import blogatto/post.{type Post, Post}
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import gleam/time/timestamp
import gleam/uri
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import modem
import plinth/browser/document
import plinth/browser/element as plinth_element
import prng/random

pub fn main() -> Nil {
  let assert Ok(el) = document.query_selector("#lustre-data")
  let txt = plinth_element.inner_text(el)
  let assert Ok(slugs) = json.parse(txt, decode.list(decode.string))

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", slugs)

  Nil
}

type Model {
  Model(slugs: List(String), state: State)
}

type State {
  Choosing
  Redirecting(slug: String)
  Failed(message: String, code: Int)
}

fn init(slugs: List(String)) -> #(Model, Effect(Msg)) {
  #(
    Model(slugs:, state: Choosing),
    effect.batch([
      effect.from(fn(dispatch) { dispatch(Redirect) }),
      modem.init(fn(_) { Go }),
    ]),
  )
}

type Msg {
  Redirect
  Go
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Redirect -> {
      case model.slugs {
        [] -> #(
          Model(..model, state: Failed(message: "No posts found", code: 53)),
          effect.none(),
        )
        [first, ..rest] -> {
          let seed =
            float.random() *. 100_000.0 |> float.round() |> random.new_seed()
          let gen = random.uniform(first, rest)
          let #(next, _) = random.step(gen, seed)
          #(
            Model(..model, state: Redirecting(next)),
            modem.replace(next, option.None, option.None),
          )
        }
      }
    }
    Go -> {
      case model.state {
        Redirecting(slug) ->
          case uri.parse("/" <> slug) {
            Ok(next) -> {
              #(model, modem.load(next))
            }
            _ -> {
              #(
                Model(..model, state: Failed("could not parse url", 85)),
                effect.none(),
              )
            }
          }
        _ -> {
          #(Model(..model, state: Failed("bad state", 89)), effect.none())
        }
      }
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  case model.state {
    Choosing | Redirecting(_) ->
      element.text(
        "Choosing a random post. You should be redirected shortly...",
      )
    Failed(message, code) ->
      element.text(
        "Failed: " <> message <> ". Error code: " <> int.to_string(code) <> ".",
      )
  }
}
