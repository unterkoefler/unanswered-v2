import gleam/int
import gleam/dict.{type Dict}
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn main() -> Nil {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

type Model {
  Model(
    posts_by_tag: Dict(String, String),
  )
}

fn init(_) -> Model {
  Model(posts_by_tag: dict.new())
}

type Msg {
  NoOp
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    NoOp -> model
  }
}

fn view(model) -> Element(Msg) {
  element.text("its alive!!!")
}
