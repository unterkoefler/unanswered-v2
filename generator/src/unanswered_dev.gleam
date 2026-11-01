import blog
import blogatto/dev
import blogatto/error
import gleam/io

pub fn main() {
  case
    blog.config()
    |> dev.new()
    |> dev.build_command("gleam run")
    |> dev.port(4000)
    |> dev.start()
  {
    Ok(Nil) -> io.println("Dev server stopped.")
    Error(err) -> io.println("Dev server error: " <> error.describe_error(err))
  }
}
