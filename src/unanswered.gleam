import blogatto
import blogatto/error
import blog
import gleam/io

pub fn main() {
  case blogatto.build(blog.config()) {
    Ok(Nil) -> io.println("Site built successfully!")
    Error(err) -> io.println("Build failed: " <> error.describe_error(err))
  }
}
