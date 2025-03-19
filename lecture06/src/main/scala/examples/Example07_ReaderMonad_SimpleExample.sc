import examples.data.Reader
import examples.typeclasses.MonadSyntax._

case class Config(url: String)

def httpRequest(url: String): Reader[Config, String] = {
  println(s"Http request $url")
  "Bob".pure[Reader[Config, *]]
}

def getUserName(id: Int): Reader[Config, String] =
  for {
    url  <- Reader.ask[Config, String](_.url)
    name <- httpRequest(s"$url/$id")
  } yield name

getUserName(42).run(Config("http://service.ru/users"))

