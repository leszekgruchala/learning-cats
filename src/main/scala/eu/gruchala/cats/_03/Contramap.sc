import scala.util.Try

trait Printable[A] {
  def format(value: A): String

  //Prepended functor
  def contramap[B](func: B => A): Printable[B] = {
    val self = this
    new Printable[B] {
      override def format(value: B): String = self.format(func(value))
    }
  }
}

def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

implicit val stringPrintable =
  new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

implicit val booleanPrintable =
  new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

format("hello")

format(true)

final case class Box[A](value: A)

implicit def boxPrintable[A: Printable]: Printable[Box[A]] = {
  val p = implicitly[Printable[A]]
  p.contramap(_.value)//contramap - prepend functor
  //to any available Printable[A] prepend mapping from Box to A
}

format(Box(true))

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): Option[A]

  //Combination of map and contramap
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      override def encode(value: B): String =
        self.encode(enc(value))

      override def decode(value: String): Option[B] =
        self.decode(value).map(dec)
    }
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
  c.decode(value)

implicit val intCoded = new Codec[Int] {
  override def encode(value: Int): String = value.toString

  override def decode(value: String): Option[Int] =
    Try(value.toInt).toOption
}

implicit def boxCodec[A: Codec]: Codec[Box[A]] = {
  val c = implicitly[Codec[A]]
  c.imap[Box[A]](Box(_), _.value)
}

encode(Box(123))

decode[Box[Int]]("123")
