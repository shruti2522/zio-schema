package zio.schema.codec

import zio.schema._
import zio._

object XmlExample extends ZIOAppDefault {

  // define a domain
  final case class Person(
    name: String,
    age: Int,
    active: Boolean,
    address: Option[Address],
    tags: List[String]
  )

  final case class Address(street: String, city: String)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  def run =
    for {
      _ <- Console.printLine("=== ZIO Schema XML Demo ===")

      // create an instance
      person = Person(
        name = "Alice",
        age = 30,
        active = true,
        address = Some(Address("123 Main St", "Wonderland")),
        tags = List("developer", "scala")
      )

      // generate XSD
      _   <- Console.printLine("\n[1] Generating XSD:")
      xsd <- ZIO.fromEither(XsdGenerator.fromSchema(Person.schema))
      _   <- Console.printLine(xsd)

      // encode to XML
      _   <- Console.printLine("\n[2] Encoding to XML:")
      xml = XmlCodec.schemaBasedBinaryCodec[Person].encode(person)
      // convert bytes to string for display
      xmlString = new String(xml.toArray)
      _         <- Console.printLine(xmlString)

      // decode back to Object
      _       <- Console.printLine("\n[3] Decoding back to Person:")
      decoded <- ZIO.fromEither(XmlCodec.schemaBasedBinaryCodec[Person].decode(xml))
      _       <- Console.printLine(s"Result: $decoded")

      _ <- Console.printLine("\n[4] Verification:")
      _ <- Console.printLine(s"Isomorphic? ${person == decoded}")

    } yield ()
}
