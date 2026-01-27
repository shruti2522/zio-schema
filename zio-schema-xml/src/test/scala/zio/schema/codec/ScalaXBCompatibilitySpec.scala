package zio.schema.codec

import zio._
import zio.schema._
import zio.test._
import zio.test.Assertion._
import scala.xml.{ Utility, XML }

// validate ZIO Schema XML Codec compatibility with ScalaXB using #$%%"Golden Master" patterns
object ScalaXBCompatibilitySpec extends ZIOSpecDefault {

  // scenarios representing ScalaXB-generated code patterns

  // scenario 1: simple case class
  // XSD:
  // <xs:complexType name="Person">
  //   <xs:sequence>
  //     <xs:element name="name" type="xs:string"/>
  //     <xs:element name="age" type="xs:int"/>
  //   </xs:sequence>
  // </xs:complexType>	

  final case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  // scenario 2: collections using "unbounded" maxOccurs
  // XSD:
  // <xs:complexType name="Inventory">
  //   <xs:sequence>
  //     <xs:element name="item" type="xs:string" maxOccurs="unbounded"/>
  //   </xs:sequence>
  // </xs:complexType>

  final case class Inventory(item: List[String])

  object Inventory {
    implicit val schema: Schema[Inventory] = DeriveSchema.gen[Inventory]
  }

  // scenario 3: optional fields
  // XSD:
  // <xs:complexType name="User">
  //   <xs:sequence>
  //     <xs:element name="email" type="xs:string" minOccurs="0"/>
  //   </xs:sequence>
  // </xs:complexType>

  final case class User(email: Option[String])

  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]
  }

  // scenario 4: nested record
  final case class Address(street: String, city: String)

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }
  final case class Contact(name: String, address: Address)

  object Contact {
    implicit val schema: Schema[Contact] = DeriveSchema.gen[Contact]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ScalaXB Compatibility Spec")(
    test("Scenario 1: Simple Record matches expected XML") {
      val data        = Person("Alice", 30)
      val expectedXml = <Person><name>Alice</name><age>30</age></Person>

      for {
        encoded <- ZIO.succeed(XmlCodec.schemaBasedBinaryCodec[Person].encode(data))
        decoded <- ZIO.fromEither(XmlCodec.schemaBasedBinaryCodec[Person].decode(encoded).left.map(_.message))
      } yield {
        val xmlString = new String(encoded.toArray)
        val loadedXml = XML.loadString(xmlString)

        // Utility.trim removes whitespace for comparison
        assertTrue(Utility.trim(loadedXml).toString == Utility.trim(expectedXml).toString) &&
        assert(decoded)(equalTo(data))
      }
    },
    test("Scenario 2: Collections encoded as repeated elements") {
      val data = Inventory(List("Apple", "Banana"))
      val expectedXml = <Inventory><item>Apple</item><item>Banana</item></Inventory>

      for {
        encoded <- ZIO.succeed(XmlCodec.schemaBasedBinaryCodec[Inventory].encode(data))
        decoded <- ZIO.fromEither(XmlCodec.schemaBasedBinaryCodec[Inventory].decode(encoded).left.map(_.message))
      } yield {
        val xmlString = new String(encoded.toArray)
        val loadedXml = XML.loadString(xmlString)
        assertTrue(Utility.trim(loadedXml).toString == Utility.trim(expectedXml).toString) &&
        assert(decoded)(equalTo(data))
      }
    },
    test("Scenario 3: Optionals omitted when None") {
      val dataFull  = User(Some("alice@example.com"))
      val dataEmpty = User(None)

      val expectedFull  = <User><email>alice@example.com</email></User>
      val expectedEmpty = <User></User>

      for {
        encodedFull  <- ZIO.succeed(XmlCodec.schemaBasedBinaryCodec[User].encode(dataFull))
        decodedFull  <- ZIO.fromEither(XmlCodec.schemaBasedBinaryCodec[User].decode(encodedFull).left.map(_.message))
        encodedEmpty <- ZIO.succeed(XmlCodec.schemaBasedBinaryCodec[User].encode(dataEmpty))
        decodedEmpty <- ZIO.fromEither(XmlCodec.schemaBasedBinaryCodec[User].decode(encodedEmpty).left.map(_.message))
      } yield {
        val xmlFull  = XML.loadString(new String(encodedFull.toArray))
        val xmlEmpty = XML.loadString(new String(encodedEmpty.toArray))

        assertTrue(Utility.trim(xmlFull).toString == Utility.trim(expectedFull).toString) &&
        assertTrue(Utility.trim(xmlEmpty).toString == Utility.trim(expectedEmpty).toString) &&
        assert(decodedFull)(equalTo(dataFull)) &&
        assert(decodedEmpty)(equalTo(dataEmpty))
      }
    },
    test("Scenario 4: Nested Records") {
      val data = Contact("Bob", Address("123 Main St", "Metropolis"))

      val expectedXml =
        <Contact>
          <name>Bob</name>
          <address>
            <street>123 Main St</street>
            <city>Metropolis</city>
          </address>
        </Contact>

      for {
        encoded <- ZIO.succeed(XmlCodec.schemaBasedBinaryCodec[Contact].encode(data))
        decoded <- ZIO.fromEither(XmlCodec.schemaBasedBinaryCodec[Contact].decode(encoded).left.map(_.message))
      } yield {
        val xmlString = new String(encoded.toArray)
        val loadedXml = XML.loadString(xmlString)
        assertTrue(Utility.trim(loadedXml).toString == Utility.trim(expectedXml).toString) &&
        assert(decoded)(equalTo(data))
      }
    }
  )
}
