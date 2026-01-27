package zio.schema.codec

import zio.Scope
import zio.schema._
import zio.test._
import zio.test.Assertion._

object XmlCodecSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  final case class User(name: String, active: Boolean, roles: List[String])

  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]
  }

  final case class Container(value: Option[Int])

  object Container {
    implicit val schema: Schema[Container] = DeriveSchema.gen[Container]
  }

  sealed trait Status
  case object Active                   extends Status
  case object Inactive                 extends Status
  case class Suspended(reason: String) extends Status

  object Status {
    implicit val schema: Schema[Status] = DeriveSchema.gen[Status]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("XML Codec Spec")(
    suite("Primitives")(
      test("round-trip string") {
        check(Gen.string) { str =>
          val codec   = XmlCodec.schemaBasedBinaryCodec[String]
          val encoded = codec.encode(str)
          val decoded = codec.decode(encoded).left.map(_.message)
          assert(decoded)(isRight(equalTo(str)))
        }
      },
      test("round-trip int") {
        check(Gen.int) { i =>
          val codec   = XmlCodec.schemaBasedBinaryCodec[Int]
          val encoded = codec.encode(i)
          val decoded = codec.decode(encoded).left.map(_.message)
          assert(decoded)(isRight(equalTo(i)))
        }
      },
      test("round-trip boolean") {
        check(Gen.boolean) { b =>
          val codec   = XmlCodec.schemaBasedBinaryCodec[Boolean]
          val encoded = codec.encode(b)
          val decoded = codec.decode(encoded).left.map(_.message)
          assert(decoded)(isRight(equalTo(b)))
        }
      }
    ),
    suite("Case Classes")(
      test("round-trip Person") {
        val person  = Person("Alice", 30)
        val codec   = XmlCodec.schemaBasedBinaryCodec[Person]
        val encoded = codec.encode(person)
        val decoded = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(person)))
      },
      test("encodes Person correctly") {
        val person    = Person("Alice", 30)
        val codec     = XmlCodec.schemaBasedBinaryCodec[Person]
        val encoded   = codec.encode(person)
        val xmlString = new String(encoded.toArray)
        // check structure
        assert(xmlString)(
          containsString("<Person>") && containsString("<name>Alice</name>") && containsString("<age>30</age>") && containsString(
            "</Person>"
          )
        )
      }
    ),
    suite("Collections")(
      test("round-trip User with list") {
        val user    = User("Bob", true, List("admin", "editor"))
        val codec   = XmlCodec.schemaBasedBinaryCodec[User]
        val encoded = codec.encode(user)
        val decoded = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(user)))
      },
      test("decodes empty list") {
        val user    = User("Bob", true, List.empty)
        val codec   = XmlCodec.schemaBasedBinaryCodec[User]
        val encoded = codec.encode(user)
        val decoded = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(user)))
      }
    ),
    suite("Optionals")(
      test("round-trip Some") {
        val container = Container(Some(42))
        val codec     = XmlCodec.schemaBasedBinaryCodec[Container]
        val encoded   = codec.encode(container)
        val decoded   = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(container)))
      },
      test("round-trip None") {
        val container = Container(None)
        val codec     = XmlCodec.schemaBasedBinaryCodec[Container]
        val encoded   = codec.encode(container)
        val decoded   = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(container)))
      },
      test("encodes None as empty/missing element") {
        val container = Container(None)
        val codec     = XmlCodec.schemaBasedBinaryCodec[Container]
        val encoded   = codec.encode(container)
        val xmlString = new String(encoded.toArray)
        // verify empty element output
        assert(xmlString)(containsString("<Container></Container>") || containsString("<Container/>"))
      }
    ),
    suite("Enums/Sealed Traits")(
      test("round-trip case object") {
        val status: Status = Active
        val codec          = XmlCodec.schemaBasedBinaryCodec[Status]
        val encoded        = codec.encode(status)
        val decoded        = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(status)))
      },
      test("round-trip case class variant") {
        val status: Status = Suspended("policy violation")
        val codec          = XmlCodec.schemaBasedBinaryCodec[Status]
        val encoded        = codec.encode(status)
        val decoded        = codec.decode(encoded).left.map(_.message)
        assert(decoded)(isRight(equalTo(status)))
      }
    )
  )
}
