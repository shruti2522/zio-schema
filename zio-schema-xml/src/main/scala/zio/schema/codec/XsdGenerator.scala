package zio.schema.codec

import zio.schema._

// generates ScalaXB-compatible XSD
object XsdGenerator {

  // generate XSD string from Schema
  def fromSchema[A](schema: Schema[A]): Either[String, String] = {
    val definitions = scala.collection.mutable.Set.empty[String]

    def visit(s: Schema[_], name: String): String = {
      val typeName = getTypeName(s, name)
      if (!definitions.contains(typeName)) {
        // prevent recursion (simplistic)

        val definition = generateDefinition(s, typeName, visit)
        definition.foreach(d => definitions.add(d))
      }
      typeName
    }

    val rootName = getTypeName(schema, "Root")
    // start visiting
    visit(schema, rootName)

    val allDefinitions = definitions.mkString("\n")

    Right(s"""<?xml version="1.0" encoding="UTF-8"?>
             |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
             |           elementFormDefault="qualified">
             |  <xs:element name="$rootName" type="$rootName"/>
             |$allDefinitions
             |</xs:schema>""".stripMargin)
  }

  private def getTypeName(schema: Schema[_], default: String): String = schema match {
    case record: Schema.Record[_] => record.id.name
    case enum: Schema.Enum[_]     => enum.id.name
    case _                        => default
  }

  private def generateDefinition(
    schema: Schema[_],
    typeName: String,
    visitor: (Schema[_], String) => String
  ): Option[String] =
    schema match {
      case record: Schema.Record[_] =>
        val fields = record.fields.map { field =>
          val fieldTypeName = field.schema match {
            case Schema.Primitive(st, _) => xsdPrimitive(st)
            case Schema.Optional(s, _) =>
              s match {
                case Schema.Primitive(st, _) => xsdPrimitive(st)
                case _                       => visitor(s, field.name + "Type")
              }
            case s => visitor(s, field.name + "Type")
          }

          val minOccurs = field.schema match {
            case Schema.Optional(_, _) => "0"
            case _                     => "1"
          }
          s"""      <xs:element name="${field.name}" type="$fieldTypeName" minOccurs="$minOccurs"/>"""
        }.mkString("\n")

        Some(s"""  <xs:complexType name="$typeName">
                |    <xs:sequence>
                |$fields
                |    </xs:sequence>
                |  </xs:complexType>""".stripMargin)

      case enum: Schema.Enum[_] =>
        val cases = enum.cases.map { c =>
          val caseTypeName = c.schema match {
            case Schema.Primitive(st, _) => xsdPrimitive(st)
            case s                       => visitor(s, c.id + "Type")
          }
          s"""      <xs:element name="${c.id}" type="$caseTypeName"/>"""
        }.mkString("\n")

        Some(s"""  <xs:complexType name="$typeName">
                |    <xs:choice>
                |$cases
                |    </xs:choice>
                |  </xs:complexType>""".stripMargin)

      case Schema.Sequence(elemSchema, _, _, _, _) =>
        val itemType = elemSchema match {
          case Schema.Primitive(st, _) => xsdPrimitive(st)
          case s                       => visitor(s, typeName + "Item")
        }
        Some(s"""  <xs:complexType name="$typeName">
                |    <xs:sequence>
                |      <xs:element name="item" type="$itemType" minOccurs="0" maxOccurs="unbounded"/>
                |    </xs:sequence>
                |  </xs:complexType>""".stripMargin)

      case _ => None // primitives handled inline
    }

  private def xsdPrimitive(standardType: StandardType[_]): String = standardType match {
    case StandardType.StringType     => "xs:string"
    case StandardType.BoolType       => "xs:boolean"
    case StandardType.ShortType      => "xs:short"
    case StandardType.IntType        => "xs:int"
    case StandardType.LongType       => "xs:long"
    case StandardType.FloatType      => "xs:float"
    case StandardType.DoubleType     => "xs:double"
    case StandardType.BinaryType     => "xs:base64Binary"
    case StandardType.CharType       => "xs:string"
    case StandardType.UUIDType       => "xs:string"
    case StandardType.BigDecimalType => "xs:decimal"
    case StandardType.BigIntegerType => "xs:integer"
    case StandardType.UnitType       => "xs:string"
    // date/time mapping
    case StandardType.InstantType       => "xs:dateTime"
    case StandardType.LocalDateType     => "xs:date"
    case StandardType.LocalTimeType     => "xs:time"
    case StandardType.LocalDateTimeType => "xs:dateTime"
    case _                              => "xs:string"
  }
}
