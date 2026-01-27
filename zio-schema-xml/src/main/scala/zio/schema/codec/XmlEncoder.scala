package zio.schema.codec

import scala.xml._
import zio.schema._
import zio.Chunk
import scala.collection.immutable.ListMap

/**
 * XML encoder that converts Scala values to XML elements based on their ZIO Schema.
 * Designed to produce ScalaXB-compatible XML output.
 */
private[codec] object XmlEncoder {

  def encode[A](value: A, schema: Schema[A], config: XmlCodec.Configuration): Elem = {
    val node = encodeValue(value, schema, config, None)
    node match {
      case e: Elem => e
      case other   => Elem(null, "Value", Null, TopScope, false, other)
    }
  }

  private def encodeValue[A](
    value: A,
    schema: Schema[A],
    config: XmlCodec.Configuration,
    elementName: Option[String]
  ): Node = {
    schema match {
      // Primitive types
      case Schema.Primitive(standardType, _) =>
        encodePrimitive(value, standardType, config, elementName)

      // Case classes (Records)
      case record: Schema.Record[A] =>
        encodeRecord(value, record, config, elementName)

      // Enumerations
      case enum: Schema.Enum[A] =>
        encodeEnum(value, enum, config, elementName)

      // Sequences
      case Schema.Sequence(elementSchema, _, toChunk, _, _) =>
        val chunk = toChunk(value)
        encodeSequence(chunk, elementSchema, config, elementName)

      // Sets
      case Schema.Set(elementSchema, _) =>
        val set = value.asInstanceOf[Set[Any]]
        encodeSequence(Chunk.fromIterable(set), elementSchema, config, elementName)

      // Optional values
      case Schema.Optional(codec, _) =>
        value.asInstanceOf[Option[Any]] match {
          case Some(v) => encodeValue(v, codec.asInstanceOf[Schema[Any]], config, elementName)
          case None    => Text("") // Empty element for None
        }

      // Either
      case Schema.Either(left, right, _) =>
        value.asInstanceOf[Either[Any, Any]] match {
          case Left(l) =>
            val leftElem = encodeValue(l, left.asInstanceOf[Schema[Any]], config, Some("Left"))
            wrapInElement(leftElem, elementName.getOrElse("Either"))
          case Right(r) =>
            val rightElem = encodeValue(r, right.asInstanceOf[Schema[Any]], config, Some("Right"))
            wrapInElement(rightElem, elementName.getOrElse("Either"))
        }

      // Tuples
      case Schema.Tuple2(left, right, _) =>
        val (l, r) = value.asInstanceOf[(Any, Any)]
        val leftNode = encodeValue(l, left.asInstanceOf[Schema[Any]], config, Some("_1"))
        val rightNode = encodeValue(r, right.asInstanceOf[Schema[Any]], config, Some("_2"))
        Elem(null, elementName.getOrElse("Tuple2"), Null, TopScope, false, leftNode, rightNode)

      // Lazy schemas
      case Schema.Lazy(schema0) =>
        encodeValue(value, schema0(), config, elementName)

      // Transform
      case Schema.Transform(codec, _, g, _, _) =>
        g(value) match {
          case Left(err) => throw new RuntimeException(s"Transform failed: $err")
          case Right(v)  => encodeValue(v, codec.asInstanceOf[Schema[Any]], config, elementName)
        }

      // Fallback - not commonly used, encode as tuple
      case Schema.Fallback(left, right, _, _) =>
        value match {
          case Fallback.Left(l) =>
            encodeValue(l, left.asInstanceOf[Schema[Any]], config, elementName)
          case Fallback.Right(r) =>
            encodeValue(r, right.asInstanceOf[Schema[Any]], config, elementName)
          case Fallback.Both(l, r) =>
            val leftNode = encodeValue(l, left.asInstanceOf[Schema[Any]], config, Some("left"))
            val rightNode = encodeValue(r, right.asInstanceOf[Schema[Any]], config, Some("right"))
            Elem(null, elementName.getOrElse("Fallback"), Null, TopScope, false, leftNode, rightNode)
        }

      case _ =>
        throw new RuntimeException(s"Unsupported schema type: ${schema.getClass.getSimpleName}")
    }
  }

  private def encodePrimitive[A](
    value: A,
    standardType: StandardType[A],
    config: XmlCodec.Configuration,
    elementName: Option[String]
  ): Node = {
    val textValue = standardType match {
      case StandardType.StringType        => value.asInstanceOf[String]
      case StandardType.BoolType          => value.toString
      case StandardType.ShortType         => value.toString
      case StandardType.IntType           => value.toString
      case StandardType.LongType          => value.toString
      case StandardType.FloatType         => value.toString
      case StandardType.DoubleType        => value.toString
      case StandardType.BinaryType        => java.util.Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray)
      case StandardType.CharType          => value.toString
      case StandardType.UUIDType          => value.toString
      case StandardType.BigDecimalType    => value.toString
      case StandardType.BigIntegerType    => value.toString
      case StandardType.DayOfWeekType     => value.toString
      case StandardType.MonthType         => value.toString
      case StandardType.MonthDayType      => value.toString
      case StandardType.PeriodType        => value.toString
      case StandardType.YearType          => value.toString
      case StandardType.YearMonthType     => value.toString
      case StandardType.ZoneIdType        => value.toString
      case StandardType.ZoneOffsetType    => value.toString
      case StandardType.DurationType      => value.toString
      case StandardType.InstantType       => value.toString
      case StandardType.LocalDateType     => value.toString
      case StandardType.LocalTimeType     => value.toString
      case StandardType.LocalDateTimeType => value.toString
      case StandardType.OffsetTimeType    => value.toString
      case StandardType.OffsetDateTimeType => value.toString
      case StandardType.ZonedDateTimeType => value.toString
      case StandardType.UnitType          => ""
      case _                              => value.toString
    }

    elementName match {
      case Some(name) => Elem(null, name, Null, TopScope, false, Text(textValue))
      case None       => Text(textValue)
    }
  }

  private def encodeRecord[A](
    value: A,
    record: Schema.Record[A],
    config: XmlCodec.Configuration,
    elementName: Option[String]
  ): Elem = {
    val recordName = elementName.getOrElse(record.id.name)
    val fields = record.fields.map { field =>
      val fieldValue = field.get(value)
      encodeValue(fieldValue, field.schema.asInstanceOf[Schema[Any]], config, Some(field.name))
    }
    Elem(null, recordName, Null, TopScope, false, fields.toSeq: _*)
  }

  private def encodeEnum[A](
    value: A,
    enumN: Schema.Enum[A],
    config: XmlCodec.Configuration,
    elementName: Option[String]
  ): Elem = {
    val enumName = elementName.getOrElse(enumN.id.name)
    
    // Find the matching case
    val caseValue = enumN.cases.find(c => c.deconstructOption(value).isDefined)
      .getOrElse(throw new RuntimeException(s"No matching case found for enum value: $value"))

    caseValue.deconstructOption(value) match {
      case Some(caseVal) =>
        val caseNode = encodeValue(caseVal, caseValue.schema.asInstanceOf[Schema[Any]], config, Some(caseValue.id))
        Elem(null, enumName, Null, TopScope, false, caseNode)
      case None =>
        throw new RuntimeException(s"Failed to deconstruct enum value: $value")
    }
  }

  private def encodeSequence[A](
    chunk: Chunk[A],
    elementSchema: Schema[A],
    config: XmlCodec.Configuration,
    elementName: Option[String]
  ): Elem = {
    val containerName = elementName.getOrElse("Sequence")
    val items = chunk.map { item =>
      encodeValue(item, elementSchema, config, Some(config.collectionItemName))
    }
    Elem(null, containerName, Null, TopScope, false, items.toSeq: _*)
  }

  private def wrapInElement(node: Node, name: String): Elem = {
    Elem(null, name, Null, TopScope, false, node)
  }
}
