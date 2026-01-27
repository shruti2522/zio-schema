package zio.schema.codec

import scala.xml._
import zio.schema._
import zio.{Chunk, Unsafe, Cause}
import java.util.Base64

/**
 * XML decoder that converts XML elements to Scala values based on their ZIO Schema.
 * Designed to parse ScalaXB-compatible XML.
 */
private[codec] object XmlDecoder {

  def decode[A](elem: Elem, schema: Schema[A], config: XmlCodec.Configuration): Either[DecodeError, A] =
    decodeValue(elem, schema, config).asInstanceOf[Either[DecodeError, A]]

  private def decodeValue[A](
    node: Node,
    schema: Schema[A],
    config: XmlCodec.Configuration
  ): Either[DecodeError, Any] = {
    schema match {
      // Primitive types
      case Schema.Primitive(standardType, _) =>
        decodePrimitive(node, standardType)

      // Case classes (Records)
      case record: Schema.Record[A] =>
        node match {
          case elem: Elem => decodeRecord(elem, record, config)
          case _ => Left(DecodeError.MalformedField(record, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      // Enumerations
      case enumN: Schema.Enum[A] =>
        node match {
          case elem: Elem => decodeEnum(elem, enumN, config)
          case _ => Left(DecodeError.MalformedField(enumN, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      // Sequences
      case Schema.Sequence(elementSchema, fromChunk, _, _, _) =>
        node match {
          case elem: Elem => decodeSequence(elem, elementSchema, config).map(fromChunk)
          case _ => Left(DecodeError.MalformedField(elementSchema, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      // Sets
      case Schema.Set(elementSchema, _) =>
        node match {
          case elem: Elem => decodeSequence(elem, elementSchema, config).map(_.toSet)
          case _ => Left(DecodeError.MalformedField(elementSchema, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      // Optional values
      case Schema.Optional(codec, _) =>
        if (node.text.isEmpty && node.child.isEmpty) {
          Right(None)
        } else {
          decodeValue(node, codec, config).map(Some(_))
        }

      // Either
      case Schema.Either(left, right, _) =>
        node match {
          case elem: Elem =>
            elem.child.find(_.isInstanceOf[Elem]) match {
              case Some(child: Elem) if child.label == "Left" =>
                decodeValue(child, left, config).map(Left(_))
              case Some(child: Elem) if child.label == "Right" =>
                decodeValue(child, right, config).map(Right(_))
              case _ =>
                Left(DecodeError.MalformedField(schema, "Expected Left or Right element"))
            }
          case _ => Left(DecodeError.MalformedField(schema, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      // Tuples
      case Schema.Tuple2(leftSchema, rightSchema, _) =>
        node match {
          case elem: Elem =>
            val leftNode = elem.child.find {
              case e: Elem if e.label == "_1" => true
              case _ => false
            }
            val rightNode = elem.child.find {
              case e: Elem if e.label == "_2" => true
              case _ => false
            }

            (leftNode, rightNode) match {
              case (Some(l), Some(r)) =>
                for {
                  leftVal <- decodeValue(l, leftSchema, config)
                  rightVal <- decodeValue(r, rightSchema, config)
                } yield (leftVal, rightVal)
              case _ =>
                Left(DecodeError.MalformedField(schema, "Expected _1 and _2 elements"))
            }
          case _ => Left(DecodeError.MalformedField(schema, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      // Lazy schemas
      case Schema.Lazy(schema0) =>
        decodeValue(node, schema0(), config)

      // Transform
      case Schema.Transform(codec, f, _, _, _) =>
        decodeValue(node, codec, config).flatMap { v =>
          f(v) match {
            case Left(err) => Left(DecodeError.MalformedField(codec, err))
            case Right(a)  => Right(a)
          }
        }

      // Fallback
      case Schema.Fallback(left, right, _, _) =>
        node match {
          case elem: Elem =>
            val leftChild = elem.child.find {
              case e: Elem if e.label == "left" => true
              case _ => false
            }
            val rightChild = elem.child.find {
              case e: Elem if e.label == "right" => true
              case _ => false
            }

            (leftChild, rightChild) match {
              case (Some(l), Some(r)) =>
                for {
                  leftVal <- decodeValue(l, left, config)
                  rightVal <- decodeValue(r, right, config)
                } yield Fallback.Both(leftVal, rightVal)
              case (Some(l), None) =>
                decodeValue(l, left, config).map(Fallback.Left(_))
              case (None, Some(r)) =>
                decodeValue(r, right, config).map(Fallback.Right(_))
              case _ =>
                Left(DecodeError.MalformedField(schema, "Expected left or right element"))
            }
          case _ => Left(DecodeError.MalformedField(schema, s"Expected element, got: ${node.getClass.getSimpleName}"))
        }

      case _ =>
        Left(DecodeError.MalformedField(schema, s"Unsupported schema type: ${schema.getClass.getSimpleName}"))
    }
  }

  private def decodePrimitive[A](
    node: Node,
    standardType: StandardType[A]
  ): Either[DecodeError, A] = {
    val text = node.text.trim
    
    try {
      val result: Any = standardType match {
        case StandardType.StringType        => text
        case StandardType.BoolType          => text.toBoolean
        case StandardType.ShortType         => text.toShort
        case StandardType.IntType           => text.toInt
        case StandardType.LongType          => text.toLong
        case StandardType.FloatType         => text.toFloat
        case StandardType.DoubleType        => text.toDouble
        case StandardType.BinaryType        => Chunk.fromArray(Base64.getDecoder.decode(text))
        case StandardType.CharType          => if (text.length == 1) text.charAt(0) else throw new IllegalArgumentException("Expected single character")
        case StandardType.UUIDType          => java.util.UUID.fromString(text)
        case StandardType.BigDecimalType    => BigDecimal(text)
        case StandardType.BigIntegerType    => BigInt(text)
        case StandardType.DayOfWeekType     => java.time.DayOfWeek.valueOf(text)
        case StandardType.MonthType         => java.time.Month.valueOf(text)
        case StandardType.MonthDayType      => java.time.MonthDay.parse(text)
        case StandardType.PeriodType        => java.time.Period.parse(text)
        case StandardType.YearType          => java.time.Year.parse(text)
        case StandardType.YearMonthType     => java.time.YearMonth.parse(text)
        case StandardType.ZoneIdType        => java.time.ZoneId.of(text)
        case StandardType.ZoneOffsetType    => java.time.ZoneOffset.of(text)
        case StandardType.DurationType      => java.time.Duration.parse(text)
        case StandardType.InstantType       => java.time.Instant.parse(text)
        case StandardType.LocalDateType     => java.time.LocalDate.parse(text)
        case StandardType.LocalTimeType     => java.time.LocalTime.parse(text)
        case StandardType.LocalDateTimeType => java.time.LocalDateTime.parse(text)
        case StandardType.OffsetTimeType    => java.time.OffsetTime.parse(text)
        case StandardType.OffsetDateTimeType => java.time.OffsetDateTime.parse(text)
        case StandardType.ZonedDateTimeType => java.time.ZonedDateTime.parse(text)
        case StandardType.UnitType          => ()
        case _                              => text
      }
      Right(result.asInstanceOf[A])
    } catch {
      // standardType is not a Schema, so we should probable use ReadError or construct a Primitive schema if possible,
      // but ReadError is better here.
      case e: Exception =>
        Left(DecodeError.ReadError(Cause.fail(e), s"Failed to parse: ${e.getMessage}"))
    }
  }

  private def decodeRecord[A](
    elem: Elem,
    record: Schema.Record[A],
    config: XmlCodec.Configuration
  ): Either[DecodeError, A] = {
    val fieldResults = record.fields.map { field =>
      val fieldNode = elem.child.find {
        case e: Elem if e.label == field.name => true
        case _ => false
      }

      fieldNode match {
        case Some(node) =>
          decodeValue(node, field.schema, config)
        case None =>
          // Check if field is optional
          def isOptional(s: Schema[_]): Boolean = s match {
            case _: Schema.Optional[_] => true
            case Schema.Lazy(l) => isOptional(l())
            case _ => false
          }

          if (isOptional(field.schema)) {
            Right(None)
          } else {
            Left(DecodeError.MissingField(field.schema, s"Required field '${field.name}' not found"))
          }
      }
    }

    // Check for errors
    val errors = fieldResults.collect { case Left(err) => err }
    if (errors.nonEmpty) {
      Left(errors.head)
    } else {
      val values = fieldResults.collect { case Right(v) => v }
      Unsafe.unsafe { implicit unsafe =>
        record.construct(Chunk.fromIterable(values)) match {
          case Left(err) => Left(DecodeError.MalformedField(record, err))
          case Right(a)  => Right(a)
        }
      }
    }
  }

  private def decodeEnum[A](
    elem: Elem,
    enumN: Schema.Enum[A],
    config: XmlCodec.Configuration
  ): Either[DecodeError, A] = {
    // Find the first child element
    elem.child.find(_.isInstanceOf[Elem]) match {
      case Some(caseElem: Elem) =>
        // Find matching case by id
        enumN.cases.find(_.id == caseElem.label) match {
          case Some(c) =>
            val caseValue = c.asInstanceOf[Schema.Case[A, Any]]
            decodeValue(caseElem, caseValue.schema, config).map { v =>
              Unsafe.unsafe { implicit unsafe =>
                caseValue.construct(v)
              }
            }
          case None =>
            Left(DecodeError.MalformedField(enumN, s"Unknown enum case: ${caseElem.label}"))
        }
      case _ =>
        Left(DecodeError.MalformedField(enumN, "Expected child element for enum case"))
    }
  }

  private def decodeSequence[A](
    elem: Elem,
    elementSchema: Schema[A],
    config: XmlCodec.Configuration
  ): Either[DecodeError, Chunk[A]] = {
    val items = elem.child.collect {
      case e: Elem if e.label == config.collectionItemName => e
    }

    val results = items.map(item => decodeValue(item, elementSchema, config))
    val errors = results.collect { case Left(err) => err }
    
    if (errors.nonEmpty) {
      Left(errors.head)
    } else {
      val values = results.collect { case Right(v) => v.asInstanceOf[A] }
      Right(Chunk.fromIterable(values))
    }
  }
}
