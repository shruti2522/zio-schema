package zio.schema.codec

import scala.xml._
import zio.schema._
import zio.{Chunk, ZIO, Cause}
import zio.stream.ZPipeline

/**
 * XML codec for ZIO Schema.
 *
 * Provides encoding and decoding of Scala values to/from XML based on their ZIO Schema.
 * Designed to be compatible with ScalaXB-generated XML output.
 */
object XmlCodec {

  /**
   * Configuration for XML encoding and decoding.
   *
   * @param useAttributes Whether to encode simple fields as attributes instead of child elements (default: false)
   * @param includeTypeInfo Whether to include type information in the XML output (default: false)
   * @param compactOutput Whether to produce compact XML without unnecessary whitespace (default: true)
   * @param namespaceUri Optional namespace URI for the root element
   * @param collectionItemName Name to use for collection items when encoding sequences (default: "item")
   */
  final case class Configuration(
    useAttributes: Boolean = false,
    includeTypeInfo: Boolean = false,
    compactOutput: Boolean = true,
    namespaceUri: Option[String] = None,
    collectionItemName: String = "item"
  )

  object Configuration {
    val default: Configuration = Configuration()
  }

  /**
   * Creates a binary codec for type A using its implicit Schema.
   * Uses default configuration.
   */
  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    schemaBasedBinaryCodec[A](Configuration.default)

  /**
   * Creates a binary codec for type A using its implicit Schema with custom configuration.
   */
  def schemaBasedBinaryCodec[A](config: Configuration)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        val xmlString = new String(whole.toArray, "UTF-8")
        try {
          val elem = XML.loadString(xmlString)
          XmlDecoder.decode(elem, schema, config)
        } catch {
          case e: Exception =>
            Left(DecodeError.ReadError(Cause.fail(e), e.getMessage))
        }
      }

      override def encode(value: A): Chunk[Byte] = {
        val elem = XmlEncoder.encode(value, schema, config)
        val xmlString = if (config.compactOutput) {
          elem.toString()
        } else {
          new PrettyPrinter(80, 2).format(elem)
        }
        Chunk.fromArray(xmlString.getBytes("UTF-8"))
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.mapChunksEither(bytes => decode(bytes).map(Chunk.single))

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks(chunk => chunk.flatMap(a => encode(a)))
    }
}
