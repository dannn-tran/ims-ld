package imsld.services

import cats.data.State
import cats.syntax.all.*
import skunk.data.{Arr, Type}
import skunk.{Codec, Decoder}

enum CompositeType:
  case Record
  case Custom(name: String)

object CompositeTypeCodec:
  private def unparameterize(name: String): String =
    name match
      case s"$_name($n)" => _name
      case _             => name

  def mkComposite[A <: Product](
      codec: Codec[A],
      desc: CompositeType = CompositeType.Record
  ): Codec[A] =
    new Codec[A]:
      override def encode(a: A): List[Option[String]] =
        val elem = codec.encode(a).flattenOption.mkString("(", ",", ")")
        List(Some(elem))

      override val sql: State[Int, String] =
        codec.sql.map(a => s"($a)")

      override val types: List[Type] =
        desc match
          case CompositeType.Record =>
            List(Type.record)
          case CompositeType.Custom(name) =>
            // parameterized types must be unparameterized
            val types =
              codec.types.map(ty => ty.copy(name = unparameterize(ty.name)))
            List(Type(name, types))

      override def decode(
          offset: Int,
          s: List[Option[String]]
      ): Either[Decoder.Error, A] =
        s match
          case Some(s"($inner)") :: Nil =>
            val composites = inner.split(",").map(_.some).toList
            codec.decode(offset, composites)
          case None :: Nil =>
            Left(
              Decoder.Error(
                offset,
                1,
                s"Unexpected NULL value in non-optional column."
              )
            )
          case _ =>
            Left(
              Decoder.Error(
                offset,
                1,
                s"Expected one input value to decode, got ${s.length}."
              )
            )

extension [A <: Product](self: Codec[A])
  def asArr: Codec[Arr[A]] =
    val ty = Type(s"_${self.types.head.name}", self.types)
    val encode = (elem: A) => self.encode(elem).head.get
    val decode = (str: String) =>
      self.decode(0, List(Some(str))).left.map(_.message)
    Codec.array(encode, decode, ty)
