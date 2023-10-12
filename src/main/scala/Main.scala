import scala.deriving.Mirror
import cats.syntax.all.*

enum Json:
  case JsonString(s: String)
  case JsonInt(n: Int)
  case JsonDouble(d: Double)
  case JsonBool(b: Boolean)
  case JsonNull
  case JsonArray(elements: List[Json])
  case JsonObject(fields: List[(String, Json)])

trait Encoder[A]: 
  def encode(a: A): Json

trait Decoder[A]:
  def decode(a: Json): Option[A]

case object JsonNull
type JsonNull = JsonNull.type
case class JsonInt(n: Int)
case class JsonDouble(d: Double)
case class JsonBool(b: Boolean)
case class JsonArray[A](elements: List[A])
case class JsonObject[A](pairs: A)
case class JsonMember[K, V](value: V)
case class JsonOptionalMember[K, V](maybeMember: Option[JsonMember[K, V]])


given Encoder[JsonNull] = _ => Json.JsonNull
given Decoder[JsonNull] = {
  case Json.JsonNull => Some(JsonNull)
  case _ => None
}

given Encoder[JsonInt] = n => Json.JsonInt(n.n)
given Decoder[JsonInt] = {
  case Json.JsonInt(n) => Some(JsonInt(n))
  case _ => None
}

given Encoder[JsonDouble] = d => Json.JsonDouble(d.d)
given Decoder[JsonDouble] = {
  case Json.JsonDouble(d) => Some(JsonDouble(d))
  case Json.JsonInt(i) => Some(JsonDouble(i.toDouble))
  case _ => None
}

given Encoder[JsonBool] = b => Json.JsonBool(b.b)
given Decoder[JsonBool] = {
  case Json.JsonBool(b) => Some(JsonBool(b))
  case _ => None
}

given [A: Encoder]: Encoder[JsonArray[A]] = a => Json.JsonArray(a.elements.map(summon[Encoder[A]].encode))
given [A: Decoder]: Decoder[JsonArray[A]] = {
  case Json.JsonArray(elements) => elements.traverse(summon[Decoder[A]].decode).map(JsonArray(_))
  case _ => None
}

given [A: JsonMemberEncoder]: Encoder[JsonObject[A]] = a => Json.JsonObject(summon[JsonMemberEncoder[A]].encode(a.pairs))

trait JsonFieldEncoder[A]:
  def encode: String

given [A <: String: ValueOf]: JsonFieldEncoder[A] with
  def encode: String = summon[ValueOf[A]].value

trait JsonMemberEncoder[A]:
  def encode(a: A): List[(String, Json)]

trait JsonMemberDecoder[A]:
  def decode(pairs: Map[String, Json]): Option[A] 

given JsonMemberEncoder[EmptyTuple] = _ => List.empty
given JsonMemberDecoder[EmptyTuple] = _ => Some(EmptyTuple)

given [K: JsonFieldEncoder, V: Encoder, T <: Tuple: JsonMemberEncoder]: JsonMemberEncoder[JsonMember[K, V] *: T] =
  tuple => (summon[JsonFieldEncoder[K]].encode, summon[Encoder[V]].encode(tuple.head.value)) :: summon[JsonMemberEncoder[T]].encode(tuple.tail)

given [A: JsonMemberDecoder]: Decoder[JsonObject[A]] = {
  case Json.JsonObject(pairs) => summon[JsonMemberDecoder[A]].decode(pairs.toMap).map(JsonObject(_))
  case _ => None
}

given [K: JsonFieldEncoder, V: Decoder]: JsonMemberDecoder[JsonMember[K, V]] = map => for {
  j <- map.get(summon[JsonFieldEncoder[K]].encode)
  v <- summon[Decoder[V]].decode(j)
} yield JsonMember(v)

given [K: JsonFieldEncoder, V: Decoder]: JsonMemberDecoder[JsonOptionalMember[K, V]] = _.get(summon[JsonFieldEncoder[K]].encode).traverse(summon[Decoder[V]].decode(_)).map(o => JsonOptionalMember(o.map(JsonMember(_))))

given [L: Decoder, R: Decoder]: Decoder[Either[L, R]] = j => summon[Decoder[L]].decode(j).map(Left(_)).orElse(summon[Decoder[R]].decode(j).map(Right(_)))

given [A: JsonMemberDecoder, T <: Tuple: JsonMemberDecoder]: JsonMemberDecoder[A *: T] = map => for {
  v <- summon[JsonMemberDecoder[A]].decode(map)
  t <- summon[JsonMemberDecoder[T]].decode(map)
} yield v *: t

given [L: Encoder, R: Encoder]: Encoder[Either[L, R]] = {
  case Left(l) => summon[Encoder[L]].encode(l)
  case Right(r) => summon[Encoder[R]].encode(r)
}


//
//  type ProductOfEs[T, E] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes = E}
//  type NamedProduct[T, E, K] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes = E; type MirroredElemLabels = K}
//
//  given [A] (using ProductOfEs[A, EmptyTuple]): Conversion[A, EmptyTuple] = _ => EmptyTuple
//  given [A, B](using Conversion[A, B]): Conversion[A, JsonObject[B]] = a => JsonObject(a)
//  given [A, K, V, T <: Tuple]: Conversion[A, JsonPair[K, V] *: T] = a => ??? 


//def derived[A, B](using Mirror.Of[A]): Conversion[A, B] = ???

