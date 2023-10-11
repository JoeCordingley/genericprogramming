import scala.deriving.Mirror

enum Json:
  case JsonString(s: String)
  case JsonInt(n: Int)
  case JsonDouble(d: Double)
  case JsonBool(b: Boolean)
  case JsonNull
  case JsonArray(elements: List[Json])
  case JsonObject(fields: Map[String, Json])

trait Encoder[A]: 
  def encode(a: A): Json

trait Decoder[A]:
  def decode(a: Json): Either[JsonError[A], A]

enum JsonError[A]:
  case ExpectedNull extends JsonError[JsonNull]
  case ExpectedInt extends JsonError[JsonInt]
  case ExpectedDouble extends JsonError[JsonDouble]
  case ExpectedBool extends JsonError[JsonBool]
  case ExpectedArray[A]() extends JsonError[JsonArray[A]]
  case ArrayErrors[A](errors: AtLeastOneError[JsonError[A], A]) extends JsonError[JsonArray[A]]

case class AtLeastOneError[A, B](bs: List[B], a: A, collected: Either[AtLeastOneError[A, B], List[B]])
object AtLeastOneError {
  def collect[A, B](
    e: Either[A, B], 
    collected: Either[AtLeastOneError[A, B], List[B]]
  ): Either[AtLeastOneError[A, B], List[B]] = for {
    b <- e.left.map(AtLeastOneError(Nil, _, collected))
    bs <- collected.left.map{
      case AtLeastOneError(bs, a, collected) => AtLeastOneError(b :: bs, a, collected)
    }
  } yield b :: bs
}



case object JsonNull
type JsonNull = JsonNull.type
case class JsonInt(n: Int)
case class JsonDouble(d: Double)
case class JsonBool(b: Boolean)
case class JsonArray[A](elements: List[A])
case class JsonObject[A](pairs: A)
case class JsonPair[K, V](value: V)

object SpecificJson:

  given Encoder[JsonNull] = _ => Json.JsonNull
  given Decoder[JsonNull] = {
    case Json.JsonNull => Right(JsonNull)
    case _ => Left(JsonError.ExpectedNull)
  }

  given Encoder[JsonInt] = n => Json.JsonInt(n.n)
  given Decoder[JsonInt] = {
    case Json.JsonInt(n) => Right(JsonInt(n))
    case _ => Left(JsonError.ExpectedInt)
  }

  given Encoder[JsonDouble] = d => Json.JsonDouble(d.d)
  given Decoder[JsonDouble] = {
    case Json.JsonDouble(d) => Right(JsonDouble(d))
    case Json.JsonInt(i) => Right(JsonDouble(i.toDouble))
    case _ => Left(JsonError.ExpectedDouble)
  }

  given Encoder[JsonBool] = b => Json.JsonBool(b.b)
  given Decoder[JsonBool] = {
    case Json.JsonBool(b) => Right(JsonBool(b))
    case _ => Left(JsonError.ExpectedBool)
  }

  given [A: Encoder]: Encoder[JsonArray[A]] = a => Json.JsonArray(a.elements.map(summon[Encoder[A]].encode))
  given [A: Decoder]: Decoder[JsonArray[A]] = {
    case Json.JsonArray(elements) => elements.foldRight[Either[AtLeastOneError[JsonError[A], A], List[A]]](Right(Nil)){
      case (element, maybeErrors) => AtLeastOneError.collect(summon[Decoder[A]].decode(element), maybeErrors) 
    }.left.map(JsonError.ArrayErrors(_)).map(JsonArray(_))
    case _ => Left(JsonError.ExpectedArray())
  }

  given [A: JsonPairsEncoder]: Encoder[JsonObject[A]] = a => Json.JsonObject(summon[JsonPairsEncoder[A]].encode(a.pairs).toMap)


  trait JsonFieldEncoder[A]:
    def encode: String


  given [A <: String: ValueOf]: JsonFieldEncoder[A] with
    def encode: String = summon[ValueOf[A]].value

  trait JsonPairsEncoder[A]:
    def encode(a: A): List[(String, Json)]

  trait JsonMapDecoder[A]:
    def decode(m: Map[String, Json]): Either[JsonObjectError[A], A] 

  type JsonObjectError[A]

  enum JsonFieldError[K, V]:
    case MissingField[K, V]() extends JsonFieldError[K, V]
    case ValueError[K, V](error: JsonError[V]) extends JsonFieldError[K, V]

  given JsonPairsEncoder[EmptyTuple] = _ => List.empty

  given [K: JsonFieldEncoder, V: Encoder, T <: Tuple: JsonPairsEncoder]: JsonPairsEncoder[JsonPair[K, V] *: T] =
    tuple => (summon[JsonFieldEncoder[K]].encode, summon[Encoder[V]].encode(tuple.head.value)) :: summon[JsonPairsEncoder[T]].encode(tuple.tail)

  given [K: JsonFieldEncoder, V: Decoder, T <: Tuple: JsonMapDecoder]: JsonMapDecoder[JsonPair[K, V] *: T] = {
    fields => 
      val maybePair = for {
        j <- fields.get(summon[JsonFieldEncoder[K]].encode).toRight(JsonFieldError.MissingField[K, V]())
        v <- summon[Decoder[V]].decode(j).left.map(JsonFieldError.ValueError[K, V](_))
      } yield JsonPair[K, V](v)
      collect(maybePair, summon[JsonMapDecoder[T]].decode(fields))
  }

  def collect[K, V, T <: Tuple](e: Either[JsonFieldError[K, V], JsonPair[K, V]], r: Either[JsonObjectError[T], T]): Either[JsonObjectError[JsonPair[K, V] *: T], JsonPair[K, V] *: T] = ???


  given [L: Encoder, R: Encoder]: Encoder[Either[L, R]] = {
    case Left(l) => summon[Encoder[L]].encode(l)
    case Right(r) => summon[Encoder[R]].encode(r)
  }

  //given [A, B: Encoder](using Conversion[A, B]): Encoder[A] = a => summon[Encoder[B]].encode(a)
  given Conversion[Int, JsonInt] = JsonInt(_)
  given Conversion[Double, JsonDouble] = JsonDouble(_)
  given Conversion[Boolean, JsonBool] = JsonBool(_)

  given [A, B](using Conversion[A, B]): Conversion[List[A], JsonArray[B]] = l => JsonArray(l.map(identity))

  given [A, B](using Conversion[A, B]): Conversion[Option[A], Either[JsonNull, B]] = {
    case Some(b) => Right(b)
    case None => Left(JsonNull)
  }

  case class Fix[F[_]](unfix: F[Fix[F]])

  enum Tree[A]:
    case Branch(left: Tree[A], right: Tree[A])
    case Leaf(value: A)

  given [F[_]](using e: => Encoder[F[Fix[F]]]): Encoder[Fix[F]] = fix => e.encode(fix.unfix)

  type JsonBranchUnfixed[T] = JsonObject[(JsonPair["left", T], JsonPair["right", T])]
  type JsonTreeUnfixed[T, A] = Either[JsonBranchUnfixed[T], A]
  type JsonTree[A] = Fix[[T] =>> JsonTreeUnfixed[T, A]]

  given [A, B](using Conversion[A, B]): Conversion[Tree[A], JsonTree[B]] = {
    case Tree.Branch(left, right) => Fix(Left(JsonObject((JsonPair(left), JsonPair(right)))))
    case Tree.Leaf(value) => Fix(Right(value))
  }

  //val x: Json = summon[Encoder[JsonTree[JsonInt]]].encode(summon[Conversion[Tree[Int], JsonTree[JsonInt]]].convert(Tree.Branch(Tree.Leaf(1), Tree.Branch(Tree.Leaf(2), Tree.Leaf(3)))))
  val x: Json = summon[Encoder[JsonTree[JsonInt]]].encode(Tree.Branch(Tree.Leaf(1), Tree.Branch(Tree.Leaf(2), Tree.Leaf(3))))
//
//  type ProductOfEs[T, E] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes = E}
//  type NamedProduct[T, E, K] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes = E; type MirroredElemLabels = K}
//
//  given [A] (using ProductOfEs[A, EmptyTuple]): Conversion[A, EmptyTuple] = _ => EmptyTuple
//  given [A, B](using Conversion[A, B]): Conversion[A, JsonObject[B]] = a => JsonObject(a)
//  given [A, K, V, T <: Tuple]: Conversion[A, JsonPair[K, V] *: T] = a => ??? 


  //def derived[A, B](using Mirror.Of[A]): Conversion[A, B] = ???

