import scala.deriving.Mirror

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

object SpecificJson:
  case object JsonNull
  type JsonNull = JsonNull.type
  case class JsonInt(n: Int)
  case class JsonDouble(d: Double)
  case class JsonBool(b: Boolean)
  case class JsonArray[A](elements: List[A])
  case class JsonObject[A](pairs: A)
  case class JsonPair[K, V](value: V)

  given Encoder[JsonNull] = _ => Json.JsonNull

  given Encoder[JsonInt] = n => Json.JsonInt(n.n)

  given Encoder[JsonDouble] = d => Json.JsonDouble(d.d)

  given Encoder[JsonBool] = b => Json.JsonBool(b.b)

  given [A: Encoder]: Encoder[JsonArray[A]] = a => Json.JsonArray(a.elements.map(summon[Encoder[A]].encode))

  given [A: JsonPairsEncoder]: Encoder[JsonObject[A]] = a => Json.JsonObject(summon[JsonPairsEncoder[A]].encode(a.pairs))

  trait JsonFieldEncoder[A]:
    def encode: String

  given [A <: String: ValueOf]: JsonFieldEncoder[A] with
    def encode: String = summon[ValueOf[A]].value

  trait JsonPairsEncoder[A]:
    def encode(a: A): List[(String, Json)]

  given JsonPairsEncoder[EmptyTuple] = _ => List.empty

  given [K: JsonFieldEncoder, V: Encoder, T <: Tuple: JsonPairsEncoder]: JsonPairsEncoder[JsonPair[K, V] *: T] =
    tuple => (summon[JsonFieldEncoder[K]].encode, summon[Encoder[V]].encode(tuple.head.value)) :: summon[JsonPairsEncoder[T]].encode(tuple.tail)

  given [L: Encoder, R: Encoder]: Encoder[Either[L, R]] = {
    case Left(l) => summon[Encoder[L]].encode(l)
    case Right(r) => summon[Encoder[R]].encode(r)
  }

  given [A, B: Encoder](using Conversion[A, B]): Encoder[A] = a => summon[Encoder[B]].encode(a)
  given Conversion[Int, JsonInt] = JsonInt(_)
  given Conversion[Double, JsonDouble] = JsonDouble(_)
  given Conversion[Boolean, JsonBool] = JsonBool(_)

  given [A, B](using Conversion[A, B]): Conversion[List[A], JsonArray[B]] = l => JsonArray(l.map(identity))

  given [A, B](using Conversion[A, B]): Conversion[Option[A], Either[JsonNull, B]] = {
    case Some(b) => Right(b)
    case None => Left(JsonNull)
  }

  case class Fix[F[_]](unfix: F[Fix[F]])

  type FixEitherRightEnding[F[_], A] = Fix[[T] =>> Either[F[T], A]]
  type FixEitherLeftEnding[F[_], A] = Fix[[T] =>> Either[A, F[T]]]

  //type L //: R = Either[L, R]

  enum Tree[A]:
    case Branch(left: Tree[A], right: Tree[A])
    case Leaf(value: A)

  //given[F[_]](using Encoder[F[Fix[F]]]): Encoder[Fix[F]] = fix => summon[Encoder[F[Fix[F]]]].encode(fix.unfix)



  type JsonBranchUnfixed[T] = JsonObject[(JsonPair["left", T], JsonPair["right", T])]

  given EncoderK[JsonBranchUnfixed] with 
    def encoder[A: Encoder]: Encoder[JsonBranchUnfixed[A]] = summon[Encoder[JsonBranchUnfixed[A]]]
  //type JsonTreeUnfixed[A, T] = Either[JsonBranchUnfixed[T], A]
  //type JsonTree[A] = Fix[[T] =>> JsonTreeUnfixed[A, T]]
  type JsonTree[A] = FixEitherRightEnding[JsonBranchUnfixed, A]

//  given [A: Encoder]: Encoder[JsonTree[A]] = _.unfix match {
//    case Right(a) => summon[Encoder[A]].encode(a)
//    case Left(j) => summon[Encoder[JsonBranchUnfixed[JsonTree[A]]]].encode(j)
//  }

  given [A: Encoder, F[_]](using EncoderK[F]): Encoder[FixEitherRightEnding[F, A]] = _.unfix match {
    case Right(a) => summon[Encoder[A]].encode(a)
    case Left(j) => summon[EncoderK[F]].encoder[FixEitherRightEnding[F, A]].encode(j)
  }

  trait EncoderK[F[_]]{
    def encoder[A: Encoder]: Encoder[F[A]]
  }
//
//  given fixEitherLeftEnding[A: Encoder]: Encoder[FixEitherLeftEnding[A]] = _.unfix match {
//    case Left(a) => summon[Encoder[A]].encode(a)
//    case Right(j) => summon[Encoder[FixEitherLeftEnding[A]]].encode(j)
//  }

  given [A, B](using Conversion[A, B]): Conversion[Tree[A], JsonTree[B]] = {
    case Tree.Branch(left, right) => Fix(Left(JsonObject((JsonPair(left), JsonPair(right)))))
    case Tree.Leaf(value) => Fix(Right(value))
  }


  val x: Json = summon[Encoder[JsonTree[JsonInt]]].encode(summon[Conversion[Tree[Int], JsonTree[JsonInt]]].convert(Tree.Branch(Tree.Leaf(1), Tree.Branch(Tree.Leaf(2), Tree.Leaf(3)))))

//
//  type ProductOfEs[T, E] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes = E}
//  type NamedProduct[T, E, K] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes = E; type MirroredElemLabels = K}
//
//  given [A] (using ProductOfEs[A, EmptyTuple]): Conversion[A, EmptyTuple] = _ => EmptyTuple
//  given [A, B](using Conversion[A, B]): Conversion[A, JsonObject[B]] = a => JsonObject(a)
//  given [A, K, V, T <: Tuple]: Conversion[A, JsonPair[K, V] *: T] = a => ??? 


  //def derived[A, B](using Mirror.Of[A]): Conversion[A, B] = ???

