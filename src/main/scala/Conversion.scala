given Conversion[Double, JsonDouble] = JsonDouble(_)
given Conversion[Boolean, JsonBool] = JsonBool(_)

given [A, B](using Conversion[A, B]): Conversion[List[A], JsonArray[B]] = l => JsonArray(l.map(identity))

given [A, B](using Conversion[A, B]): Conversion[Option[A], Either[JsonNull, B]] = {
  case Some(b) => Right(b)
  case None => Left(JsonNull)
}

//
//  type ProductOfEs[T, E] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes = E}
//  type NamedProduct[T, E, K] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes = E; type MirroredElemLabels = K}
//
//  given [A] (using ProductOfEs[A, EmptyTuple]): Conversion[A, EmptyTuple] = _ => EmptyTuple
//  given [A, B](using Conversion[A, B]): Conversion[A, JsonObject[B]] = a => JsonObject(a)
//  given [A, K, V, T <: Tuple]: Conversion[A, JsonPair[K, V] *: T] = a => ??? 


//def derived[A, B](using Mirror.Of[A]): Conversion[A, B] = ???

