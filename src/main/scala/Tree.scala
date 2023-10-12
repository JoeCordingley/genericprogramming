
case class Fix[F[_]](unfix: F[Fix[F]])

enum Tree[A]:
  case Branch(left: Tree[A], right: Tree[A])
  case Leaf(value: A)

given [F[_]](using e: => Encoder[F[Fix[F]]]): Encoder[Fix[F]] = fix => e.encode(fix.unfix)

type JsonBranchUnfixed[T] = JsonObject[(JsonMember["left", T], JsonMember["right", T])]
type JsonTree[A] = Fix[[T] =>> Either[JsonBranchUnfixed[T], A]]

def convertTree[A, B]: Tree[A] => JsonTree[A] = {
  case Tree.Branch(left, right) => Fix(Left(JsonObject((JsonMember(convertTree(left)), JsonMember(convertTree(right))))))
  case Tree.Leaf(value) => Fix(Right(value))
}

//val x: Json = summon[Encoder[JsonTree[JsonInt]]].encode(summon[Conversion[Tree[Int], JsonTree[JsonInt]]].convert(Tree.Branch(Tree.Leaf(1), Tree.Branch(Tree.Leaf(2), Tree.Leaf(3)))))
val x: Json = summon[Encoder[JsonTree[JsonInt]]].encode(convertTree(Tree.Branch(Tree.Leaf(JsonInt(1)), Tree.Branch(Tree.Leaf(JsonInt(2)), Tree.Leaf(JsonInt(3))))))

