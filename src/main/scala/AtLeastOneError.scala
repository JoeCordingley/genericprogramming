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

