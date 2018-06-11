import scala.language.higherKinds

// the functor takes a "container" F[_], a function A -> B, and transforms F[A] -> F[B]
// A -> B
//
// F[A] -> F[B]


trait Functor[F[_]] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
}

// infix notation. Container.myMap(function)
implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
  def myMap[B](f: A ⇒ B): F[B] =
    F.map(f)(fa)
}

// with the container Option
implicit val optionFunction: Functor[Option] = new Functor[Option] {
  override def map[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = {
    case None ⇒ None
    case Some(a) ⇒ Some(f(a))
  }
}

val doubleOp: Int => Int = n => n * 2
def double[F[_]](ini: F[Int])(implicit F: Functor[F]): F[Int] =
  F.map(doubleOp)(ini)

optionFunction.map(doubleOp)(Some(3))
optionFunction.map(doubleOp)(None)
double[Option](Some(3))
double[Option](None)


// with the container Vector
implicit val vectorFunctor: Functor[Vector] = new Functor[Vector] {
  override def map[A, B](f: A ⇒ B): Vector[A] ⇒ Vector[B] = _.map(f)
}

vectorFunctor.map(doubleOp)(Vector(4, 5))
double(Vector(4, 5))
Vector(4, 5).myMap(doubleOp)
