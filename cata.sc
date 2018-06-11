// https://www.youtube.com/watch?reload=9&v=XZ9nPZbaYfE
// http://slides.com/zainabali_/peeling_the_banana#/

import scala.language.higherKinds

// recursive data structure
sealed trait List
case object Nil extends List
case class Cons(head: Int, tail: List) extends List


Cons(2, Cons(5, Nil))

// cata is a generalized fold. From List to Int. From Tree to Int...
// From a recursive data structure to a value


// higher kinded types

sealed trait ListF[A]
case class NilF[A]() extends ListF[A]
case class ConsF[A](head: Int, tail: A) extends ListF[A]

NilF()
ConsF(2, ConsF(5, NilF()))
ConsF(2, "hello")
ConsF(2, ConsF(5, NilF[List]()))

// isomorphism
// R -> (in)  FR
//   <- (out)

val in: ListF[List] => List = {
  case NilF() ⇒ Nil
  case ConsF(h, t) ⇒ Cons(h, t)
}

val out: List ⇒ ListF[List] = {
  case Nil ⇒ NilF()
  case Cons(h, t) ⇒ ConsF(h, t)
}

val l: ListF[List] = NilF[List]()
val l2: ListF[List] = ConsF[List](2, Nil)

in(l)
in(l2)
//in.apply(ConsF(2, ConsF(5, NilF[List]())))

out(Cons(2, Cons(5, Nil)))

// with higher-kind -> Functor
trait Functor[F[_]] {
  def map[A, B](f: A ⇒ B): F[A] ⇒ F[B]
}

implicit val listFFunctor: Functor[ListF] = new Functor[ListF] {
  override def map[A, B](f: A ⇒ B): ListF[A] ⇒ ListF[B] = {
    case NilF() ⇒ NilF()
    case ConsF(h, a) ⇒ ConsF(h, f(a))
  }
}

// with functor -> F-Algebra
type Algebra[F[_], A] = F[A] => A

// same as 'in'
val in2: Algebra[ListF, List] = {
  case NilF() ⇒ Nil
  case ConsF(h, t) ⇒ Cons(h, t)
}

in2(l)
in2(l2)

val multiplyAlgebra: Algebra[ListF, Int] = {
  case NilF() ⇒ 1
  case ConsF(h, t) ⇒ h * t
}

// cata = out -> map cata -> alg
def cata[F[_], R, A](algebra: F[A] ⇒ A, out: R ⇒ F[R])(r: R)(implicit F: Functor[F]): A = {
  val FR = out(r)
  val mapCata: R ⇒ A = cata(algebra, out)
  val FA = F.map(mapCata)(FR)
  algebra(FA)
}


def multiply: List => Int = cata[ListF, List, Int](multiplyAlgebra, out)

multiply(Cons(2, Cons(5, Nil)))


// ana to build (generalized build)
def range: Int => List =
  n => if (n > 0) Cons(n, range(n - 1)) else Nil

range(5)

// ana = coalg -> map ana -> in
type Coalgebra[F[_], A] = A => F[A]

def ana[F[_], R, A](coalgebra: Coalgebra[F, A], in: F[R] ⇒ R)(a: A)(implicit F: Functor[F]): R = {
  val f: A ⇒ R = ana(coalgebra, in)
  in(F.map(f)(coalgebra(a)))
}

// ana[ListF, List, Int]
//
def rangeCoalgebra: Coalgebra[ListF, Int] =
  n => if (n > 0) ConsF(n, n - 1) else NilF()

def range2: Int => List = ana(rangeCoalgebra, in)

range2(5)


// generalized recursion
// hylomorphism

def factorial: Int ⇒ Int =
  n ⇒  if (n == 0) 1 else n * factorial(n - 1)

factorial(4)

// hylo = coalg -> map hylo -> alg
def hylo[F[_], A, B](coalgebra: Coalgebra[F, A], algebra: Algebra[F, B])
                    (a: A)(implicit F: Functor[F]): B = {
  val f: A ⇒ B = hylo(coalgebra, algebra)
  algebra(F.map(f)(coalgebra(a)))
}

def factorial2: Int ⇒ Int = hylo(rangeCoalgebra, multiplyAlgebra)
factorial2(4)

// avoid writing in and out by hand with Fix

case class Fix[F[_]](unfix: F[Fix[F]])

def in3: ListF[Fix[ListF]] ⇒ Fix[ListF] = Fix(_)
def out3: Fix[ListF] ⇒ ListF[Fix[ListF]] = _.unfix


in3(ConsF(3, Fix(NilF())))

// I don't understand how it helps
// def range2: Int => List = ana(rangeCoalgebra, in3) //cannot compile
