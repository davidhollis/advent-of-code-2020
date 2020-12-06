package util

trait Semigroup[T] {
  def combine(a: T, b: T): T
  def zero: T
}

object Semigroup {
  def apply[T](implicit s: Semigroup[T]): Semigroup[T] = s
  def apply[T](z: T, c: (T, T) => T): Semigroup[T] = new Semigroup[T] {
    def combine(a: T, b: T): T = c(a, b)
    def zero: T = z
  }
}
