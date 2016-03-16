package common

/**
  * Created by hb on 3/16/16.
  */
object sets {

  def minus[T](s1: Set[T], s2: Set[T]): Set[T] = s1 filterNot (s2 contains _)

  def symmdiff[T](s1: Set[T], s2: Set[T]): Set[T] = minus(s1,s2) ++ minus(s2,s1)

}
