package com.evolutiongaming.bootcamp.basics
import scala.util.Try

object DataStructures {

  def sortConsideringEqualValues[T: Ordering](map: Map[T, Int]): List[(Set[T], Int)] = {
    map
      .groupBy({ case (_, value) => value })
      .map({ case (_, value) => value })
      .map(_.unzip)
      .map({case (key, value) => (key.toSeq.sorted.toSet, value.head)})
      .toList
  }
}
