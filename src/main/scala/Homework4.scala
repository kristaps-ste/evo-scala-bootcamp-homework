package com.evolutiongaming.bootcamp.basics

object DataStructures {

 def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    val grouped = map.groupBy { case (_, value) => value }
    val sorted = grouped.toList.sortBy { case (value, _) => value }
    val normalized = sorted.map { case (_, value) => value.unzip }
    normalized.map { case (k, v) => (k.toSet, v.head) }
  }
}
