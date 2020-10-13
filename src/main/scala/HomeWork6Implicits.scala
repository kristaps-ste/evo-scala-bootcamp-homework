package com.evolutiongaming.bootcamp.typeclass
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {

  /**
    * Lo and behold! Brand new super-useful collection library for Scala!
    *
    * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
    * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
    * of the data stored.
    *
    * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
    * a thing called size score. Its calculation rules:
    * - size score of a Byte is 1
    * - Int - 4 (as primitive JVM int consists of 4 bytes)
    * - Long - 8
    * - Char - 2 (one UTF-16 symbol is 2 bytes)
    * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
    * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
    * the fields
    * - score for any sequence (Array[T], List[T], Vector[T]) is
    * 12 (our old friend object header) + sum of scores of all elements
    * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
    */
  object SuperVipCollections4s {
    type SizeScore = Int
    val jvmHeaderLength: SizeScore = 12
    trait GetSizeScore[T] {
      def calculateSizeScore(value: T): SizeScore

    }
    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](itemWhereScoreMustBeCalculated: T) {

        def sizeScore(implicit GetSizeScoreInstance: GetSizeScore[T]): SizeScore =
          GetSizeScoreInstance.calculateSizeScore(itemWhereScoreMustBeCalculated)
      }

    }

    /**
      * Mutable key-value cache which limits the size score of the data scored.
      *
      * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
      * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
      * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
      * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
      *
      * @param maxSizeScore max size score for the stored data
      * @tparam K key type
      * @tparam V value type
      */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */

      private val map = mutable.LinkedHashMap.empty[K, V]

      def get(key: K): Option[V] = map.get(key)
      def put(key: K, value: V): Unit = {
        val inputSizeScore: SizeScore = key.sizeScore + value.sizeScore

        if (inputSizeScore <= maxSizeScore) {

          freeUpSpaceIfNeeded(inputSizeScore, currentFreeSize)
          map.addOne(key, value)
        }
        // if input sizeScore is  greater than max , then no point to clear all the cache,
        // leave  put() execution silently as undefined
      }

      private def freeUpSpaceIfNeeded(inputSize: SizeScore, freeSize: SizeScore): Unit = {

        map.headOption match {
          case None                         =>
          case _ if (inputSize <= freeSize) =>
          case Some((key, value)) => {

            val sizeAllocated = value.sizeScore + key.sizeScore
            map.remove(key)
            freeUpSpaceIfNeeded(inputSize, freeSize + sizeAllocated)
          }
        }
      }
      private def currentFreeSize: SizeScore = (maxSizeScore - currentSize)
      private def currentSize: SizeScore = {
        map.map { case (key, value) => key.sizeScore + value.sizeScore }.sum
      }
    }

    /**
      * Cool custom immutable multi-map collection - does not extend the standard library collection types
      * (yes, this is a feature)
      */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] =
        PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
      * Type-class allowing us to iterate over different "collection-like" types with one type arg
      */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
      * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
      */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val packedIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = ???

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = ???
      }
      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      implicit def byteScore: GetSizeScore[Byte] = (_: Byte) => 1
      implicit def charScore: GetSizeScore[Char] = (_: Char) => 2
      implicit def intScore: GetSizeScore[Int] = (_: Int) => 4
      implicit def longScore: GetSizeScore[Long] = (_: Long) => 8
      implicit def stringScore: GetSizeScore[String] = (s: String) => jvmHeaderLength + s.size * 2

      //Array, List, Vector in generic form
      implicit def sequenceScore[A[_], T: GetSizeScore](implicit e: A[T] => Iterable[T]): GetSizeScore[A[T]] =
        (sequence: A[T]) => jvmHeaderLength + sequence.map(_.sizeScore).sum

      implicit def mapScore[K: GetSizeScore, V: GetSizeScore]: GetSizeScore[Map[K, V]] =
        (mapToClc: Map[K, V]) =>
          jvmHeaderLength +
            mapToClc.map { case (key, value) => key.sizeScore + value.sizeScore }.sum

      implicit def packedMapScore[K: GetSizeScore, V: GetSizeScore]: GetSizeScore[PackedMultiMap[K, V]] =
        (map: PackedMultiMap[K, V]) =>
          jvmHeaderLength +
            map.inner.map { case (key, value) => key.sizeScore + value.sizeScore }.sum

      //implicit def stubGetSizeScore[T]: GetSizeScore[T] = (_: T) => 42
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import syntax._
    import instances._
    final case class Twit(
        id: Long,
        userId: Int,
        hashTags: Vector[String],
        attributes: PackedMultiMap[String, String],
        fbiNotes: List[FbiNote]
    )
    implicit def twitSizeScore: GetSizeScore[Twit] =
      (t: Twit) => {
        t.id.sizeScore +
          t.userId.sizeScore +
          t.hashTags.sizeScore +
          t.attributes.sizeScore +
          t.fbiNotes.sizeScore
      }

    final case class FbiNote(
        month: String,
        favouriteChar: Char,
        watchedPewDiePieTimes: Long
    )
    implicit def noteSizeScore: GetSizeScore[FbiNote] =
      (n: FbiNote) => {
        n.month.sizeScore +
          n.favouriteChar.sizeScore +
          n.watchedPewDiePieTimes.sizeScore
      }

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache =
      new TwitCache {
        override def put(twit: Twit): Unit = dataStore.put(twit.id, twit)
        override def get(id: Long): Option[Twit] = dataStore.get(id)
        private val dataStore = new MutableBoundedCache[Long, Twit](maxSizeScore)
      }
  }
}
