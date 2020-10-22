package com.evolutiongaming.bootcamp.json
import java.time.{LocalDate}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.{Decoder}
import io.circe.parser._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec, JsonKey}
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

/**
  * HOMEWORK:
  *
  * Some classes and generated JSON codecs are provided for NBA API.
  * Unfortunately, they don't work as expected out of the box.
  * The task is to fix (rewrite) some of the codecs to make tests pass.
  * You are not supposed to change anything in _class_ HomeworkSpec,
  * instead of it you are supposed to change whatever you want inside _companion object_ for HomeworkSpec.
  *
  * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
  */
class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }

}

object HomeworkSpec {
  import java.time.format.DateTimeFormatter
  import java.time.{LocalDate, ZonedDateTime}
  import cats.implicits._

  val formatter = DateTimeFormatter.ofPattern("yyyyMMdd")
  implicit val dateDecoder: Decoder[LocalDate] =
    Decoder.decodeString.emap[LocalDate](str => {
      Either.catchNonFatal(LocalDate.parse(str, formatter)).leftMap(_.getMessage)
    })

  implicit val useDefaultValues = Configuration.default.withDefaults
  @ConfiguredJsonCodec final case class TeamTotals(
      points: Int,
      assists: String,
      @JsonKey("full_timeout_remaining") fullTimeoutRemaining: String,
      plusMinus: String,
      @JsonKey("team_fouls") teamFouls: Int
  )
  @JsonCodec final case class Person(firstName: String, lastName: String)
  @JsonCodec final case class Leaders(
      points: LeaderContainer,
      rebounds: LeaderContainer,
      assists: LeaderContainer
  )
  @JsonCodec final case class LeaderContainer(value: String, players: List[Person]);
  @JsonCodec final case class TeamBoxScore(totals: TeamTotals, leaders: Leaders)
  @JsonCodec final case class GameStats(
      hTeam: TeamBoxScore,
      vTeam: TeamBoxScore,
      activePlayers: List[Person]
  )
  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)

  @JsonCodec final case class BoxScore(
      basicGameData: Game,
      previousMatchup: PrevMatchup,
      stats: Option[GameStats]
  )

  @JsonCodec final case class JustScore(score: String)
  @JsonCodec final case class TeamStats(
      linescore: List[JustScore],
      loss: String,
      score: String,
      teamId: String,
      triCode: String
  )
  @JsonCodec final case class GameDuration(hours: String, minutes: String)
  @JsonCodec final case class Arena(
      city: String,
      country: String,
      isDomestic: Boolean,
      name: String,
      stateAbbr: String
  )
  @JsonCodec final case class Game(
      arena: Arena,
      attendance: String,
      endTimeUTC: Option[ZonedDateTime],
      gameDuration: GameDuration,
      gameId: String,
      gameUrlCode: String,
      hTeam: TeamStats,
      isBuzzerBeater: Boolean,
      startTimeUTC: ZonedDateTime,
      vTeam: TeamStats
  )
  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(
      s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
    ).asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(
      date: LocalDate,
      gameId: String
  ): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(
      s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
    ).asString.body
    decode[BoxScore](body)
  }
}
