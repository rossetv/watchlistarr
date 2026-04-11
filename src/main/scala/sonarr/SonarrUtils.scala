package sonarr

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import configuration.SonarrConfiguration
import http.HttpClient
import io.circe.{Decoder, Json}
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import model.Item
import org.http4s.{MalformedMessageBodyFailure, Method, Uri}
import org.slf4j.LoggerFactory

trait SonarrUtils extends SonarrConversions {

  private val logger = LoggerFactory.getLogger(getClass)

  protected def fetchSeries(
      client: HttpClient
  )(apiKey: String, baseUrl: Uri, bypass: Boolean): EitherT[IO, Throwable, Set[Item]] =
    for {
      shows <- getToArr[List[SonarrSeries]](client)(baseUrl, apiKey, "series")
      exclusions <-
        if (bypass) {
          EitherT.pure[IO, Throwable](List.empty[SonarrSeries])
        } else {
          getToArr[List[SonarrSeries]](client)(baseUrl, apiKey, "importlistexclusion")
        }
    } yield (shows.map(toItem) ++ exclusions.map(toItem)).toSet

  protected def addToSonarr(client: HttpClient)(config: SonarrConfiguration)(item: Item): IO[Unit] = {

    val addOptions = SonarrAddOptions(config.sonarrSeasonMonitoring)
    val show = SonarrPost(
      item.title,
      item.getTvdbId.getOrElse(0L),
      config.sonarrQualityProfileId,
      config.sonarrRootFolder,
      addOptions,
      config.sonarrLanguageProfileId,
      tags = config.sonarrTagIds.toList
    )

    val result = postToArr[Unit](client)(config.sonarrBaseUrl, config.sonarrApiKey, "series")(show.asJson)
      .fold(
        err => logger.debug(s"Received warning for sending ${item.title} to Sonarr: $err"),
        result => result
      )

    result.map { r =>
      logger.info(s"Sent ${item.title} to Sonarr")
      r
    }
  }

  protected def deleteFromSonarr(client: HttpClient, config: SonarrConfiguration, deleteFiles: Boolean)(
      item: Item
  ): EitherT[IO, Throwable, Unit] = {
    val showId = item.getSonarrId.getOrElse {
      logger.warn(s"Unable to extract Sonarr ID from show to delete: $item")
      0L
    }

    for {
      _ <- deleteToArr(client)(config.sonarrBaseUrl, config.sonarrApiKey, showId, deleteFiles)
      _ = logger.info(s"Deleted ${item.title} from Sonarr")
      // Clear any import list exclusion so re-adding to watchlist works
      _ <- clearSonarrExclusion(client)(config.sonarrBaseUrl, config.sonarrApiKey, item)
    } yield ()
  }

  private def clearSonarrExclusion(
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, item: Item): EitherT[IO, Throwable, Unit] = {
    val tvdbId = item.getTvdbId
    if (tvdbId.isEmpty) {
      EitherT.pure[IO, Throwable](())
    } else {
      (for {
        exclusions <- getToArr[List[SonarrSeries]](client)(baseUrl, apiKey, "importlistexclusion")
        matching = exclusions.filter(e => e.tvdbId.isDefined && tvdbId == e.tvdbId)
        _ <- matching.traverse_ { excl =>
          val url = baseUrl / "api" / "v3" / "importlistexclusion" / excl.id
          EitherT(client.httpRequest(Method.DELETE, url, Some(apiKey)))
            .recover { case _: MalformedMessageBodyFailure => Json.Null }
            .map { _ =>
              logger.info(s"Cleared Sonarr exclusion for ${item.title} (tvdbId: ${excl.tvdbId})")
            }
        }
      } yield ()).recover {
        case err =>
          logger.debug(s"Could not clear Sonarr exclusion for ${item.title}: $err")
      }
    }
  }

  private def deleteToArr(
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, id: Long, deleteFiles: Boolean): EitherT[IO, Throwable, Unit] = {
    val urlWithQueryParams = (baseUrl / "api" / "v3" / "series" / id)
      .withQueryParam("deleteFiles", deleteFiles)
      .withQueryParam("addImportListExclusion", false)

    EitherT(client.httpRequest(Method.DELETE, urlWithQueryParams, Some(apiKey)))
      .recover { case _: MalformedMessageBodyFailure => Json.Null }
      .map(_ => ())
  }

  private def getToArr[T: Decoder](
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, endpoint: String): EitherT[IO, Throwable, T] =
    for {
      response     <- EitherT(client.httpRequest(Method.GET, baseUrl / "api" / "v3" / endpoint, Some(apiKey)))
      maybeDecoded <- EitherT.pure[IO, Throwable](response.as[T])
      decoded <- EitherT.fromOption[IO](maybeDecoded.toOption, new Throwable("Unable to decode response from Sonarr"))
    } yield decoded

  private def postToArr[T: Decoder](
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, endpoint: String)(payload: Json): EitherT[IO, Throwable, T] =
    for {
      response <- EitherT(
        client.httpRequest(Method.POST, baseUrl / "api" / "v3" / endpoint, Some(apiKey), Some(payload))
      )
      maybeDecoded <- EitherT.pure[IO, Throwable](response.as[T])
      decoded <- EitherT.fromOption[IO](maybeDecoded.toOption, new Throwable("Unable to decode response from Sonarr"))
    } yield decoded
}
