package exercises09.ex1.service

import cats.effect.IO
import cats.syntax.all._
import exercises09.ex1.service.domain.GetTweetResponse.{Found, NotFound}
import exercises09.ex1.service.domain.{GetTweetResponse, GetTweetsResponse}
import exercises09.ex1.twitter.TwitterApi
import exercises09.ex1.twitter.domain._

import scala.util.{Failure, Success, Try}

// Воспользуйтесь синтаксисом map, recover, traverse из cats.syntax.all_
class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {

  def tweet(user: User, text: String): IO[TweetId] =
    IO.async_[TweetId] { cb =>
        api.tweet(user, text) {
          case Success(tweetId) => cb(Right(tweetId))
        }
      }
      .recover { case _ => TweetId.generate() }

  def like(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_[Unit] { cb =>
        api.like(user, tweetId) {
          case Success(()) => cb(Right(()))
        }
      }
      .recover {
        case TwitterError.LikeAlreadyExistError => IO.pure()
        case err                                => IO.raiseError(err)
      }

  def unlike(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_[Unit] { cb =>
        api.unlike(user, tweetId) {
          case Success(()) => cb(Right(()))
        }
      }
      .recover {
        case TwitterError.LikeNotExistError => IO.pure()
        case err                            => IO.raiseError(err)
      }

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] =
    IO.async_[Try[TweetInfo]] { cb =>
        api.get(tweetId)(result => cb(Right(result)))
      }
      .map {
        case Success(tweetInfo) => Found(tweetInfo)
      }
      .recover { case _ => NotFound(tweetId) }

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] =
    ids.traverse(getTweet).map { responses =>
      val foundTweets = responses.collect { case Found(tweetInfo) => tweetInfo }.toSet
      GetTweetsResponse(ids.toSet.diff(foundTweets.map(x => x.id)), foundTweets)
    }
}
