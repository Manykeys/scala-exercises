package exercises08.service

import exercises08.service.domain.GetTweetResponse.{Found, NotFound}
import exercises08.service.domain.{GetTweetResponse, GetTweetsResponse}
import exercises08.twitter.TwitterApi
import exercises08.twitter.domain.{TweetId, TwitterError, User}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/*
 * Future обертка над колбечным апи (TwitterAPI).
 */
class TwitterServiceFuture(api: TwitterApi)(implicit ec: ExecutionContext) extends TwitterService[Future] {
  private def toFuture[A](f: (Try[A] => Unit) => Unit): Future[A] = {
    val p = Promise[A]()
    Future {
      f(p.complete)
    }
    p.future
  }

  //  override def tweet(user: User, text: String): Future[TweetId] = {
  //    Future {
  //      var tweetId: TweetId = null
  //      api.tweet(user, text) { res =>
  //        tweetId = res.get
  //      }
  //      tweetId
  //    }
  //  }

  def tweet(user: User, text: String): Future[TweetId] = {
    toFuture(api.tweet(user, text))
  }

  def like(user: User, tweetId: TweetId): Future[Unit] =
    toFuture[Unit](cb => api.like(user, tweetId)(cb)).recover {
      case TwitterError.LikeAlreadyExistError => ()
    }

  def unlike(user: User, tweetId: TweetId): Future[Unit] =
    toFuture[Unit](cb => api.unlike(user, tweetId)(cb)).recover {
      case TwitterError.LikeNotExistError => ()
    }

  def getTweet(tweetId: TweetId): Future[GetTweetResponse] =
    toFuture { cb =>
      api.get(tweetId) {
        case Success(tweet) => cb(Success(Found(tweet)))
        case Failure(_)     => cb(Success(NotFound(tweetId)))
      }
    }

  def getTweets(ids: List[TweetId]): Future[GetTweetsResponse] = {
    val futures = ids.map { id =>
      getTweet(id).map {
        case Found(tweet) => id -> Some(tweet)
        case NotFound(_)  => id -> None
      }
    }

    Future.sequence(futures).map { results =>
      val (found, notFound) = results.partitionMap {
        case (_, Some(tweet)) => Left(tweet)
        case (id, None)       => Right(id)
      }
      GetTweetsResponse(notFound.toSet, found.toSet)
    }
  }
}
