package exercises08.service

import exercises08.service.domain.{GetTweetResponse, GetTweetsResponse}
import exercises08.twitter.TwitterApi
import exercises08.twitter.domain.{TweetId, User}

import scala.concurrent.{ExecutionContext, Future, Promise}

/*
 * Future обертка над колбечным апи (TwitterAPI).
 */
class TwitterServiceFuture(api: TwitterApi)(implicit ec: ExecutionContext) extends TwitterService[Future] {
  def tweet(user: User, text: String): Future[TweetId] = ???

  def like(user: User, tweetId: TweetId): Future[Unit] = ???

  def unlike(user: User, tweetId: TweetId): Future[Unit] = ???

  def getTweet(tweetId: TweetId): Future[GetTweetResponse] = ???

  def getTweets(ids: List[TweetId]): Future[GetTweetsResponse] = ???
}
