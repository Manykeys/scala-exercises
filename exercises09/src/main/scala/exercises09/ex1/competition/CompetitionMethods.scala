package exercises09.ex1.competition

import cats.Monad
import cats.syntax.all._
import cats.instances.all._
import exercises09.ex1.service.TwitterService
import exercises09.ex1.service.domain.GetTweetResponse.{Found, NotFound}
import exercises09.ex1.service.domain.GetTweetsResponse
import exercises09.ex1.twitter.domain.{TweetId, TweetInfo, User}

class CompetitionMethods[F[_]: Monad](service: TwitterService[F]) {

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * найти в них твиты где есть лайки указанного юзера
    * удалить эти лайки вызвав unlike
    */
  def unlikeAll(user: User, tweetIds: List[TweetId]): F[Unit] =
    tweetIds.traverse(z => service.unlike(user, z)).void

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * выбрать среди них тот твит у которого больше всего лайков или он раньше создан, если лайков одинаковое количество
    */
  def topAuthor(tweetIds: List[TweetId]): F[Option[User]] = {
    service.getTweets(tweetIds).map {
      case GetTweetsResponse(_, tweets) if tweets.nonEmpty =>
        val topTweetOpt = tweets.toList.sortBy(t => (-t.likedBy.size, t.created.toEpochMilli)).headOption
        topTweetOpt.map(_.author)
      case _ => None
    }
  }
}
