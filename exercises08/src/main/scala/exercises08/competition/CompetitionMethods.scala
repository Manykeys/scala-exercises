package exercises08.competition

import cats.Monad
import cats.implicits.{toFlatMapOps, toFoldableOps, toFunctorOps}
import cats.instances.list._
import exercises08.service.TwitterService
import exercises08.twitter.domain._

import scala.concurrent.ExecutionContext

class CompetitionMethods[F[_]: Monad](service: TwitterService[F])(implicit ec: ExecutionContext) {

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * найти в них твиты, где есть лайки указанного юзера
    * удалить эти лайки, вызвав unlike
    */
  def unlikeAll(user: User, tweetIds: List[TweetId]): F[Unit] = {
    for {
      tweets <- service.getTweets(tweetIds)
      liked = tweets.found.toList.filter(_.likedBy.contains(user))
      _ <- liked.traverse_(t => service.unlike(user, t.id))
    } yield ()
  }

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * выбрать среди них тот твит у которого больше всего лайков или он раньше создан, если лайков одинаковое количество
    */
  def topAuthor(tweetIds: List[TweetId]): F[Option[User]] = {
    for {
      tweets <- service.getTweets(tweetIds)
      found = tweets.found.toList.sortBy(t => (-t.likedBy.size, t.created.toEpochMilli))
    } yield found.headOption.map(_.author)
  }
}
