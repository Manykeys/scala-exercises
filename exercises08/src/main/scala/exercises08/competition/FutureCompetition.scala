package exercises08.competition

import exercises08.competition.domain.ScenarioError.TopAuthorNotFound
import exercises08.service.{TwitterService, TwitterServiceFuture}
import exercises08.twitter.domain._
import exercises08.twitter.{LocalTwitterApi, TwitterApi}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Конкурс! Кто наберет больше лайков под своим постом - тот победил
  *
  * Каждый пользовать постит твит "${user.id} will win!", и его фолловеры его лайкают
  * юзеры постят твиты параллельно, и так же параллельно их лайкают фолловеры
  *
  * Но случилась беда: пользователь с именем bot нарушил правила конкурса, и все его лайки надо удалить
  *
  * В конце надо вывести победителя
  * Если победителей несколько, то того, у которого твит был раньше
  * Если победителей нет, то вернуть ошибку TopAuthorNotFound
  *
  * используйте методы
  * CompetitionMethods.unlikeAll
  * CompetitionMethods.topAuthor
  */
class FutureCompetition(service: TwitterService[Future], methods: CompetitionMethods[Future])(
    implicit ec: ExecutionContext
) extends Competition[Future] {
  def winner(
      users: List[User],
      followers: Map[User, List[User]],
      botUser: User
  ): Future[User] = {
    for {
      tweetUserPairs <- Future.traverse(users) { user =>
        service.tweet(user, s"${user.id} will win!").map(tweetId => (user, tweetId))
      }

      (_, userTweets) = tweetUserPairs.partition(_._1 == botUser)
      tweetIds        = userTweets.map(_._2)
      allTweetIds     = tweetUserPairs.map(_._2)

      _ <- Future.sequence {
        userTweets.flatMap {
          case (user, tweetId) =>
            followers.getOrElse(user, Nil).map(follower => service.like(follower, tweetId))
        }
      }

      _ <- methods.unlikeAll(botUser, allTweetIds)

      maybeWinner <- methods.topAuthor(tweetIds)
      winner <- maybeWinner match {
        case Some(user) => Future.apply(user)
        case None       => Future.failed(TopAuthorNotFound)
      }
    } yield winner
  }
}

object FutureCompetitionStart extends App {
  import scala.concurrent.Await
  import scala.concurrent.duration.DurationInt
  import scala.util.Random

  implicit val ec: ExecutionContext = ExecutionContext.global

  val api: TwitterApi = new LocalTwitterApi(Iterator.continually((Random.nextDouble() * 1000).toInt))

  val service: TwitterService[Future] = new TwitterServiceFuture(api)

  val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

  val oleg: User   = User("oleg")
  val ivan: User   = User("ivan")
  val marya: User  = User("marya")
  val gustav: User = User("gustav")
  val bot: User    = User("bot")

  val users: List[User] = List(oleg, ivan, marya, gustav, bot)

  val followers: Map[User, List[User]] = Map(
    oleg   -> List(ivan, bot),
    ivan   -> List(oleg, gustav),
    marya  -> List(oleg, ivan, gustav, bot),
    gustav -> List(oleg, ivan, marya),
    bot    -> List(bot)
  )

  private val winner: User =
    Await.result(new FutureCompetition(service, methods).winner(users, followers, bot), 30.seconds)
  println(s"${winner.id} win!!!")
}
