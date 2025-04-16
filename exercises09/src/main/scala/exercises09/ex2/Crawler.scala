package exercises09.ex2

import cats.effect.{IO, Ref}
import cats.implicits._

/*
 * В данной работе необходимо написать web-crawler.
 *
 * Crawler - это программа, которая анализует структуру данного ей сайта.
 * Эта программа посещает все ссылки, которые ей удаётся найти на заданном ей ресурсе.
 *
 * Вам необходимо реализовать функцию `crawl`.
 *
 * Эта функция будет вызвана с некоторым урлом. Например: "http://wikipedia.org"
 * Задача этой функции:
 *   1. Посетить переданный ей урл.
 *   2. Обработать ответ формата html - получив новые урлы для посещения.
 *   3. Посетить полученные из ответа урлы, в случае если они принадлежат тому же домену (поддомены не считать, только строгое совпадение).
 *   4. Для каждого урла, из шага 3, повторять все с 1 шага до тех пор, пока Crawler не посетит все страницы
 *   5. Вернуть набор посещённых страниц
 *
 * Таким образом, программа должна посетить каждую страницу сайта, до которой сможет добраться через html ответы.
 *
 * При этом, должны соблюдаться условия:
 *   - Каждая страница должна быть посещена строго 1 раз.
 *   - Ответ HttpClient может вернуть ошибку (завершение IO с Exception). В этом случае, необходимо попытаться
 *     повторить запрос, но не более 3 раз. Если после 3 запросов всё ещё возвращается ошибка,
 *     урл необходимо пропустить (урла не должно быть в результате функции).
 *   - Нельзя покидать сайт. Скажем если на сайте "http://wikipedia.org" вы вдруг найдёт ссылку на сторонний ресурс,
 *     её необходимо пропустить (её нельзя вызывать, урла не должно быть в результате функции).
 *
 * Прочие условности:
 *   - Пример html ответа
 *       <html><body>
 *         <div>some hmtl here</div>
 *         <div>quite <p style="color: red">natural</p></div>
 *         <div><a href="/page1">first page</a></div>
 *       </body></html>
 *     Таким образом, следует считать что html ответ приближен к реальности.
 *   - Искомые ссылки находятся в атрибуте `href`
 *   - На странице может быть 0+ ссылок. Не обязательно одна.
 *   - Поиск по html можете производить любым удобным способом.
 *     Например, можете использовать UrlSearch.search
 *   - Учитывайте, что URL адреса могут быть как абсолютными так и относительными.
 *   - В идеале, обработку ссылок выполнять конкурентно, использовав параллельные комбинаторы.
 *   - Обычно, в реальном мире, перед тем как совершить повторный запрос, вы бы выжидали какое-то время.
 *     Возможно даже это время увеличивалось бы с каждый запросом согласно какой-то функции.
 *     Однако в данной работе, не нужно выжидать и делать паузы. Именно по этой причине вам не предоставлен Timer.
 *     Получив ошибку, необходимо сразу же повторить запрос.
 *   - Можете определять доменную модель, как считаете нужным.
 */
class Crawler(client: HttpClient[IO]) {

  private val visitedRef: IO[Ref[IO, Set[HttpClient.URL]]] = Ref.of[IO, Set[HttpClient.URL]](Set.empty)

  def crawl(
      root: HttpClient.URL,
      parent: Option[HttpClient.URL] = None
  ): IO[Set[HttpClient.URL]] =
    for {
      ref    <- visitedRef
      result <- crawlInternal(root, ref, parent)
    } yield result

  private def crawlInternal(
      url: HttpClient.URL,
      visitedRef: Ref[IO, Set[HttpClient.URL]],
      parent: Option[HttpClient.URL]
  ): IO[Set[HttpClient.URL]] =
    for {
      alreadyVisited <- markVisited(url, visitedRef)
      result <- if (alreadyVisited) skip
      else fetchAndProcess(url, visitedRef, parent)
    } yield result

  private def markVisited(
      url: HttpClient.URL,
      visitedRef: Ref[IO, Set[HttpClient.URL]]
  ): IO[Boolean] =
    visitedRef.modify { current =>
      if (current.contains(url)) (current, true)
      else (current + url, false)
    }

  private def fetchAndProcess(
      url: HttpClient.URL,
      visitedRef: Ref[IO, Set[HttpClient.URL]],
      parent: Option[HttpClient.URL]
  ): IO[Set[HttpClient.URL]] = {
    val attempted = retry(client.get(url), 2)
    attempted.attempt.flatMap {
      case Left(_)     => skip
      case Right(body) => processBody(url, body, parent, visitedRef)
    }
  }

  private def processBody(
      url: HttpClient.URL,
      body: HttpClient.HttpResponse,
      parent: Option[HttpClient.URL],
      visitedRef: Ref[IO, Set[HttpClient.URL]]
  ): IO[Set[HttpClient.URL]] =
    for {
      links    <- IO.delay(UrlSearch.search(parent.getOrElse(url), url, body))
      children <- links.toList.traverse(link => crawlInternal(link, visitedRef, Some(url)))
    } yield children.flatten.toSet + url

  private def retry[A](
      action: IO[A],
      retries: Int
  ): IO[A] =
    action.handleErrorWith {
      case _ if retries > 0 => retry(action, retries - 1)
      case err              => IO.raiseError(err)
    }

  private def skip: IO[Set[HttpClient.URL]] =
    IO.pure(Set.empty)
}
