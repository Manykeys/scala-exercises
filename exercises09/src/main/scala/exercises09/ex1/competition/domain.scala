package exercises09.ex1.competition

object domain {
  sealed trait ScenarioError extends Throwable
  object ScenarioError {
    case object TopAuthorNotFound extends ScenarioError
  }
}
