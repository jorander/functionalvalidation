package jorander

package object functionalvalidation {
  private def concatErrors(fs: String*) = fs.toList
  def validate(validation1: () => Option[String], validation2: () => Option[String]): Option[List[String]] =
    (validation1(), validation2()) match {
      case (None, None) => validationOK
      case (Some(s), None) => validationErrors(concatErrors(s))
      case (None, Some(s)) => validationErrors(concatErrors(s))
      case (Some(s1), Some(s2)) => validationErrors(concatErrors(s1, s2))
    }
  def validationError(msg: String) = Some(msg)
  def validationErrors(messages: List[String]) = Some(messages)
  def validationOK() = None

  def mustNotBeNull(input: Any, attributeName: String) =
    if (input != null) validationOK else validationError(attributeName + " cannot be null.")
}