package jorander

package object functionalvalidation {

  def validation(validations: () => Option[String]*): Option[List[String]] =
    validations.map(_() match {
      case None => validationOK
      case Some(errorMsg) => validationErrors(List(errorMsg))
    }).reduce(
      (_, _) match {
        case (None, None) => validationOK
        case (Some(errorList), None) => validationErrors(errorList)
        case (None, Some(errorList)) => validationErrors(errorList)
        case (Some(errorList1), Some(errorList2)) => validationErrors(errorList1 ::: errorList2)
      })

  def validationError(msg: String) = Some(msg)
  def validationErrors(messages: List[String]) = Some(messages)
  def validationOK() = None

  def mustNotBeNull(input: Any, attributeName: String) =
    if (input != null) validationOK else validationError(attributeName + " must not be null.")
}