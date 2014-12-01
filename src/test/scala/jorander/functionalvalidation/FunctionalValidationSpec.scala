package jorander.functionalvalidation

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
class FunctionalValidationSpec extends FlatSpec with Matchers {
  val EXPECTED_ERROR_MSG_1 = "Expected error message1"
  val EXPECTED_ERROR_MSG_2 = "Expected error message2"
  "Function validation" should "run the two supplied validators functions and return the two error message strings if both yealds errors." in {
    validation(() => validationError(EXPECTED_ERROR_MSG_1),
      () => validationError(EXPECTED_ERROR_MSG_2)) should be === Some(List(EXPECTED_ERROR_MSG_1, EXPECTED_ERROR_MSG_2))
  }

  it should "run the two supplied validators functions and return the only error message string if only one yealds error." in {
    validation(() => validationError(EXPECTED_ERROR_MSG_1),
      () => validationOK) should be === Some(List(EXPECTED_ERROR_MSG_1))
  }

  it should "run the two supplied validators functions and return no error message if none yealds error." in {
    validation(() => validationOK, () => validationOK) should be === None
  }

  it should "be able to run a single supplied validator and return the error message if any." in {
    validation(() => validationError(EXPECTED_ERROR_MSG_1)) should be === Some(List(EXPECTED_ERROR_MSG_1))
  }

  it should "be able to run any number of supplied validators and return the error messages if any." in {
    validation(() => validationOK, () => validationOK, 
        () => validationError(EXPECTED_ERROR_MSG_1), 
        () => validationError(EXPECTED_ERROR_MSG_2))should be === Some(List(EXPECTED_ERROR_MSG_1, EXPECTED_ERROR_MSG_2))
  }

  "Function mustNotBeNull" should "test for null and if so return an error message." in {
    mustNotBeNull(null, "TestVal") should be === (Some("TestVal must not be null."))
  }
}