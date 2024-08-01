package exercises.action.imperative

import exercises.action.imperative.UserCreationExercises.User

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object UserCreationServiceApp extends App {
  val console = Console.system
  val clock   = Clock.system
  val service = new UserCreationService(console, clock)

  service.readUser(maxAttempts = 3)
}

class UserCreationService(console: Console, clock: Clock) {

  import exercises.action.imperative.UserCreationService._

  def readName(): String = {
    console.writeLine("What's your name?")
    console.readLine()
  }

  def readDateOfBirth(): LocalDate = {
    console.writeLine("What's your date of birth? [dd-mm-yyyy]")
    val line = console.readLine()
    onError(
      action = LocalDate.parse(line, dateOfBirthFormatter),
      cleanup = exception => {
        console.writeLine("""Incorrect format, for example enter "18-03-2001" for 18th of March 2001""")
        throw exception
      }
    )
  }

  def readSubscribeToMailingList(): Boolean = {
    console.writeLine("Would you like to subscribe to our mailing list? [Y/N]")
    val answer = console.readLine()
    onError(
      action = parseYesNo(answer),
      cleanup = exception => {
        console.writeLine("""Incorrect format, enter "Y" for Yes or "N" for "No"""")
        throw exception
      }
    )
  }

  def readUser(maxAttempts: Int): User = {
    val name                    = readName()
    val dateOfBirth             = retry(maxAttempts)(readDateOfBirth())
    val subscribedToMailingList = retry(maxAttempts)(readSubscribeToMailingList())
    val user                    = User(name, dateOfBirth, subscribedToMailingList, clock.now())

    console.writeLine(s"User is $user")
    user
  }

}

object UserCreationService {
  val dateOfBirthFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  def parseYesNo(answer: String): Boolean = {
    answer match {
      case "Y"   => true
      case "N"   => false
      case other => throw new IllegalArgumentException(s"""Expected "Y" or "N" but received $other""")
    }
  }

  def formatYesNo(yesNo: Boolean): String = {
    if (yesNo) "Y" else "N"
  }
}
