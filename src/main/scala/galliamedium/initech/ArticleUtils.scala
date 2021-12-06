package galliamedium
package initech

import java.util.Calendar
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoField

// ===========================================================================
object ArticleUtils {
  
  val DefaultCalendar = Calendar.getInstance()

  // Java...
  def ageFrom(dob: String): Int =
    DefaultCalendar.get(Calendar.YEAR) -
    DateTimeFormatter.ISO_LOCAL_DATE.parse(dob).get(ChronoField.YEAR)

}    

// ===========================================================================
