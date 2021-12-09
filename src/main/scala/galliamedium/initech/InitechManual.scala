package galliamedium.initech

import scala.util.chaining._

// ===========================================================================
object InitechManual { // manual counterpart

  def main(args: Array[String]): Unit = {
    apply(
        employees = Parsing.readEmployees(s"$InputDir/employees.tsv"),
        projects  = Parsing.readProjects (s"$InputDir/projects.tsv" ),
        issues    = Parsing.readIssues   (s"$InputDir/issues.tsv"   ))
      .mkString("\n")
      .tap(println) // putting aside to-json (not generally done manually)
  }

  // ---------------------------------------------------------------------------
  def apply(
        employees: Seq[Domain.Origin.Employee],
        projects : Seq[Domain.Origin.Project ],
        issues   : Seq[Domain.Origin.Issue   ])
      : Seq[Domain.Destination.Employee] = {

    val targetProjectIds: Seq[String] =
      projects
        .filter { project =>
          Seq("W2K", "W3K").contains(project.Name) }
        .map(_.ID)

    // ---------------------------------------------------------------------------
    val counts: Map[String /* employee ID */, Domain.Destination.Issues] =
      issues
        .filter { issue =>
          targetProjectIds.contains(issue.`Project ID`) }
        .groupBy(_.`Employee ID`)
        .mapValues { employeeIssues =>
          val statusLookup: Map[String /* status */, Int /* count */] =
            employeeIssues
              .groupBy  (_.Status)
              .mapValues(_.size)
              .toMap

          Domain.Destination.Issues(
            OPEN        = statusLookup.get("OPEN"),
            IN_PROGRESS = statusLookup.get("IN_PROGRESS"),
            RESOLVED    = statusLookup.get("RESOLVED")) }
        .toMap        

    // ---------------------------------------------------------------------------
    employees
      .map { employee =>
        Domain.Destination.Employee(
          name               = Domain.Destination.Name(
            given = s"${employee.`First Name`}, ${employee.`Middle Name`.getOrElse("N/A")}",
            last  = employee.`Last Name`),
          address            = Domain.Destination.Address(
            number = employee.`Address number`,
            street = employee.`Address street`,
            city   = employee.`Address city`  ,
            zip    = employee.`Address zip`   ),
          age                = ArticleUtils.ageFrom(employee.DOB),
          title              = employee.Title,
          tps_reports        = employee.`TPS reports`,
          satisfaction_level = employee.`Employee Satisfaction` match {
            case "Abysmal" =>  0
            case "Poor"    =>  3
            case "Greaaat" => 10 },
          is_manager         = employee.Title == "Manager",
          to_fire            = employee.`Good for the company?` match {
            case "Y" => false
            case "N" => true },
          issues             = counts.get(employee.`Employee ID`),
          comment            = employee.Comment) }
  }

}

// ===========================================================================
object Domain {
  
  object Origin {
  
    case class Employee(
      `Employee ID`          : String,
      `First Name`           : String,
      `Middle Name`          : Option[String],
      `Last Name`            : String,
      `DOB`                  : String,
      `Address number`       : Int,
      `Address street`       : String,
      `Address city`         : String,
      `Address zip`          : Int,
      `Title`                : String,
      `TPS reports`          : Int,
      `Employee Satisfaction`: String,
      `Good for the company?`: String,
      `Comment`              : String)
  
    // ---------------------------------------------------------------------------
    case class Issue(
      `Issue ID`       : String,
      `Project ID`     : String,
      `Priority`       : String,
      `Employee ID`    : String,
      `Creation date`  : String,
      `Start date`     : String,
      `Resolution date`: String,
      `Status`         : String)
  
    // ---------------------------------------------------------------------------
    case class Project(
      ID         : String,
      Name       : String,
      Description: String)
  }
  
  // ===========================================================================
  object Destination {
  
    case class Employee(
          name              : Name,
          address           : Address,
          age               : Int,
          title             : String,
          tps_reports       : Int,
          satisfaction_level: Int,
          is_manager        : Boolean,
          to_fire           : Boolean,
          issues            : Option[Issues],
          comment           : String)
  
        // ---------------------------------------------------------------------------
        case class Name(
          given: String,
          last : String)
  
        // ---------------------------------------------------------------------------
        case class Address(
          number: Int,
          street: String,
          city  : String,
          zip   : Int)
  
        // ---------------------------------------------------------------------------
        case class Issues(
          OPEN       : Option[Int],
          IN_PROGRESS: Option[Int],
          RESOLVED   : Option[Int])
  
  }
}

// ===========================================================================
/** this could be delegated to a XSV to case class library, although it's not clear that
  *   there's one obvious choice on the matter within the Scala ecosystem. */
object Parsing { import Domain.Origin._

  def readEmployees(path: String): Seq[Employee] = readTsv(path) { iter =>
    Employee(
      `Employee ID`          = iter.next(),
      `First Name`           = iter.next(),
      `Middle Name`          = { val tmp = iter.next(); if (tmp.isEmpty) None else Some(tmp) },
      `Last Name`            = iter.next(),
      `DOB`                  = iter.next(),
      `Address number`       = iter.next().toInt,
      `Address street`       = iter.next(),
      `Address city`         = iter.next(),
      `Address zip`          = iter.next().toInt,
      `Title`                = iter.next(),
      `TPS reports`          = iter.next().toInt,
      `Employee Satisfaction`= iter.next(),
      `Good for the company?`= iter.next(),
      `Comment`              = iter.next()) }

  // ---------------------------------------------------------------------------
  def readIssues(path: String): Seq[Issue] = readTsv(path) { iter =>
    Issue(
      `Issue ID`       = iter.next(),
      `Project ID`     = iter.next(),
      `Priority`       = iter.next(),
      `Employee ID`    = iter.next(),
      `Creation date`  = iter.next(),
      `Start date`     = iter.next(),
      `Resolution date`= iter.next(),
      `Status`         = iter.next()) }

  // ---------------------------------------------------------------------------
  def readProjects(path: String): Seq[Project] = readTsv(path) { iter =>
    Project(
      ID         = iter.next(),
      Name       = iter.next(),
      Description= iter.next()) }

  // ===========================================================================
  private def readTsv[T](path: String)(populate: Iterator[String] => T): Seq[T] = {
    val src   = scala.io.Source.fromFile(path)
    val lines =
      src
        .getLines()
        .drop(1) // drop header
        .map { _.split("\t", -1).toIterator.pipe(populate) } // CSV would be trickier
        .toList
    src.close()

    lines
  }

}

// ===========================================================================
