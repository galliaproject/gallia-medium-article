package galliamedium.initech

import gallia._ // assumes: libraryDependencies += "io.github.galliaproject" %% "gallia-core" % "0.3.1"

// ===========================================================================
object Initech {  

  def main(args: Array[String]): Unit = {
    Initech(
        // these will infer the schemas for convenience 
        projects  = s"$InputDir/projects.tsv" .stream(),
        employees = s"$InputDir/employees.tsv".stream(),
        issues    = s"$InputDir/issues.tsv"   .stream())
      .printJsonl() // or eg .writeFile("/tmp/initech.jsonl.gz")       
  }

  // ===========================================================================
  def apply(
        projects : HeadS,
        employees: HeadS,
        issues   : HeadS)
      : HeadS = {

    val revampedEmployees: HeadS =
      employees
        .rename(
            "First Name"  ~> "first" ,
            "Middle Name" ~> "middle",
            "Last Name"   ~> "last"  )
        .nest("first", "middle", "last").under("name")
        .renest { _.filterKeys(_.startsWith("Address")) }.usingSeparator(" ")
        .convert("Good for the company?").toBoolean(trueValue = "Y", falseValue = "N")
        .flip   ("Good for the company?" ~> "to_fire")
        .generate("is_manager").from(_.string("Title")).using(_ == "Manager")
        .transform(_.string("DOB" ~> "age")).using { dob => ArticleUtils.ageFrom(dob) }
        .transform(_.string("Employee Satisfaction" ~> "satisfaction_level")).using {
            case "Abysmal" =>  0
            case "Poor"    =>  3
            case "Greaaat" => 10 }
        .fuse( // just to show nested manipulation
            _.string ("name" |> "first"),
            _.string_("name" |> "middle"))
          .as("name" |> "given")
            .using { (first, middle) => s"${first}, ${middle.getOrElse("N/A")}" }

    // ---------------------------------------------------------------------------
    val counts: HeadS =
      issues
        .bring(projects, target = "Name", via = "Project ID" <~> "ID")
        .convert ("Name").toRequired // we know will always be the case
        .filterBy("Name").in(Seq("W2K", "W3K"))
        .remove  ("Name")
        .groupBy("Employee ID").as("issues")
        .transformObjects("issues").using {
          _ .countBy("Status") // defaults to "_count"        
            .pivot(_count).column("Status")
              .asNewKeys("OPEN", "IN_PROGRESS", "RESOLVED") }

    // ---------------------------------------------------------------------------
    revampedEmployees
      .bring(counts, target = "issues") // "via" optional here (same key)
      .remove("Employee ID")
      .rename(_.toLowerCase.replace(" ", "_"))
  }

}

// ===========================================================================
