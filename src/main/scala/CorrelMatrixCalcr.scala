import org.nirvana._
import org.joda.time.format.DateTimeFormat
import org.joda.time.{Period, DateTime, Duration}

object CorrelMatrixCalcr {

  val dfmt = DateTimeFormat.forPattern("yyyy-MM-dd")

  def parseDate(s: String): DateTime = {
    dfmt.parseDateTime(s)
  }

  def calcAvgGrpCorrel(stk: Int, grp: List[Int], correlMatrix: List[List[Double]]): Double = {
    val grpcor = grp.filter(_ != stk).map(correlMatrix(stk)(_))
    grpcor.sum / grpcor.length
  }

  def convToStringRepre(grp: List[List[Int]]): String = {
    grp.map(_.sorted.mkString(",")).sorted.mkString("|")
  }

  def computeHash(grp: List[List[Int]]): BigInt = {
    grp.map(_.sum).fold(1)(_ * _)
  }

  def calcCorrelBetwGrps(grp1: List[Int], grp2: List[Int], correlMatrix: List[List[Double]]): Double = {
    val lsAvgCorrelOfEach = grp1.map(s1 => {
      val lsCorrelWithG2 = grp2.map(correlMatrix(_)(s1))
      lsCorrelWithG2.sum / lsCorrelWithG2.length
    })
    lsAvgCorrelOfEach.sum / lsAvgCorrelOfEach.length
  }

  def printCorrelMatrix(correlMatrix: List[List[Double]]) {
    correlMatrix.foreach(r => println(r.mkString("\t")))
  }

  def main(args: Array[String]) {

    if (args.length == 0) {
      println("USAGE: [csv file with data] [YYYY-MM-DD] [num of days] [correl threshold for grouping] [print correl matrix]")
      System.exit(0)
    }

    val uptoDate = parseDate(args(1))
    val numOfDays = args(2).toInt
    val correlThreshold = args(3).toDouble
    val printCorMat = (args(4) == "T" || args(4) == "t")
    val symFile = args(5)
    val bCalcMinVarcWei = (args(6) == "T" || args(6) == "t")

    val lines = scala.io.Source.fromFile(args(0)).getLines.toList
    val mapIdxSym = scala.io.Source.fromFile(symFile).getLines.toList.mkString(" ").split(" ").toList.zipWithIndex.map(x => (x._2, x._1)).toMap

    var lsDate = List[DateTime]()
    var lslsReturns = List[List[Double]]()

    val lslsStr = lines.map(_.split(",").toList)

    var lsFlattenedValues = List[Double]()
    lslsStr.foreach(x => {
      val barDate = parseDate(x(0))
      if (barDate.getMillis <= uptoDate.getMillis) {
        lsDate = lsDate :+ barDate
        lsFlattenedValues = lsFlattenedValues ::: x.tail.map(_.toDouble)
      }
    })

    val numOfInstr = lslsStr(0).length - 1

    (0 until numOfInstr).foreach(x => {
      val ls = lsFlattenedValues.zipWithIndex.collect {
        case (e, i) if ((i % numOfInstr) == x) => e
      }
      lslsReturns = lslsReturns :+ SUtil.calcReturns(ls).map(_ - 1.0).takeRight(numOfDays)
    })

    val correlMatrixStk = SUtil.calcCorrelMatrix(lslsReturns)

    if (printCorMat) {
      printCorrelMatrix(correlMatrixStk)
    }

    //--------------------------------------------------
    // grouping
    //--------------------------------------------------
    var groups = List[List[Int]]()
    //--------------------------------------------------
    // first pass
    //--------------------------------------------------
    (0 until numOfInstr).foreach(x => {

      if (groups.isEmpty) {
        groups :+= List(x)
      }
      else {

        val lsGrpCorAvg = groups.map(g => calcAvgGrpCorrel(x, g, correlMatrixStk))

        val friends = lsGrpCorAvg.zipWithIndex.sortBy(_._1).reverse.filter(_._1 > correlThreshold)
        if (friends.isEmpty) {
          groups :+= List(x)
        }
        else {
          val chosenGrp = friends.head._2
          val chosenGrpUpdated = groups(chosenGrp) :+ x
          groups = (groups.take(chosenGrp) :+ chosenGrpUpdated) ::: groups.takeRight(groups.length - chosenGrp - 1)
        }
      }

    })

    //--------------------------------------------------
    // second pass
    //--------------------------------------------------
    var shouldContinue = true
    var count = 0
    while (shouldContinue && count < 50) {

      val belongsToGrp = (0 until numOfInstr).map(x => {
        groups.map(g => calcAvgGrpCorrel(x, g, correlMatrixStk)).zipWithIndex.sortBy(_._1).reverse.head._2
      })
      val groups_changed = belongsToGrp.zip(0 until numOfInstr).groupBy(_._1).map { case (grpNum, lsStk) => lsStk.map(_._2).toList }.toList

      val lGroups = computeHash(groups)
      val lGroupsChgd = computeHash(groups_changed)
      if (lGroups == lGroupsChgd) {
        shouldContinue = false
      }
      else {
        groups = groups_changed
      }

      count += 1
    }

    //--------------------------------------------------
    // third pass
    //--------------------------------------------------
    count = 0
    while (shouldContinue && count < 50) {

      shouldContinue = false
      (0 until numOfInstr).toList.foreach(s => {
        val newGrp = groups.map(g => calcAvgGrpCorrel(s, g, correlMatrixStk)).zipWithIndex.sortBy(_._1).reverse.head._2
        if (!groups(newGrp).contains(s)) {
          //--------------------------------------------------
          // update groups immediately
          //--------------------------------------------------
          groups = groups.take(newGrp).map(_.filter(_ != s)) :::
            List(groups(newGrp) :+ s) :::
            groups.takeRight(groups.length - newGrp - 1).map(_.filter(_ != s))
          shouldContinue = true
        }
      })

      count += 1
    }
    //--------------------------------------------------

    println
    //--------------------------------------------------
    // print groups
    //--------------------------------------------------
    println("No of groups = " + groups.length)
    groups.zipWithIndex.foreach(x => {
      println("Group " + x._2.toString + ": " + x._1.map(mapIdxSym.get(_).get).mkString(","))
    })

    groups.zipWithIndex.foreach(x => {
      val symls = x._1.map(mapIdxSym.get(_).get)

      symls.foreach(s => {
        println(dfmt.print(uptoDate) + "_" + s + "," + x._2.toString)
      })
    })

    if (bCalcMinVarcWei) {

      //--------------------------------------------------
      // average out the individual stock return to become the group return
      // don't know how to implement yet
      // use the return series of the first sym
      //--------------------------------------------------
      val lslsGrpRtn = groups.map(g => lslsReturns(g.head))
      val covarMatrixGrp = SUtil.calcCovarMatrix(lslsGrpRtn)
      // printCorrelMatrix(covarMatrixGrp)

      val lsAvgPiecewiseCovar = covarMatrixGrp.map(x => x.sum / x.length)
      val avgOfAvgPiecewiseCovar = lsAvgPiecewiseCovar.sum / lsAvgPiecewiseCovar.length
      val stdevOfAvgPiecewiseCovar = SUtil.stdev(lsAvgPiecewiseCovar)

      val lsGaussianConversion = lsAvgPiecewiseCovar.map(x => 1.0 - SUtil.CNDF((x - avgOfAvgPiecewiseCovar) / stdevOfAvgPiecewiseCovar))

      val lsProportionalAvgCovarWeight = lsGaussianConversion.map(x => x / lsGaussianConversion.sum)
      val lsInverseVar = (0 until covarMatrixGrp.length).toList.map(x => 1.0 / covarMatrixGrp(x)(x))
      val lsInverseVarWeight = lsInverseVar.map(x => x / lsInverseVar.sum)

      val lsPdt = lsProportionalAvgCovarWeight.zip(lsInverseVarWeight).map(x => x._1 * x._2)
      val lsFinalWeight = lsPdt.map(x => x / lsPdt.sum)

      lsFinalWeight.zipWithIndex.foreach(x => {
        print("Group ")
        print(x._2)
        print(": ")
        print(x._1 * 100.0)
        println("%")
      })
    }

    // //--------------------------------------------------
    // // correl matrix of groups
    // //--------------------------------------------------
    // (0 until groups.length).toList.foreach(g => {
    //   val lsCorWithGrp = (0 until groups.length).toList.map(i => calcCorrelBetwGrps(groups(g), groups(i), correlMatrix))
    //   println(lsCorWithGrp.map(j => Math.round(j*100.0)/100.0).mkString("\t"))
    // })

  }
}
