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

  def calcCorrelBetwGrps(grp1: List[Int], grp2: List[Int], correlMatrix: List[List[Double]]): Double = {
    val lsAvgCorrelOfEach = grp1.map(s1 => {
      val lsCorrelWithG2 = grp2.map(correlMatrix(_)(s1))
      lsCorrelWithG2.sum / lsCorrelWithG2.length
    })
    lsAvgCorrelOfEach.sum / lsAvgCorrelOfEach.length
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
      lslsReturns = lslsReturns :+ SUtil.calcReturns(ls).map(_ - 1.0)
    })

    var correlMatrix = List[List[Double]]()
    for (i <- 0 until lslsReturns.length) {
      var lsCorrelMatrixRow = List[Double]()
      for (j <- 0 until lslsReturns.length) {
        val subSet_i = lslsReturns(i).takeRight(numOfDays)
        val subSet_j = lslsReturns(j).takeRight(numOfDays)

        val mean_i = subSet_i.sum / subSet_i.length
        val mean_j = subSet_j.sum / subSet_j.length

        val dist_from_mean_i = subSet_i.map(_ - mean_i)
        val dist_from_mean_j = subSet_j.map(_ - mean_j)

        val var_i = dist_from_mean_i.map(x => x * x).sum / dist_from_mean_i.length
        val var_j = dist_from_mean_j.map(x => x * x).sum / dist_from_mean_j.length

        val dist_tup = dist_from_mean_i.zip(dist_from_mean_j)

        val scale = 1000
        val correl = Math.round(dist_tup.map(x => x._1 * x._2).sum /
          dist_tup.length / Math.sqrt(var_i) / Math.sqrt(var_j) * scale).toDouble / scale

        if (printCorMat) print(correl)
        lsCorrelMatrixRow :+= correl
        if (printCorMat) print("\t")
      }
      if (printCorMat) println
      correlMatrix :+= lsCorrelMatrixRow
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

        val lsGrpCorAvg = groups.map(g => calcAvgGrpCorrel(x, g, correlMatrix))

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
    while (shouldContinue) {
      val belongsToGrp = (0 until numOfInstr).map(x => {
        groups.map(g => calcAvgGrpCorrel(x, g, correlMatrix)).zipWithIndex.sortBy(_._1).reverse.head._2
      })

      val groups_changed = belongsToGrp.zip(0 until numOfInstr).groupBy(_._1).map { case (grpNum, lsStk) => lsStk.map(_._2).toList }.toList
      if (convToStringRepre(groups) != convToStringRepre(groups_changed)) groups = groups_changed
      else shouldContinue = false
    }

    //--------------------------------------------------
    // print groups
    //--------------------------------------------------
    println("No of groups = " + groups.length)
    groups.zipWithIndex.foreach(x => {
      println("Group " + x._2.toString + ": " + x._1.map(mapIdxSym.get(_).get).mkString(","))
    })

    //--------------------------------------------------
    // correl matrix of groups
    //--------------------------------------------------
    (0 until groups.length).toList.foreach(g => {
      val lsCorWithGrp = (0 until groups.length).toList.map(i => calcCorrelBetwGrps(groups(g), groups(i), correlMatrix))
      println(lsCorWithGrp.map(j => Math.round(j*100.0)/100.0).mkString("\t"))
    })

  }
}
