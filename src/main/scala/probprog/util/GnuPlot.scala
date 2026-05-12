package probprog.util

import java.nio.file.Files
import sys.process._

object GnuPlot {
  val tempFilePrefix = "probprog_"
  val tempFileSuffix = ".txt"

  def histogram[T](bins: Iterable[(T, Double)]) {
    val tempFile = Files.createTempFile(tempFilePrefix, tempFileSuffix)

    val writer = Files.newBufferedWriter(tempFile)
    var yrangeMax = 0.0
    var yrangeMin = 0.0

    for(line <- bins) {
      writer.write(s"${line._1} ${line._2}\n")
      yrangeMax = yrangeMax.max(line._2)
      yrangeMin = yrangeMin.min(line._2)
    }

    writer.close()

    yrangeMax = yrangeMax + yrangeMax * 0.2

    val gnuplotExpression = s"""
    |set style data histogram;
    |set style fill solid;
    |set yrange [$yrangeMin:$yrangeMax];
    |plot '${tempFile}' using 2:xtic(1)
    """.stripMargin

    val gnuplotCommand = Seq("gnuplot", "-p", "-e", gnuplotExpression)

    gnuplotCommand.!
    Files.delete(tempFile)
  }

  def line[T](xStart: Double, xEnd: Double, values: Iterable[Double]) {
    val tempFile = Files.createTempFile(tempFilePrefix, tempFileSuffix)

    val writer = Files.newBufferedWriter(tempFile)
    var yrangeMax = 0.0
    var yrangeMin = 0.0

    for(v <- values) {
      writer.write(s"$v\n")
      yrangeMax = yrangeMax.max(v)
      yrangeMin = yrangeMin.min(v)
    }

    writer.close()

    yrangeMax = yrangeMax + yrangeMax * 0.2

    val gnuplotExpression = s"""
    |set style data histogram; <= FIX ME!!!
    |set style fill solid;
    |set yrange [$yrangeMin:$yrangeMax];
    |plot '${tempFile}' using 2:xtic(1)
    """.stripMargin

    val gnuplotCommand = Seq("gnuplot", "-p", "-e", gnuplotExpression)

    gnuplotCommand.!
    Files.delete(tempFile)
  }
}
