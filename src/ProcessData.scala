import scala.collection.mutable.ListBuffer
import scala.io
import scala.util.Random

object ProcessData {

  def stringToFloat(row: ListBuffer[String]) : ListBuffer[Float] = {
    row.map(x => x.toFloat)
  }

  def main(args: Array[String]): Unit = {
    /*val bufferedSource = io.Source.fromFile("data/wheat-seeds.csv")
    var file = new ListBuffer[ListBuffer[String]]
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      file += ListBuffer(cols: _ *)
    }
    bufferedSource.close

    file.map(x => stringToFloat(x))

    println(file.head.head.getClass)*/

    val x = List.fill(3)(Random.nextFloat)
    println(x)
  }

}
