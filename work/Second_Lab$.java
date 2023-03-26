object Second_Lab {
  
  def main(args: String*): Unit = {
    val letter = args(1)
    val file = Source.fromFile(args(0))
    val words = file.getLines().flatMap(x => x.split("\\s+")).filter(_.takeRight(1) == letter).toList
    words.distinct.sorted.foreach(println)
  }
}
