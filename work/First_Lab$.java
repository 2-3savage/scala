object First_Lab {
  def main(args: Array[String]): Unit = {
    println("Введите X начальное:")
    val X_1 = rd()
    if X_1 >= -1 then
      println("X < -1")
      System.exit(1)
    println("Введите X конечное:")
    val X_2 = rd()
    if X_2 >= -1 then
      println("X < -1")
      System.exit(1)
    println("Введите dx - Шаг:")
    val DX = rd()
    val rdx = X_2 - X_1
    if (rdx > 0 && DX < 0) || (rdx < 0 && DX > 0) || DX == 0 then
      println("Неправильный шаг")
      System.exit(1)
    println("Введите e:")
    val E = rd()
    if E < 0 then
      println("E > 0")
      System.exit(1)

    val buf = rec(X_1, X_2, DX, E).toList
    println("x\t\tf(x)\t\tTaylor(x)\tTI (Taylor Iterations)")
    for (bufes <- buf)
      print(bufes)
  }
  def rec(x1: Double, x2: Double, dx: Double, e: Double) :  ListBuffer[String] =
    @tailrec
    def func_taylor(x: Double, acc: ListBuffer[String]) : ListBuffer[String]=
      if (x > x2 && dx > 0) || (dx < 0 && x < x2) then
        acc
      else
        val (taylor_iter, taylor_x) = func(x, e)
        acc += f"$x%.2f\t" // Сам x
        acc += f"${atan(x)}%.8f\t" // арктангенс
        acc += f"$taylor_x%.8f\t" // мой арктангенс
        acc += f"$taylor_iter\t" // итератор
        acc += "\n"
        func_taylor(x + dx, acc)
    func_taylor(x1, new ListBuffer())


  def func(x: Double, e: Double): (Int, Double) =
    @tailrec
    def f(n: Int, acc: Double): (Int, Double) =
      val dni = 2 * n + 1
      val inc = pow(-1, n + 1) / (dni * pow(x, dni))

      if abs(inc) < e then
        (n, acc)
      else
        f(n + 1, inc + acc)
    f(0, -Pi/2)
}
