import scala.util.Random

val l = List.fill(10)((Random.nextInt(),
  Random.nextDouble()))

val (ints, double) = l.unzip

ints
double

val x = "Hello World"
val r1 = x.reverse

val r2 = "Hello World".reverse

val sb = new StringBuilder("Hello")

val rr1 = sb.append(", Hello").toString()
val rr2 = sb.append(", Hello").toString()





