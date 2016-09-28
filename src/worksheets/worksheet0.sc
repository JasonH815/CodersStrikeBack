import math._
import app.Simulation._

val position = Point(2771, 2853)
val checkPoint = Point(11485, 6086)
val target = Point(2771, 8000)

val a = nextAngle(-69, position, checkPoint, target)
val s = calculateVelocity(position, checkPoint, MathVector(17,154), a, 100)
val ns = nextVelocity(s)
val p = nextPosition(position, s)



1-2