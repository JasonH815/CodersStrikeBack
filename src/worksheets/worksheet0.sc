import math._
import app.Simulation._


val checkPoint = Point(11222, 5407)
val position = Point(7718, 2198)
val target = Point(11222, 5407)

val a = nextAngle(35, position, checkPoint, target)
val s = nextVelocity(position, checkPoint, MathVector(0,0), a, 100)



1-2