package app

import math._
import scala.util._

/**
  * This code automatically collects game data in an infinite loop.
  * It uses the standard input to place data into the game variables such as x and y.
  * YOU DO NOT NEED TO MODIFY THE INITIALIZATION OF THE GAME VARIABLES.
  **/
object PlayerOld extends App {


  var playerParams = new PlayerParams()
  var opponentParams = new OpponentParams()

  object EnvironmentParams {
    var maxDistance = 0
    var checkpointList:Seq[Point] = Vector.empty[Point]
    var discoveredCheckPoints = false
    var checkPointAngles = Map.empty[Point, Int]

    def calculateCheckpointAngles(cpList:Seq[Point]):Map[Point,Int] = {
      if (cpList.size < 3)
        return EnvironmentParams.checkPointAngles

      // middle elements
      var angleResult = EnvironmentParams.checkPointAngles
      for (i <- 1 until cpList.length - 1) {
        angleResult = angleResult.updated(cpList(i), calculateAngle(cpList(i-1), cpList(i), cpList(i+1)))
      }
      //first
      angleResult = angleResult.updated(cpList.head, calculateAngle(cpList.last, cpList.head, cpList.tail.head))
      // last
      angleResult = angleResult.updated(cpList.last, calculateAngle(cpList.dropRight(1).last, cpList.last, cpList.head))

      angleResult
    }
  }

  case class Point(x:Int, y:Int)
  class EnvironmentParams(maxDistance:Int, checkPointList:Seq[Point], checkpointMap:Map[Point,Int], discoveredCheckpoints:Boolean)

  class PlayerParams(
    val position:Point = Point(-1, -1),
    val previousPosition:Point = Point(-1,-1),
    val nextPosition:Point = Point(-1, -1),
    val checkPoint:Point = Point(-1, -1),
    val dist:Int = -1,
    val angle:Int = -1,
    val previousAngle:Int = -1,
    val speed:Int = -1,
    val angleOfAttack:Int = -1,
    var hasBoost:Boolean = true
  ) {

    // parse player input for the turn and update state
    def updated(input:String):PlayerParams = {
      (input split " ") map(_.toInt) match {
        case Array(x,y,checkpointX,checkpointY,checkpointDist,checkpointAngle) =>

          // update checkpoint list
          val cpList = EnvironmentParams.checkpointList
          if(cpList.isEmpty || (cpList.last.x != checkpointX && cpList.last.y != checkpointY)) {
            if(cpList.isEmpty || !EnvironmentParams.discoveredCheckPoints && cpList.head.x != checkpointX && cpList.head.y != checkpointY)
              EnvironmentParams.checkpointList = cpList :+ Point(checkpointX, checkpointY)
            else if (!EnvironmentParams.discoveredCheckPoints && checkpointX == cpList.head.x && checkpointY == cpList.head.y) {
              EnvironmentParams.discoveredCheckPoints = true
              EnvironmentParams.checkPointAngles = EnvironmentParams.calculateCheckpointAngles(cpList)
            }
          }
          // check max distance seen
          if (checkpointDist > EnvironmentParams.maxDistance) EnvironmentParams.maxDistance = checkpointDist

          // calculate speed
          val currentPosition = Point(x, y)
          val speed = if (this.position.x != -1 && this.position.y != -1) calculateDistance(this.position, currentPosition) else 0

          //calculate angle of attack
          val angleOfAttack = calculateAngle(checkPoint, Point(x, y), calculateNextPosition(position, Point(x, y)))

          new PlayerParams(Point(x, y), this.position, calculateNextPosition(this.position, Point(x, y)),
            Point(checkpointX, checkpointY), checkpointDist, checkpointAngle, this.angle, speed, angleOfAttack, this.hasBoost)

      }
    }

    def useBoost(): Boolean = {
      hasBoost = false
      this.hasBoost
    }

    def setTarget(opponentParams: OpponentParams):Point = {
      val distanceOfOpponent = calculateDistance(position, opponentParams.position)
      val previousDistanceOfOpponent = calculateDistance(previousPosition, opponentParams.previousPosition)
      val nextDistanceToOpponent = calculateDistance(nextPosition, opponentParams.nextPosition)
      val angleOfOpponent = calculateAngle(checkPoint, position, opponentParams.position)

      // attempt to ram opponent
      if ((speed > opponentParams.speed * 1.33 || opponentParams.speed > speed * 1.33) &&
      angleOfOpponent < 30 &&
      distanceOfOpponent < previousDistanceOfOpponent &&
      nextDistanceToOpponent < distanceOfOpponent)
        return opponentParams.nextPosition

      checkPoint
    }


    // choose thrust level for current turn
    def setThrust(opponentParams: OpponentParams, target:Point):String = {

      val distanceOfOpponent = calculateDistance(position, opponentParams.position)
      val previousDistanceOfOpponent = calculateDistance(previousPosition, opponentParams.previousPosition)
      val nextDistanceToOpponent = calculateDistance(nextPosition, opponentParams.nextPosition)

      // full speed if targeting opponent
      if (target != checkPoint)
        return if (distanceOfOpponent < 1000 && distanceOfOpponent < previousDistanceOfOpponent) "SHIELD" else "100"


      // maximize speed on oblique angles
      val nextCheckpoint = checkPoint
      EnvironmentParams.checkPointAngles.get(nextCheckpoint).foreach { cpAngle =>
        if (cpAngle > 90 && angle < 10 && angle > -10 && calculateDistance(position, nextPosition) < dist)
          if (hasBoost) {
            useBoost()
            return "BOOST"
          } else return "100"
      }


      // use shield if we are about to crash


      if (speed > 0 && nextDistanceToOpponent < 1200 && nextDistanceToOpponent * 1.20 < distanceOfOpponent) return "SHIELD"
      if (speed > 0 && nextDistanceToOpponent < 850 && nextDistanceToOpponent * 1.10 < distanceOfOpponent) return "SHIELD"
      if (speed > 0 && nextDistanceToOpponent < 700 && nextDistanceToOpponent < distanceOfOpponent) return "SHIELD"

//      if (speed > 0 && distanceOfOpponent < 2000 && distanceOfOpponent * 1.30 < previousDistanceOfOpponent) return "SHIELD"
//      if (speed > 0 && distanceOfOpponent < 1300 && distanceOfOpponent * 1.20 < previousDistanceOfOpponent) return "SHIELD"
//      if (speed > 0 && distanceOfOpponent < 900 && distanceOfOpponent * 1.10 < previousDistanceOfOpponent) return "SHIELD"
//      if (speed > 0 && distanceOfOpponent < 800 & distanceOfOpponent < previousDistanceOfOpponent) return "SHIELD"

      // early turn detection
      if (playerParams.previousAngle == 0 && playerParams.angle != 0)
        return "0"

      // set thrust to 100 * cos(angle) + 25, cap at 100
//      var thrust:Long = if (angle > 90 || angle < -90)
//        0
//      else
//        min((100 * abs(cos(angleOfAttack * Pi/180.0))).round + 15, 100)

      var thrust = min((100 * abs(cos(angleOfAttack * Pi/180.0))).round + 0, 100)

       //brake before entering corner
      if (playerParams.dist < 2000)
         thrust -= 25
      if (playerParams.dist < 1500)
        thrust -= 15
      if (playerParams.dist < 500)
        thrust -= 10

      // don't go below 25
      thrust = max(25, thrust)

      var thrustString = thrust.toString

      // use boost
      if (EnvironmentParams.discoveredCheckPoints &&
        angle < 10 &&
        angle > -10 &&
        angleOfAttack < 10 &&
        dist > EnvironmentParams.maxDistance * .75 &&
        hasBoost) { //use boost after first lap when close to the max distance and at 0 angle
        useBoost()
        thrustString = "BOOST"
      }

      thrustString
    }
  }

  class OpponentParams(
    val position:Point = Point(-1,-1),
    val previousPosition:Point = Point(-1, -1),
    val nextPosition:Point = Point(-1, -1),
    val speed:Int = -1
  ) {
    // parse opponent input for the turn
    def updated(input:String):OpponentParams = {
      (input split " ") map(_.toInt) match {
        case Array(x,y) =>

          // calculate speed
          val currentPosition = Point(x, y)
          val speed = if (this.position.x != -1 && this.position.y != -1) calculateDistance(this.position, currentPosition) else -1

          // update the angle to opponent
          new OpponentParams(Point(x, y), this.position, calculateNextPosition(this.position, Point(x, y)), speed)
      }
    }
  }

  def calculateDistance(p1:Point, p2:Point):Int = {
    sqrt(pow(p1.x  - p2.x, 2) + pow(p1.y - p2.y, 2)).round.toInt
  }

  // calculates the angle between p1-p2-p3 using cosine law
  def calculateAngle(p1:Point, p2:Point, p3:Point):Int = {
    val a = calculateDistance(p1, p2)
    val b = calculateDistance(p1, p3)
    val c = calculateDistance(p2, p3)
    (acos((pow(a,2) + pow(c, 2) - pow(b, 2))/(2*a*c))*180/Pi).round.toInt
  }

  def calculateNextPosition(previousPosition:Point, currentPosition:Point):Point = {
    Point(2 * currentPosition.x - previousPosition.x, 2 * currentPosition.y - previousPosition.y)
  }



  // game loop
  while(true) {

    // read input lines
    val playerLine = readLine
    val opponentLine = readLine

    playerParams = playerParams.updated(playerLine)
    opponentParams = opponentParams.updated(opponentLine)

    // input debug
    Console.err.println("Angle: " + playerParams.angle)
    Console.err.println("Angle of Attack: " + playerParams.angleOfAttack)


    Console.err.println(playerLine + " " + playerParams.speed + " " + playerParams.angleOfAttack + " " + playerParams.hasBoost)

    //Console.err.println("Opponent Line: " + opponentLine)
    //Console.err.println("Angles: " + EnvironmentParams.checkPointAngles)

    val target = playerParams.setTarget(opponentParams)
    val thrust = playerParams.setThrust(opponentParams, target)


    // Edit this line to output the target position
    // and thrust (0 <= thrust <= 100)
    // i.e.: "x y thrust"
    println(target.x + " " + target.y + " " + thrust)
  }
}