package app

/**
  * Created by Jason on 9/22/2016.
  */
object Simulation {

  case class Point(x:Int, y:Int)

  val checkpoints = Vector(Point(500, 500))

  var currentCheckpoint:Map[Player, Point]


  case class Player(position:Point, velocity:Point, angle:Int)


  var player = Player(Point(200, 200), Point(0,0), 0)


  //assign player starting position and checkpoint
  def init():String = {
      //set up checkpoint list

      //set up player
      player = Player(Point(200, 200), Point(0,0), 0)

      //assign player to first checkpoint

      player.position.x + " " + player.position.y +
      " " + currentCheckpoint(player).x + " " + currentCheckpoint(player).y +
      -1 + " " + 180
  }


  def updated(input:String): String = {



  }

}
