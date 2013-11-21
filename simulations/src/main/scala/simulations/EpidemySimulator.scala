package simulations

import math.random

class EpidemySimulator extends Simulator {

  val myWorld = createWorld
  
  class State(val x:Int, val y:Int) {
    var infected = 0
    var dead = 0
    var visiblyInfetious = 0
    
    def livablePlace: Boolean = {
      dead == 0 && visiblyInfetious == 0
    }
  }
  
  class World {
    val state = new Array[Array[State]](SimConfig.roomRows)
    
    for(i<- 0 until SimConfig.roomRows) {
      state(i) = new Array[State](SimConfig.roomColumns)
      for(j<- 0 until SimConfig.roomColumns) {
        state(i)(j) = new State(i,j)
      }
    }
    
    def neighboors(x: Int, y: Int): List[State] = {
      List(0,1,2,3).map(i => {
        val nx = (x + SimConfig.dx(i) + SimConfig.roomRows) % SimConfig.roomRows
        val ny = (y + SimConfig.dy(i) + SimConfig.roomColumns) % SimConfig.roomColumns
        new State(nx, ny)})
    }
    
    def coolPlace(x: Int, y: Int) = {
      state(x)(y).livablePlace
    }
    
    def addSick(x: Int, y: Int) = {
      state(x)(y).visiblyInfetious += 1
    }
    
    def turnHealty(x: Int, y: Int) = {
      state(x)(y).visiblyInfetious -= 1
    }
  }
  
  def createWorld = { // for testing pourposes only
    new World
  }
  
  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
  	val transmissibilityRate = 40
  	val deadRate = 25
  	
  	val maxDays = 5 // 1 - 5
  	val infectedDays = 6	// 6
  	val dieDays = 8		// 14
  	val immuneDays = 2 	// 16
  	val healthyDays = 2 // 18
  	
  	val dx = Array(0,1,0,-1)
  	val dy = Array(1,0,-1,0)
  }

  import SimConfig._

  val persons: List[Person] = {
    ((0 until SimConfig.population) map 
    	{ i:Int => val p = new Person(i) ; p.infected = i < (SimConfig.prevalenceRate * SimConfig.population) ; p }).toList
  }

  class Person (val id: Int) {
    {
      afterDelay(0)(if(infected) (afterDelay(SimConfig.infectedDays)(getSick)))
      move()
    }
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def move() {
      afterDelay(1 + randomBelow(SimConfig.maxDays))(moveRoom)
    }
    
    def moveRoom() {
      if(dead) return;
      
      val possibleMoves = myWorld.neighboors(row, col) filter (_.livablePlace)
      if(possibleMoves.length > 0) {
        val randomRoom = possibleMoves(randomBelow(possibleMoves.length))
        changePlace(randomRoom.x, randomRoom.y)
      }
      move()
    }
    
    def changePlace(x: Int, y: Int) {
      if(myWorld.coolPlace(x,y)) {
        col = x
        row = y
      }
      
      if(!(infected || immune)) {
        if(randomBelow(100) > SimConfig.transmissibilityRate) {
          infected = true
          afterDelay(SimConfig.infectedDays)(getSick)
        }
      }
    }
    
    def getSick() {
      sick = true
      myWorld.addSick(col, row)
      afterDelay(SimConfig.dieDays)(getDead)
    }
    
    def getDead() {
      if(randomBelow(100) <= SimConfig.deadRate) {
        dead = true
      } else {
        afterDelay(SimConfig.immuneDays)(getImmune)
      }
    }
    
    def getImmune() {
      sick = false
      immune = true
      infected = false
      myWorld.turnHealty(col, row)
      afterDelay(SimConfig.healthyDays)(getHealthy)
    }
    
    def getHealthy() {
      infected = false
      immune = false
    }
    
  }
}
