class AI(private var player: Player, private var depth: Int) extends Solver {

  override def getMoves(b: Board): Array[Move] = {

    // Create Game State from Player and Depth

    val opponent = Player.getOther(player)
    val state = new State(opponent, b, new Move(opponent, b.getLastCol))
    AI.createGameTree(state, depth)

    // Recurse to the bottom and evaluate the boards there 
    evaluateBottomStates(state, 0)

    // TODO use miniMax to evaluate State Tree
    minimax(state)

    // Get final best state 

    //println("*** After MiniMax get Best Col and Val are col: " + state.getBestCol() + " val " + state.value)

    // TODO Return array of best moves (hard coded) 

    val bestMoves = new Array[Move](1)
    bestMoves(0) = new Move(player, state.getBestCol())
    bestMoves
  }

  def evaluateBottomStates(s: State, curDepth: Int) {

    if (curDepth == depth) {
      s.setValue(evaluateBoard(s.getBoard()))
      //println(" In evaluateAllStates curDepth is " + curDepth + "  col is " + s.lastMove.column + " and value is " + s.value)
    }

    if (s.children.length > 0) {
      s.children.map(state => evaluateBottomStates(state, curDepth + 1))
    }
  }

  def XgetBestCol(s: State, p: Player) {

    // If state player is our player we choose the highest value and column
    // otherwise we choose the lowest value 

    // If we've not found the best col yet then find it 
    if (s.getBestCol() == -1 && s.children.length > 0) {

      s.setBestCol(0) // Default move - stops infinite recursion

      // //println(" In getBestCol player is " + p.toString())
      // //println(" and stateChild player is " + s.children(0).player.toString())

      val minOrMax: String = if (p == s.children(0).player) "max" else "min"

      var bestChildValue: Int = Int.MinValue
      if (minOrMax == "min") bestChildValue = Int.MaxValue
      var bestChildCol: Int = 0

      // Loop through the children to find the best value 

      // RECURSE FIRST 
      // Use MiniMax to find best column from children
      for (childState <- s.children) {

        // Recurse !
        if (childState.getBestCol() == -1 && childState.children.length > 0) {
          //println("GOING FOR RECURSION ! with childstate column " + childState.lastMove.column)
          XgetBestCol(childState, p)
          //println(" Out of recursion ")
        }

      }

      // THEN bubble up 
      for (childState <- s.children) {

        // //println("minMax is " + minOrMax + " ChildState last move was " + childState.lastMove.column + " and bestCol was " + childState.getBestCol() + " val was " + childState.getValue())

        if (minOrMax == "min") {

          if (childState.getValue() < bestChildValue) {

            bestChildValue = childState.getValue()

            if (childState.getBestCol() > -1) {
              bestChildCol = childState.getBestCol()
            } else {
              bestChildCol = childState.lastMove.column
            }
            //println(" ++ Setting Col for Best vale of " + bestChildValue + " to " + bestChildCol)
          }
        } else {
          if (childState.getValue() > bestChildValue) {
            bestChildValue = childState.getValue()

            if (childState.getBestCol() > -1) {
              // bestChildCol = childState.lastMove.column
              bestChildCol = childState.getBestCol()
            } else {
              bestChildCol = childState.lastMove.column
            }
          }
        }
      }
      s.setBestCol(bestChildCol)
      s.setValue(bestChildValue)
      ////println(" minMax was " + minOrMax + " bestVal was " + s.getValue)
      ////println(" Best col was " + s.getBestCol())

    }
  }

  def getBestCol(s: State, curDepth: Int) {

    val minOrMax: String = if (curDepth % 2 == 0) "max" else "min"

    var bestChildValue: Int = Int.MinValue
    if (minOrMax == "min") bestChildValue = Int.MaxValue
    var bestChildCol = 0;

    // Iterate over child states, get best col val 
    // THEN bubble up 
    for (childState <- s.children) {
      if (minOrMax == "min") {

        if (childState.getValue() < bestChildValue) {

          bestChildValue = childState.getValue()

          // At the bottom best Col is the actual "Last Move Column"
          // In the middle the best col is the "bestCol" value 
          if (childState.getBestCol() > -1) {
            bestChildCol = childState.getBestCol()
          } else {
            bestChildCol = childState.lastMove.column
          }
        }
      } else {
        if (childState.getValue() > bestChildValue) {
          bestChildValue = childState.getValue()
          if (childState.getBestCol() > -1) {
            bestChildCol = childState.getBestCol()
          } else {
            bestChildCol = childState.lastMove.column
          }
        }
      }
    }
    s.setBestCol(bestChildCol)
    s.setValue(bestChildValue)
    //println("*** minMax was " + minOrMax + " bestVal was " + s.getValue + " Best col was " + s.getBestCol())
  }

  def bubbleUp(s: State, maxDepth: Int, curDepth: Int) {

    if (curDepth == maxDepth) {
      //println(" In bubbleUp curDepth is " + curDepth + " state col is " + s.lastMove.column)

      // Get Best Column 
      getBestCol(s, curDepth)
      
    }

    // Get the best col from the child things 
    if (curDepth < maxDepth) {
      s.children.map(state => bubbleUp(state, maxDepth, curDepth + 1))
    }

  }

  def getBestFinalCol(s: State) {
    //println("+++ In Get Best Final Col ")

    // we need to take the LOWEST value 
    var bestChildValue = Int.MaxValue
    var bestChildCol = 0
    for (childState <- s.children) {

      // //println(" In getBestFinalCol loop value is " + childState.getValue() + " col is " + childState.getBestCol())

      if (childState.getValue() < bestChildValue) {

        bestChildValue = childState.getValue()
        bestChildCol = childState.getBestCol()
      }

    }
    //println(" In getBestFinalCol outer value is " + bestChildValue + " col is " + bestChildCol)
    s.setBestCol(bestChildCol)

  }

  def minimax(s: State) {

    //println(" In Mininax ")

    // Am I at the top level ? 
    // Do I have any children ? 
    // If I DO have children I need to go down to their level and see if 
    // THEY have any children 
    // etc until I get to the bottom

    var maxDepth = depth - 1
    while (maxDepth > 0) {
      //println("maxDepth is " + maxDepth)
      bubbleUp(s, maxDepth, 0)
      maxDepth -= 1
    }

    getBestFinalCol(s)

  }

  def evaluateBoard(b: Board): Int = {

    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }

}

object AI {

  def createGameTreeHelper(s: State, maxDepth: Int, curDepth: Int): Unit = {

    // //println("In createGameTreeHelper at depth  " + curDepth)

    // If we've recursed to the bottom just exit here 
    if (curDepth == maxDepth) {
      ()
    } else {
      s.initializeChildren()
      s.children.map(state => createGameTreeHelper(state, maxDepth, curDepth + 1))
    }
  }

  def createGameTree(s: State, maxDepth: Int): Unit = {

    // Set off filling in state children to max depth
    createGameTreeHelper(s, maxDepth, 0)
  }

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

