

class Node (val depth: Int, val column: Int) {
  
  var bestChildValue: Int = Int.MinValue
  var bestChildMove: Int = 0
  var value: Int = Int.MinValue
  var childNodes: Array[Node] = _
  
  def isBottom(): Boolean = { 
    (depth == 0)
  }
  
  def testBestChild(value: Int, col: Int) {
    if (value > bestChildValue) {
      bestChildValue = value
      bestChildMove = col
    }
  }
  
  
  def getBestChild(): Unit = {
    
    if (isBottom()) Unit
    
    childNodes.map(childNode => testBestChild(childNode.value, childNode.column))
    
  }
  
  
}

