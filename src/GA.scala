class GA(val name: String) extends Thread {
  var p =GA.initialise.map{c => (GA.fitness(c),c)}
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
  
  def pop = p

  
  def select = {
	  
	  // Selection
	  val pGood = p.filter(_._1 > (avgFit))
  	def rg = scala.util.Random.nextInt(pGood.length)
	  
	  // New p
	  p = (0 to GA.POP_SIZE / 2).flatMap{ j =>
	   val (c1, c2) = GA.breed(pGood(rg)._2, pGood(rg)._2)
	   List((GA.fitness(c1), c1), (GA.fitness(c2), c2))
	  }
	  
	  avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
	  avgFit
	    
   }
  
  override def run ={
    var i = 0
    while(avgFit < 26){
  	i += 1
  	select
  	if (i%500==0)
  	  println("The "+name+" says "+i + " " + avgFit)
    }
    
  }
  
}
  
object GA{

  val POP_SIZE = 10000
  
  val alphabet = "01234567".toCharArray
  def ra = scala.util.Random.nextInt(alphabet.length)
  val len = alphabet.length
  
  def initialise = {
  	(0 to POP_SIZE).map{i =>
  	val indi = for(j <- 0 until alphabet.length) yield alphabet(ra)
  	indi.map(_.toString).reduce(_+_)	
  	}
  }
  
  def fitness(indi:String)={
  	val c = indi.toCharArray.map(_.toInt - 48)
  	//We subtract coz we get acsii hence to convert the subtraction
  	var fit = 0
  	(0 to 7).foreach{i =>
  		(i+1 to 7).foreach{j =>
  			val cj = if(c(i) > c(j)) c(i)+i-j else c(i)-i+j
  			if(c(i) == c(j) || cj ==c(j) ) fit+=1
  			
  		}
  	}
  	(28 - fit)
  }
  
  def rt = scala.util.Random.nextInt(len)
  
  def die = scala.util.Random.nextInt(6)

  def breed(p1: String, p2: String) = {
    val cPoint = rt

    // Crossover...
    var child1 = p1.substring(0, cPoint) + p2.substring(cPoint)
    var child2 = p2.substring(0, cPoint) + p1.substring(cPoint)

    // Mutation... probabilistic
    die match {
      case 1 => (child1.replace(child1.charAt(rt), alphabet(ra)), child2)
      case 2 => (child1, child2.replace(child2.charAt(rt), alphabet(ra)))
      case _ => (child1, child2)
    }
  }
 
  
 def main(args: Array[String])={
    val G = (0 to 5).map{x=> new GA(x.toString())}
    G.foreach(_.start())
    
    def conc = !G.map(_.isAlive).reduce(_ || _)
    
    while(!conc) Thread.sleep(100)
    
    G.flatMap(_.pop.filter(_._1 == 28)).distinct.foreach(println)
    
}
 
}