object Queen {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //8-Queens
  val POP_SIZE = 1000                             //> POP_SIZE  : Int = 1000
  
  val alphabet = "01234567".toCharArray           //> alphabet  : Array[Char] = Array(0, 1, 2, 3, 4, 5, 6, 7)
  def ra = scala.util.Random.nextInt(alphabet.length)
                                                  //> ra: => Int
  val len = alphabet.length                       //> len  : Int = 8
  
  def initialise = {
  	(0 to POP_SIZE).map{i =>
  	val indi = for(j <- 0 until alphabet.length) yield alphabet(ra)
  	indi.map(_.toString).reduce(_+_)
  		
  	}
  	
  }                                               //> initialise: => scala.collection.immutable.IndexedSeq[String]
  
  /*
  p.filter { c =>
  	c.distinct.length == c.length
  } */
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
  }                                               //> fitness: (indi: String)Int
  
  def rt = scala.util.Random.nextInt(len)         //> rt: => Int
  
  def die = scala.util.Random.nextInt(6)          //> die: => Int

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
   }                                              //> breed: (p1: String, p2: String)(String, String)
   
  var p = initialise.map{c => (fitness(c),c)}     //> p  : scala.collection.immutable.IndexedSeq[(Int, String)] = Vector((18,4331
                                                  //| 1415), (19,66677351), (19,23213657), (21,21520327), (18,77215772), (20,7053
                                                  //| 3626), (19,07430573), (23,50572177), (18,24427047), (21,75053747), (22,0046
                                                  //| 3352), (25,57002713), (14,52022545), (18,50610325), (13,43556555), (22,7111
                                                  //| 7520), (23,70320142), (14,01121056), (18,74717671), (16,66755452), (20,0341
                                                  //| 6414), (18,75272165), (20,50601324), (23,64125277), (22,32044715), (22,2570
                                                  //| 7005), (24,60550142), (18,32224364), (23,04603312), (19,64604460), (16,7174
                                                  //| 6677), (21,73260340), (17,25026765), (26,50246130), (19,11150516), (20,6341
                                                  //| 4607), (19,07512300), (16,61666226), (19,67504444), (24,35350474), (19,6642
                                                  //| 1706), (21,73510566), (23,43521475), (21,45213661), (22,35770720), (22,0377
                                                  //| 7253), (19,36301371), (20,17543747), (20,52644231), (20,47200460), (19,2106
                                                  //| 1145), (18,00072333), (19,56754463), (19,11137432), (21,37050433), (18,6626
                                                  //| 6727), (20,51114046), (
                                                  //| Output exceeds cutoff limit.
    
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 20.186813
  
  def select = {
	  
	  // Selection
	  val pGood = p.filter(_._1 > (avgFit))
  	def rg = scala.util.Random.nextInt(pGood.length)
	  
	  // New p
	  p = (0 to POP_SIZE/2).flatMap{j =>
	   val (c1, c2) = breed(pGood(rg)._2, pGood(rg)._2)
	   List((fitness(c1), c1), (fitness(c2), c2))
	  }
	  
	  avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
	  avgFit
	    
   }                                              //> select: => Float
  var i = 0                                       //> i  : Int = 0
  while(avgFit < 26){
  	i += 1
  	println(i + " " + select)
  }                                               //> 1 21.207584
                                                  //| 2 21.758484
                                                  //| 3 21.912176
                                                  //| 4 21.953094
                                                  //| 5 22.065868
                                                  //| 6 22.42016
                                                  //| 7 22.41018
                                                  //| 8 22.452095
                                                  //| 9 22.696608
                                                  //| 10 22.698603
                                                  //| 11 22.718563
                                                  //| 12 22.6497
                                                  //| 13 22.7006
                                                  //| 14 22.62974
                                                  //| 15 22.648703
                                                  //| 16 22.771458
                                                  //| 17 22.909182
                                                  //| 18 22.846308
                                                  //| 19 22.853292
                                                  //| 20 22.831337
                                                  //| 21 23.040918
                                                  //| 22 23.359282
                                                  //| 23 23.551897
                                                  //| 24 23.693613
                                                  //| 25 23.708582
                                                  //| 26 23.791418
                                                  //| 27 23.915169
                                                  //| 28 23.916168
                                                  //| 29 23.988024
                                                  //| 30 24.13074
                                                  //| 31 24.632734
                                                  //| 32 24.857285
                                                  //| 33 24.815369
                                                  //| 34 24.941118
                                                  //| 35 24.879242
                                                  //| 36 25.030937
                                                  //| 37 25.608782
                                                  //| 38 25.709581
                                                  //| 39 25.909182
                                                  //| 40 25.85529
                                                  //| 41 25.994013
                                                  //| 42 26.227545
  p.filter(_._1 == 28).distinct.length            //> res0: Int = 1
  p.filter(_._1 == 28).distinct                   //> res1: scala.collection.immutable.IndexedSeq[(Int, String)] = Vector((28,350
                                                  //| 41726))
  

    
    
}