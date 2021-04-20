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
   
  var p = initialise.map{c => (fitness(c),c)}     //> p  : scala.collection.immutable.IndexedSeq[(Int, String)] = Vector((20,2077
                                                  //| 0446), (17,01554141), (23,60217332), (24,74020736), (22,67361572), (20,1213
                                                  //| 1756), (23,73051272), (18,47600311), (21,25431425), (16,40674654), (20,3621
                                                  //| 0162), (17,50335330), (16,44343174), (20,21043701), (22,07570255), (20,2241
                                                  //| 4105), (23,00165213), (19,73151251), (23,64063377), (16,12212174), (22,7566
                                                  //| 4004), (17,32123765), (20,41274152), (24,55177620), (21,12364202), (25,4273
                                                  //| 1500), (22,37770235), (20,06276002), (15,43210626), (17,37432106), (22,4610
                                                  //| 3621), (17,41236113), (17,40663453), (20,23771321), (22,50736664), (21,4205
                                                  //| 2743), (24,64075331), (23,35205117), (20,45015373), (22,77201751), (19,6244
                                                  //| 5312), (19,46424354), (20,56564752), (24,14570265), (18,32532560), (21,1036
                                                  //| 6540), (17,41217317), (19,35473754), (22,10570371), (17,00724577), (20,3301
                                                  //| 2776), (23,04126035), (21,11703472), (21,51604711), (13,12332133), (24,5720
                                                  //| 3502), (20,07067402), (
                                                  //| Output exceeds cutoff limit.
    
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 20.053946
  
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
  while(avgFit < 24){
  	i += 1
  	println(i + " " + select)
  }                                               //> 1 21.39421
                                                  //| 2 21.82535
                                                  //| 3 21.842316
                                                  //| 4 21.90519
                                                  //| 5 22.042913
                                                  //| 6 22.308384
                                                  //| 7 22.41916
                                                  //| 8 22.52994
                                                  //| 9 22.597805
                                                  //| 10 22.597805
                                                  //| 11 22.713573
                                                  //| 12 22.72555
                                                  //| 13 22.7994
                                                  //| 14 22.735529
                                                  //| 15 22.879242
                                                  //| 16 22.765469
                                                  //| 17 22.787426
                                                  //| 18 22.802395
                                                  //| 19 22.96008
                                                  //| 20 22.971058
                                                  //| 21 23.061876
                                                  //| 22 23.553892
                                                  //| 23 23.918163
                                                  //| 24 23.99002
                                                  //| 25 24.150698
  p.filter(_._1 == 0).distinct.length             //> res0: Int = 0
     
  

    
    
}