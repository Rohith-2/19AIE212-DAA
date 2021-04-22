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
   
  var p = initialise.map{c => (fitness(c),c)}     //> p  : scala.collection.immutable.IndexedSeq[(Int, String)] = Vector((18,2101
                                                  //| 6415), (16,57711101), (23,67146042), (23,51003613), (20,23612602), (22,3361
                                                  //| 7640), (15,32245274), (20,06063503), (21,07603315), (18,54374576), (18,4557
                                                  //| 4357), (17,56675455), (17,66121264), (21,44145136), (22,41503320), (21,6253
                                                  //| 2764), (18,14434077), (21,17716614), (21,45013601), (19,05672743), (14,5411
                                                  //| 1112), (21,23631315), (21,32011252), (14,66633633), (22,72655141), (17,6764
                                                  //| 6222), (22,05302437), (19,73172745), (23,51520477), (19,12001422), (18,6763
                                                  //| 0776), (17,62262312), (16,01213337), (15,00002322), (21,17165274), (19,1106
                                                  //| 1222), (23,07241623), (22,37576107), (22,00747605), (19,00764064), (22,0272
                                                  //| 3651), (19,15676261), (15,04304334), (17,20234343), (19,05421504), (21,0740
                                                  //| 2757), (17,42375544), (22,05527116), (19,55311067), (21,36043754), (24,1160
                                                  //| 7526), (23,25637540), (19,55361214), (21,47352527), (20,17535735), (25,2073
                                                  //| 0541), (20,06164117), (
                                                  //| Output exceeds cutoff limit.
    
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 20.077923
  
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
  }                                               //> 1 21.16966
                                                  //| 2 21.73054
                                                  //| 3 21.865269
                                                  //| 4 22.137724
                                                  //| 5 22.498005
                                                  //| 6 22.505987
                                                  //| 7 22.659681
                                                  //| 8 22.59481
                                                  //| 9 22.552895
                                                  //| 10 22.557884
                                                  //| 11 22.683632
                                                  //| 12 22.68064
                                                  //| 13 22.692616
                                                  //| 14 22.813374
                                                  //| 15 22.848303
                                                  //| 16 22.842316
                                                  //| 17 22.908184
                                                  //| 18 22.966068
                                                  //| 19 23.047905
                                                  //| 20 23.431137
                                                  //| 21 23.728542
                                                  //| 22 23.933134
                                                  //| 23 24.072855
                                                  //| 24 24.753492
                                                  //| 25 24.883234
                                                  //| 26 24.961079
                                                  //| 27 25.030937
                                                  //| 28 25.739521
                                                  //| 29 25.74451
                                                  //| 30 25.779442
                                                  //| 31 25.773453
                                                  //| 32 25.82435
                                                  //| 33 25.827345
                                                  //| 34 25.87026
                                                  //| 35 25.890219
                                                  //| 36 25.948103
                                                  //| 37 26.021955
  p.filter(_._1 == 28).distinct.length            //> res0: Int = 0
  p.filter(_._1 == 28).distinct                   //> res1: scala.collection.immutable.IndexedSeq[(Int, String)] = Vector()
  

    
    
}