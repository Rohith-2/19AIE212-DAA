object GA {
  // best fit target solution
  val target = "rohith"                           //> target  : String = rohith
  val len = target.length                         //> len  : Int = 6
  
  val alphabet = "artmieouhc".toCharArray         //> alphabet  : Array[Char] = Array(a, r, t, m, i, e, o, u, h, c)
  
  def r = scala.util.Random.nextInt(alphabet.length)
                                                  //> r: => Int
                                                 
  def fitness(indi:String) = {
  	val cntArray = for(i <- 0 until len) yield (if(target(i) == indi(i)) 1 else 0)
  	cntArray.reduce(_+_)/target.length.toFloat * 100
  }                                               //> fitness: (indi: String)Float
  
  // Generating random index over pGood
  def rt = scala.util.Random.nextInt(len)         //> rt: => Int
  def die = scala.util.Random.nextInt(6)          //> die: => Int
  
  def breed(p1:String, p2:String) = {
  	val cPoint = rt
  	
  	// Crossover...
  	var child1 = p1.substring(0,cPoint)+p2.substring(cPoint)
  	var child2 = p2.substring(0,cPoint)+p1.substring(cPoint)
  	
  	// Mutation... probabilistic
  	die match {
  		case 1 => (child1.replace(child1.charAt(rt),alphabet(r)), child2)
  		case 2 => (child1, child2.replace(child2.charAt(rt),alphabet(r)))
  		case _ => (child1, child2)
  	}
  	
  }                                               //> breed: (p1: String, p2: String)(String, String)
  
  // Generate random population
  var p = (0 to 1000).map{i =>
  	var indi = ""
  	(for(j <- 0 until len) yield alphabet(r).toString).reduce(_+_)
  }.map(indi => (fitness(indi), indi))            //> p  : scala.collection.immutable.IndexedSeq[(Float, String)] = Vector((16.66
                                                  //| 6668,hehahc), (0.0,ucauce), (16.666668,cmiceh), (16.666668,uharuh), (16.666
                                                  //| 668,tmhmhc), (16.666668,ctamti), (16.666668,mreimo), (0.0,heumir), (16.6666
                                                  //| 68,ehrihc), (0.0,eumacu), (0.0,ohiuai), (0.0,eeetet), (0.0,omouhc), (0.0,er
                                                  //| mchr), (0.0,ueotcu), (16.666668,ioccua), (33.333336,ioiete), (0.0,immtcm), 
                                                  //| (16.666668,tahtmo), (0.0,ehoore), (0.0,criuue), (16.666668,rircec), (33.333
                                                  //| 336,ohorth), (0.0,taouct), (0.0,amucco), (0.0,eumhoi), (16.666668,rumuea), 
                                                  //| (16.666668,coraut), (0.0,huioeu), (0.0,oemhci), (16.666668,mroerh), (0.0,tr
                                                  //| trru), (0.0,mmehma), (16.666668,ahtrtc), (16.666668,ruruhc), (16.666668,eoe
                                                  //| omc), (0.0,caremi), (0.0,marrmu), (0.0,hhcuhu), (0.0,memtim), (0.0,ectomi),
                                                  //|  (0.0,ieaoiu), (0.0,ettrma), (16.666668,riirat), (0.0,ocoahe), (0.0,itthmr)
                                                  //| , (0.0,ieaoae), (16.666668,ichtau), (0.0,hutorr), (0.0,mcahct), (0.0,huctce
                                                  //| ), (16.666668,rreeci), 
                                                  //| Output exceeds cutoff limit.
  
  
	var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 9.5238085
                                                  
  def select = {
	  
	  // Selection
	  val pGood = p.filter(_._1 > (avgFit+1))
  def rg = scala.util.Random.nextInt(pGood.length)
	  
	  // New p
	  p = (0 to 500).flatMap{j =>
	   val (c1, c2) = breed(pGood(rg)._2, pGood(rg)._2)
	   List((fitness(c1), c1), (fitness(c2), c2))
	  }
	  
	  avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
	  avgFit
	  
  }                                               //> select: => Float
  
  (0 to 10).foreach{ i =>
  	var aF = select
  	println(i + " " + aF)
  }                                               //> 0 20.309368
                                                  //| 1 35.595505
                                                  //| 2 51.081192
                                                  //| 3 67.19907
                                                  //| 4 82.285446
                                                  //| 5 85.24631
                                                  //| 6 96.523636
                                                  //| 7 96.47375
                                                  //| 8 96.88958
                                                  //| 9 96.57352
                                                  //| 10 96.124435
  
  p.filter(t => t._2 == target).length            //> res0: Int = 832
  
  
}