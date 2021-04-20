object GA_1 {

  /*
  	0. Representations(genes and choromosem/ candidate solution)
  	1. Initial Population (random generation of a list of chromosomes)
  	2. Fitness Function
  	3. Genetic operators
  	4. Selection
  	5. Generations and stopage
  */
  
  def f(x:Int) = x*x - 5*x + 6 // (x-2)(x-3) => x=2,3
                                                  //> f: (x: Int)Int
  
  // 0. Representation (binary string)
  val len = 5 // 11111(b2) -> 0x1F(b16) -> 31(b10)//> len  : Int = 5
  val (lb, ub) = (0, 31)                          //> lb  : Int = 0
                                                  //| ub  : Int = 31
  
  // 1. Initialisation .... Initial population
  def initialize = (0 to 10).toArray.map{ i =>
  	scala.util.Random.nextInt(ub+1)
  	}                                         //> initialize: => Array[Int]
  
  // 2. Fitness -> measures weakness ... higher the fitness more the weakness.. i.e. fitness should give a val = 0
  def fitness(x:Int) = math.abs(f(x))             //> fitness: (x: Int)Int
    
  def rt = scala.util.Random.nextInt(len)         //> rt: => Int
  def die = scala.util.Random.nextInt(6)          //> die: => Int
  
  /* 
   index = 3
   e.g. => 10111 (when mask1 is applied) => 101
   
   				 101 11               011 10
   (mask1) 111 00				(mask2)	000 11
          ---&---							---&---
  				 101 00				|				000 10
   
  
  Example Demo for Mutation
  __________________________________________________________________________
  */
  val (x1, x2) = (1,7)                            //> x1  : Int = 1
                                                  //| x2  : Int = 7
  val index = 3                                   //> index  : Int = 3
  val mask1 = ( 0x1F << (len-index)) & 0x1F       //> mask1  : Int = 28
  val mask2 = ( 0x1F >> index) & 0x1F             //> mask2  : Int = 3
    
  val c1 = (x1 & mask1) | (x2 & mask2)            //> c1  : Int = 3
  val c2 = (x2 & mask1) | (x1 & mask2)            //> c2  : Int = 5
  
  Integer.toBinaryString(mask1)                   //> res0: String = 11100
  Integer.toBinaryString(mask2)                   //> res1: String = 11
  Integer.toBinaryString(x1)                      //> res2: String = 1
  Integer.toBinaryString(x2)                      //> res3: String = 111
  Integer.toBinaryString(c1)                      //> res4: String = 11
  Integer.toBinaryString(c2)                      //> res5: String = 101
  
  /*
  _____________________________________________________________________________
  */
  
  // 3. Genetic operators
  def breed(x1:Int, x2:Int) = {
  	val index = rt
	  val mask1 = ( 0x1F << (len-index)) & 0x1F
	  val mask2 = ( 0x1F >> index) & 0x1F
	  
	  val child1 = (x1 & mask1) | (x2 & mask2)
	  val child2 = (x2 & mask1) | (x1 & mask2)
	  
	  die match {
	  	case 1 => ((child1 | 0x1 << rt) & 0x1F, child2)
	  	case 2 => (child1, (child2 | 0x1 << rt) & 0x1F)
	  	case _ => (child1, child2)
	  	}
  }                                               //> breed: (x1: Int, x2: Int)(Int, Int)
  
  
  var p = initialize.map( indi => (fitness(indi),indi))
                                                  //> p  : Array[(Int, Int)] = Array((12,6), (462,24), (600,27), (2,1), (600,27),
                                                  //|  (72,11), (650,28), (552,26), (110,13), (132,14), (6,5))
  


	
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 290.72726
  
  	                
  def select = {
	  
	  // Selection
	  val pGood = p.filter(_._1 < (avgFit))
  	def rg = scala.util.Random.nextInt(pGood.length)
	  
	  // New p
	  p = (0 to 10).flatMap{j =>
	   val (c1, c2) = breed(pGood(rg)._2, pGood(rg)._2)
	   List((fitness(c1), c1), (fitness(c2), c2))
	  }.toArray
	  
	  avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
	  avgFit
	  
   }                                              //> select: => Float
   
  var i = 0                                       //> i  : Int = 0
  while(avgFit > 1){
  	i += 1
  	println(i + " " + select)
  }                                               //> 1 90.63636
                                                  //| 2 34.272728
                                                  //| 3 77.181816
                                                  //| 4 59.545456
                                                  //| 5 14.727273
                                                  //| 6 25.0
                                                  //| 7 35.81818
                                                  //| 8 8.727273
                                                  //| 9 50.727272
                                                  //| 10 24.818182
                                                  //| 11 15.363636
                                                  //| 12 3.4545455
                                                  //| 13 21.272728
                                                  //| 14 0.45454547
  
  p.filter(t => t._1 == 0)                        //> res6: Array[(Int, Int)] = Array((0,3), (0,2), (0,2), (0,2), (0,2), (0,2), (
                                                  //| 0,2), (0,3), (0,2), (0,2), (0,2), (0,2), (0,2), (0,2), (0,2), (0,2), (0,2),
                                                  //|  (0,2), (0,2))
      
  p.length                                        //> res7: Int = 22
  
  
}