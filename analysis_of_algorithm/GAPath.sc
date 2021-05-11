object GAPath{

  val path_len = 25                               //> path_len  : Int = 25
  def f(x:Int) = 2*x*x - x                        //> f: (x: Int)Int
  val path = (0 to path_len).map(x=>f(x)).toArray //> path  : Array[Int] = Array(0, 1, 6, 15, 28, 45, 66, 91, 120, 153, 190, 231, 
                                                  //| 276, 325, 378, 435, 496, 561, 630, 703, 780, 861, 946, 1035, 1128, 1225)
                                                 // Array(516, 1, 2, 334, 253, 7
  val pop = 10                                    //> pop  : Int = 10
  val len = path.length                           //> len  : Int = 26
  val (lb, ub) = (0, 1000)                        //> lb  : Int = 0
                                                  //| ub  : Int = 1000
  
  def pred(B:Array[Int]) = (0 to path_len).map{x =>
    var s = 0
  	for (i<-0 until B.length){
  		var b = B(i)
  		s+=math.abs(path(i)-b)
  	}
  	s/B.length.toFloat
  	}.reduce(_+_)                             //> pred: (B: Array[Int])Float
  

  def initialize = (0 to pop).toArray.map{ i =>
  	(0 to path_len).map(_ => scala.util.Random.nextInt(ub)).toArray
  	}                                         //> initialize: => Array[Array[Int]]
  
  def fitness(x:Array[Int]) = pred(x)             //> fitness: (x: Array[Int])Float
    
  def rt = scala.util.Random.nextInt(len)         //> rt: => Int
  def die = scala.util.Random.nextInt(4)          //> die: => Int
  
  def breed(x1:Array[Int], x2:Array[Int]) = {
  	val index = rt
	  val child1 = x1.take(index)++x2.takeRight(x2.length-index)
	  val child2 = x2.take(index)++x1.takeRight(x1.length-index)
		
	  die match {
	  	case 1 => child1(index)=index;(child1,child2)
	  	case 2 => child2(index)=index;(child1,child2)
	  	case _ => (child1,child2)
	  	}
	  
  }                                               //> breed: (x1: Array[Int], x2: Array[Int])(Array[Int], Array[Int])
 
  var p = initialize.map( indi => (fitness(indi),indi))
                                                  //> p  : Array[(Float, Array[Int])] = Array((11661.0,Array(527, 782, 57, 444, 4
                                                  //| 62, 917, 847, 521, 963, 841, 252, 385, 467, 776, 5, 518, 735, 120, 638, 438
                                                  //| , 133, 422, 210, 794, 153, 705)), (9799.997,Array(784, 792, 144, 464, 130, 
                                                  //| 80, 812, 994, 434, 27, 113, 412, 636, 799, 74, 748, 162, 223, 617, 701, 48,
                                                  //|  599, 602, 923, 733, 54)), (10241.002,Array(634, 310, 586, 548, 369, 740, 3
                                                  //| 78, 171, 625, 695, 198, 978, 466, 805, 516, 971, 308, 318, 851, 462, 616, 9
                                                  //| 29, 686, 855, 44, 263)), (11203.996,Array(3, 659, 968, 513, 270, 592, 761, 
                                                  //| 852, 126, 50, 954, 338, 668, 129, 618, 37, 572, 576, 799, 997, 970, 521, 18
                                                  //| 0, 41, 442, 123)), (9007.999,Array(377, 338, 587, 597, 504, 250, 665, 149, 
                                                  //| 63, 244, 29, 521, 255, 293, 764, 647, 880, 367, 433, 384, 559, 823, 71, 87,
                                                  //|  757, 229)), (9013.001,Array(151, 535, 61, 696, 691, 10, 529, 76, 485, 846,
                                                  //|  737, 899, 814, 465, 690, 480, 519, 198, 839, 884, 309, 432, 407, 799, 781,
                                                  //|  915)), (7858.998,Array
                                                  //| Output exceeds cutoff limit.
	
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 9832.272
  
  	                
  def select = {
	  
	  // Selection
	  val pGood = p.filter(_._1 <= (avgFit))
  	def rg = {
	  	var r = 0
	  	if(pGood.length>0){
	  		r = scala.util.Random.nextInt(pGood.length)
	  	}
	  	else{println("Mafi Array")}
	  	r
  	}
	  
	  // New p
	  p = (0 to pop).flatMap{j =>
	  val x = rg
	  val y = rg
	   val (c1, c2) = breed(pGood(x)._2, pGood(y)._2)
	   List((fitness(c1), c1),(fitness(c2),c2))
	  }.toArray
	  
	  avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
	  avgFit
	    
   }                                              //> select: => Float
   
  while(avgFit > 4000){
  	select
  }
          
  p.filter(t => t._1 <= 300)                      //> res0: Array[(Float, Array[Int])] = Array()
         
  p.length                                        //> res1: Int = 22

  
}