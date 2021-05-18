object GAPath{

  val path_len = 25                               //> path_len  : Int = 25
  def f(x:Int) = 2*x*x - x                        //> f: (x: Int)Int
  val path = (0 to path_len).map(x=>f(x)).toArray //> path  : Array[Int] = Array(0, 1, 6, 15, 28, 45, 66, 91, 120, 153, 190, 231, 
                                                  //| 276, 325, 378, 435, 496, 561, 630, 703, 780, 861, 946, 1035, 1128, 1225)
  val pop = 25                                    //> pop  : Int = 25
  val len = path.length                           //> len  : Int = 26
  val (lb, ub) = (0, 2000)                        //> lb  : Int = 0
                                                  //| ub  : Int = 2000
  
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
                                                  //> p  : Array[(Float, Array[Int])] = Array((17329.998,Array(405, 1887, 882, 15
                                                  //| 51, 1255, 1, 629, 56, 682, 1676, 1391, 1963, 180, 945, 1184, 1050, 650, 137
                                                  //| 9, 157, 1259, 1170, 1199, 883, 1244, 1142, 1813)), (19101.998,Array(85, 173
                                                  //| 2, 981, 551, 1458, 71, 899, 812, 1362, 1925, 1105, 1330, 1822, 1775, 904, 2
                                                  //| 22, 639, 775, 270, 785, 1166, 1284, 1339, 1865, 123, 1391)), (25446.996,Arr
                                                  //| ay(709, 1024, 1274, 1821, 391, 1302, 1351, 1560, 1728, 598, 506, 524, 760, 
                                                  //| 1735, 1781, 1996, 1997, 1473, 1963, 1550, 1964, 634, 331, 106, 364, 1660)),
                                                  //|  (21572.996,Array(1897, 634, 355, 1109, 1262, 1086, 1724, 443, 678, 1791, 5
                                                  //| 12, 1514, 1495, 925, 817, 1547, 881, 24, 1830, 1362, 769, 1623, 1712, 180, 
                                                  //| 415, 969)), (21532.008,Array(1692, 483, 232, 870, 1905, 1048, 1318, 1232, 1
                                                  //| 817, 844, 1546, 1531, 556, 1395, 12, 1862, 455, 805, 1241, 134, 1180, 1354,
                                                  //|  1676, 1326, 451, 1986)), (15803.994,Array(1543, 156, 590, 238, 880, 1411, 
                                                  //| 219, 974, 217, 912, 112
                                                  //| Output exceeds cutoff limit.
	
  var avgFit = p.map(_._1).reduce(_+_)/p.length.toFloat
                                                  //> avgFit  : Float = 19743.154
  
  	                
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
           
  p.length                                        //> res1: Int = 52
  p                                               //> res2: Array[(Float, Array[Int])] = Array((3996.999,Array(0, 101, 2, 3, 4, 5
                                                  //| , 6, 7, 8, 489, 136, 11, 284, 539, 43, 904, 508, 322, 783, 1068, 886, 857, 
                                                  //| 682, 717, 945, 1506)), (4076.9993,Array(0, 101, 2, 3, 4, 5, 6, 7, 8, 9, 136
                                                  //| , 183, 284, 539, 43, 1119, 508, 322, 783, 109, 886, 857, 682, 717, 945, 150
                                                  //| 6)), (3948.0007,Array(0, 101, 2, 3, 4, 5, 6, 7, 8, 489, 136, 11, 284, 539, 
                                                  //| 43, 15, 508, 322, 783, 1068, 886, 857, 682, 717, 945, 1506)), (3920.9993,Ar
                                                  //| ray(0, 101, 2, 3, 4, 5, 6, 7, 8, 9, 136, 183, 284, 292, 953, 904, 508, 322,
                                                  //|  783, 109, 886, 857, 682, 717, 945, 1506)), (4076.9993,Array(0, 101, 2, 3, 
                                                  //| 4, 5, 6, 7, 8, 9, 136, 183, 284, 539, 43, 1119, 508, 322, 783, 109, 886, 85
                                                  //| 7, 682, 717, 945, 1506)), (3948.0007,Array(0, 101, 2, 3, 4, 5, 6, 7, 8, 489
                                                  //| , 136, 11, 284, 539, 43, 15, 508, 322, 783, 1068, 886, 857, 682, 717, 945, 
                                                  //| 1506)), (3937.9985,Array(0, 101, 2, 3, 4, 5, 6, 7, 8, 9, 136, 183, 284, 539
                                                  //| , 43, 15, 508, 322, 783
                                                  //| Output exceeds cutoff limit.

  
}