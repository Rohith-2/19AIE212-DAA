object insertSort {

   def insertSort(l:Array[Int]) = {
  	for(i<- 1 to l.length-1){
  	var value = l(i)
  	var	location = i - 1
		while (location >= 0 && l(location)>value)
		{
			l(location + 1) = l(location)
			location = location - 1
		}
		l(location+1) = value
		}
  }                                               //> insertSort: (l: Array[Int])Unit
  
 

  
  val L = Array(2,1,5,7,2,4,9,3,10,6,8)           //> L  : Array[Int] = Array(2, 1, 5, 7, 2, 4, 9, 3, 10, 6, 8)
  val t0 = System.nanoTime()                      //> t0  : Long = 38826428180837
  insertSort(L)
  val t1 = System.nanoTime() - t0                 //> t1  : Long = 741129
  println("Elapsed time: " + t1 + " ns")          //> Elapsed time: 741129 ns
  val L1 = List(2,1,5,7,2,4,9,3,10,6,8)           //> L1  : List[Int] = List(2, 1, 5, 7, 2, 4, 9, 3, 10, 6, 8)
  
  
  
  
 def while_(element:Int, sorted:Array[Int]):Array[Int] = sorted match{
  	case a:Array[Int] if a.length == 0  => element +: sorted
  	case _ if sorted(0) < element => sorted(0) +: while_(element, sorted.tail)
  	case _ => element +: sorted
  }                                               //> while_ : (element: Int, sorted: Array[Int])Array[Int]
  
  def insertionSort11(a:Array[Int]):Array[Int] = a match {
  	case a:Array[Int] if a.length == 0 => a;
  	case _ =>
  		val sorted = insertionSort11(a.tail)
  		while_(a.head, sorted)
  }                                               //> insertionSort11: (a: Array[Int])Array[Int]
  
 	val b = Array(35, 12, 56, 78, 12, 34, 67, 12, 45, 95, 45, 79, 69, 25, 64)
                                                  //> b  : Array[Int] = Array(35, 12, 56, 78, 12, 34, 67, 12, 45, 95, 45, 79, 69,
                                                  //|  25, 64)
  val t01 = System.nanoTime()                     //> t01  : Long = 38826445844422
  insertionSort11(b)                              //> res0: Array[Int] = Array(12, 12, 12, 25, 34, 35, 45, 45, 56, 64, 67, 69, 78
                                                  //| , 79, 95)
                                                  
	val tt1 = System.nanoTime()               //> tt1  : Long = 38826454046503
	println("Time taken:"+(tt1-t01)+" in ns") //> Time taken:8202081 in ns
  
  
  
  def insertionSort1(l0:Array[Int]) = {
  	def iter(l:Array[Int],acc:Array[Int]) : Array[Int] = l match {
  		case l:Array[Int] if l.length==0  => acc
  		case _ =>
  			iter ( l.slice(1,l.length),
  				acc.filter(_ < l(0)) ++
  				( l(0) +: acc.filter(_ == l(0)) ) ++
  				acc.filter(_ > l(0))
  				)
  	}
  	iter(l0,Array())
  
  }                                               //> insertionSort1: (l0: Array[Int])Array[Int]
  var L11 = Array(2,1,5,7,2,4,9,3,10,6,8)         //> L11  : Array[Int] = Array(2, 1, 5, 7, 2, 4, 9, 3, 10, 6, 8)
  var t00 = System.nanoTime()                     //> t00  : Long = 38826454496927
  insertionSort1(L11)                             //> res1: Array[Int] = Array(1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  var t11 = System.nanoTime() -t00                //> t11  : Long = 5349091
  
  def insertionSort(l0:List[Int]) = {
  	def iter(l:List[Int],acc:List[Int]) : List[Int] = l match {
  		case Nil => acc
  		case x :: ys => iter(ys,acc.filter(_ < x) ++( x :: acc.filter(_ == x) ) ++acc.filter(_ > x))
  	}
  	iter(l0,Nil)
  }                                               //> insertionSort: (l0: List[Int])List[Int]
  
 
  def insertionSort3(l0:List[Int]) = {
  	
  	def lessThan(l:List[Int], key:Int, acc:List[Int] = Nil): List[Int] = l match {
  		case Nil => acc
  		case x :: ys => if( x < key ) lessThan(ys,key, acc ++ List(x) ) else acc
  	}
  	
  	def greaterThan(l:List[Int], key:Int): List[Int] = l match {
  		case Nil => Nil
  		case x :: ys => if( x > key ) l else greaterThan(ys,key)
  	}
  	
  	def equalTo(l:List[Int], key:Int, acc:List[Int] = Nil): List[Int] = l match {
  		 case Nil => acc
  		 case x :: ys => x match {
  		 		case x if x < key => 	equalTo( ys, key)
  		 		case x if x == key => equalTo( ys, key, x :: acc)
  		 		case x if x > key => 	acc
  		 	}
  	}
  	
  	def iter(l:List[Int],acc:List[Int]) : List[Int] = l match {
  		case Nil => acc
  		case x :: ys =>
  			val lt = lessThan(acc,x)
  			val eq = ( x :: equalTo(acc,x) )
  			val gt = greaterThan(acc,x)
  			//print(lt + " " +eq + " "+ gt+"\n")
  			iter ( ys,
  				lt ++ 	// acc.filter(_ < x) ++
  				eq ++		// ( x :: acc.filter(_ == x) ) ++
  				gt 			// acc.filter(_ > x)
  				)
  	}
  	iter(l0,Nil)
  }                                               //> insertionSort3: (l0: List[Int])List[Int]
  
  var L111 = List(2,1,5,7,2,4,9,3,10,6,8)         //> L111  : List[Int] = List(2, 1, 5, 7, 2, 4, 9, 3, 10, 6, 8)
  t00 = System.nanoTime()
  insertionSort(L111)                             //> res2: List[Int] = List(1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  t11 = System.nanoTime() -t00
  println(t11)                                    //> 3354862

	L111 = List(2,1,5,7,2,4,9,3,10,6,8)
  t00 = System.nanoTime()
  insertionSort3(L111)                            //> res3: List[Int] = List(1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  t11 = System.nanoTime() -t00
  println(t11)                                    //> 1869723
  
  val exec = sc.parallelize(50 to )
}