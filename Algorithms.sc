object Algorithms {
  val l = List(35, 12, 56, 78, 12, 34, 67, 12, 45, 95, 45, 79, 69, 25, 64)
                                                  //> l  : List[Int] = List(35, 12, 56, 78, 12, 34, 67, 12, 45, 95, 45, 79, 69, 25,
                                                  //|  64)
  val a = Array(35, 12, 56, 78, 12, 34, 67, 12, 45, 95, 45, 79, 69, 25, 64)
                                                  //> a  : Array[Int] = Array(35, 12, 56, 78, 12, 34, 67, 12, 45, 95, 45, 79, 69, 
                                                  //| 25, 64)
  
  // Simple recursive
  
  // Length:
  def length(l0:List[Int]):Int = {
		if(l0 == Nil) 0
		else length(l0.tail)+1
  }                                               //> length: (l0: List[Int])Int
   
   /*
  def length1(a0:Array[Int], n:Int):Int = {
		if(a0.isEmpty) n
		else length1(a0, n+1)
  }
    */
  length(l)                                       //> res0: Int = 15
  //length1(a, 0)
  
  // Searching:
  
  def search(l0:List[Int], key:Int): Boolean = {
  	if(l0 == Nil) false
  	
  	if(l0.head == key) true
  	else search(l0.tail, key)
  }                                               //> search: (l0: List[Int], key: Int)Boolean
  
  def search1(a0:Array[Int], n:Int, key:Int): Boolean = {
  	try{
  		if(a0(n) == key) true
  		else search1(a0, n+1, key)
  	}
  	catch{
	  	case e:Exception => false
  	}
  }                                               //> search1: (a0: Array[Int], n: Int, key: Int)Boolean
  
  //search(l, 44)
  search1(a, 0, 44)                               //> res1: Boolean = false
  
  // Divide and conquer
  def quickSort(l0:List[Int], a:Int):List[Int] = {
  	if(l0.length < 2) l0
  	else{
  		val p = l0.head
  		val l_lessThanP = quickSort(l0.filter(_ < p), a) // or (x => x < p) this is a shorthand
  		val l_greaterThanP = quickSort(l0.filter(_ > p), a)
  		val l_equalsP = l0.filter(_ == p)
  		
  		if(a == 0)	l_lessThanP ++ l_equalsP ++ l_greaterThanP
  		else l_greaterThanP ++ l_equalsP ++ l_lessThanP
  	}
  }                                               //> quickSort: (l0: List[Int], a: Int)List[Int]
  // 0 for ascending and anything else for descending
  
  quickSort(l, 0)                                 //> res2: List[Int] = List(12, 12, 12, 25, 34, 35, 45, 45, 56, 64, 67, 69, 78, 
                                                  //| 79, 95)
  quickSort(l, 1)                                 //> res3: List[Int] = List(95, 79, 78, 69, 67, 64, 56, 45, 45, 35, 34, 25, 12, 
                                                  //| 12, 12)
 
}