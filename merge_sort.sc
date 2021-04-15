object merge_sort {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val a1 = List(23,34,56,67)                      //> a1  : List[Int] = List(23, 34, 56, 67)
  val a2 = List(12,35,54,70)                      //> a2  : List[Int] = List(12, 35, 54, 70)
  
  /*
  Pairs
  Not a Data Structure but a data item, we cant access it using indexs
  */
  val p = ("Rohith",19)                           //> p  : (String, Int) = (Rohith,19)
  p._1                                            //> res0: String = Rohith
  
  
  //Recursive but not Tail recursive
  def merge (a1:List[Int],a2:List[Int]):List[Int] = (a1,a2) match{
  	case (Nil,_) => a2
  	case (_,Nil) => a1
  	case(x1::y1s,x2::y2s) =>
  		if(x1 < x2) x1 :: merge (y1s,a2)
  		else x2 :: merge (a1,y2s)
  }                                               //> merge: (a1: List[Int], a2: List[Int])List[Int]
  
  merge(a1,a2)                                    //> res1: List[Int] = List(12, 23, 34, 35, 54, 56, 67, 70)
  
  def mergesort(a:List[Int]):List[Int] ={
    val len = a.length/2
    if(len<1) a
    else{
  	val (l,r) = a.splitAt(len)
  	merge(mergesort(l),mergesort(r))
  	}
  }                                               //> mergesort: (a: List[Int])List[Int]
  
  mergesort(List(2,5,4,3,2,6,7,5,12,21,11,65,32,9,0,3,60))
                                                  //> res2: List[Int] = List(0, 2, 2, 3, 3, 4, 5, 5, 6, 7, 9, 11, 12, 21, 32, 60, 
                                                  //| 65)
                                                  
                                                  //> f  : Double = 0.0
}