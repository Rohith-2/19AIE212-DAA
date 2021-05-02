object mergesort {
  
 def merge(l1:List[Double],l2:List[Double]):List[Double] = (l1,l2) match{
     case (Nil,_) => l2.reverse:::l2
     case (_,Nil) => l2.reverse:::l1
     case(h1::t1,h2::t2) =>
         if (h1 < h2) h1::merge(t1,l2)
         else h2::merge(l1,t2)
  
  }                                               //> merge: (l1: List[Double], l2: List[Double])List[Double]
 def mergesort(lst:List[Double]):List[Double]= lst match {
      case Nil => lst
      case h::Nil => lst
      case _=>
         val (l1,l2) = lst.splitAt(lst.length/2)
         merge(mergesort(l1),mergesort(l2))
  }                                               //> mergesort: (lst: List[Double])List[Double]
  mergesort(List.fill(10000)(math.random))        //> java.lang.StackOverflowError
                                                  //| 	at scala.collection.immutable.Nil$.equals(List.scala:433)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:4)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mergesort.scala:8)
                                                  //| 	at mergesort$.merge$1(mer
                                                  //| Output exceeds cutoff limit.
}