import java.io._
import util.Random


object Merge_sort {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
 
 def merge(l1:List[Int],l2:List[Int]):List[Int] = (l1,l2) match{
     case (Nil,_) => l2
     case (_,Nil) => l1
     case(h1::t1,h2::t2) =>
         if (h1 < h2) h1::merge(t1,l2)
         else h2::merge(l1,t2)
  
  }                                               //> merge: (l1: List[Int], l2: List[Int])List[Int]
 def mergesort(lst:List[Int]):List[Int]= lst match {
      case Nil => lst
      case h::Nil => lst
      case _=>
         val (l1,l2) = lst.splitAt(lst.length/2)
         merge(mergesort(l1),mergesort(l2))
  }                                               //> mergesort: (lst: List[Int])List[Int]
  mergesort(List.fill(200)(Random.nextInt))       //> res0: List[Int] = List(-2146653170, -2140655970, -2090824057, -2089343444, -
                                                  //| 2085032709, -2056581600, -2034266871, -1985797474, -1965670574, -1862481346,
                                                  //|  -1844992281, -1799948431, -1791327439, -1789900401, -1780861685, -178017810
                                                  //| 6, -1762518526, -1666871125, -1636045901, -1634134371, -1629021675, -1580658
                                                  //| 154, -1551664289, -1546288642, -1527358004, -1503242492, -1491028864, -14520
                                                  //| 97577, -1439306289, -1420556317, -1322750386, -1275239335, -1240544562, -120
                                                  //| 9760827, -1203575338, -1195084819, -1186346680, -1101037616, -1053010490, -1
                                                  //| 026030366, -972977697, -921639224, -902597069, -838801856, -824164195, -7418
                                                  //| 04730, -725078633, -704458340, -660547424, -647205305, -644471060, -61477120
                                                  //| 7, -595978295, -583080242, -580403676, -555458964, -547482027, -515277203, -
                                                  //| 495069149, -492882436, -486951101, -453213138, -449534041, -440914166, -4099
                                                  //| 09972, -408764714, -404664134, -379188450, -370598088, -368321199, -33755054
                                                  //| 8, -317033002, -27442690
                                                  //| Output exceeds cutoff limit.
  
 val end = 100                                    //> end  : Int = 100
 val begn = 2                                     //> begn  : Int = 2
 var fibTime:Array[Long] = Array()                //> fibTime  : Array[Long] = Array()
 val nVal = (begn to end).toArray                 //> nVal  : Array[Int] = Array(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1
                                                  //| 6, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 3
                                                  //| 5, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 5
                                                  //| 4, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 7
                                                  //| 3, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 9
                                                  //| 2, 93, 94, 95, 96, 97, 98, 99, 100)
 
 for(i <- begn to end) {
   var avgTime:Array[Long] = Array()
  	for (_ <-0 to 5) {
	    var x = List.fill(i)(1000).map(Random.nextInt)
	  	val t0 = System.nanoTime()
	  	mergesort(x)
	  	val t1 = System.nanoTime() - t0
	  	avgTime = avgTime :+ t1
  	}
  	fibTime = fibTime :+ (avgTime.reduce(_+_)/avgTime.length)
  }
  val time = fibTime                              //> time  : Array[Long] = Array(28116, 13983, 20800, 30600, 32883, 35000, 48133
                                                  //| , 36666, 43983, 37183, 33400, 40500, 39733, 39016, 52316, 52416, 104883, 58
                                                  //| 116, 60500, 53350, 59383, 53583, 57283, 57000, 59666, 67016, 67916, 70300, 
                                                  //| 70466, 74816, 77750, 83400, 85283, 90283, 87050, 92783, 94783, 107616, 1032
                                                  //| 83, 105666, 108433, 132116, 115000, 118700, 120600, 128316, 125550, 130450,
                                                  //|  144333, 135866, 164550, 66000, 47666, 43300, 45400, 46766, 51233, 50083, 4
                                                  //| 8583, 903283, 62400, 63783, 66183, 49966, 59850, 49550, 40283, 38466, 38016
                                                  //| , 39533, 41033, 41083, 42100, 41350, 41666, 43333, 45433, 61516, 46716, 447
                                                  //| 33, 45966, 46150, 48000, 49650, 55383, 47366, 37850, 41133, 47866, 39433, 4
                                                  //| 0166, 42516, 41966, 41800, 42400, 42933, 42483, 43200, 43483)
 val file = new File("C:/Users/Anirudh Bhaskar/Documents/MergeSort.csv")
                                                  //> file  : java.io.File = C:\Users\Anirudh Bhaskar\Documents\MergeSort.csv
	val bw = new BufferedWriter(new FileWriter(file))
                                                  //> bw  : java.io.BufferedWriter = java.io.BufferedWriter@3ecd23d9
	for(n<-1 until time.length){
		bw.write(nVal(n).toString+","+time(n).toString+"\n")
	}
	bw.close()
}