import java.io._
import util.Random

object ShellSort {
	
	
def ShellSort(a:Array[Int]){
		val n = a.length;
		var gap:Int = n/2
		
	while(gap>0){
		for(i<-gap until n){
			var temp = a(i)
			var j = i
			
			while(j>=gap && a(j-gap)>temp){
				a(j) = a(j-gap)
				j-=1
			}
			a(j) = temp
		}
		gap/=2
		}
}                                                 //> ShellSort: (a: Array[Int])Unit

val n = 100                                       //> n  : Int = 100
var fibTime:Array[Long] = Array()                 //> fibTime  : Array[Long] = Array()

  for(i <- 3 to n){
    var x = Array.fill(i)(10000).map(Random.nextInt)
  	val t0 = System.nanoTime()
  	ShellSort(x)
  	val t1 = System.nanoTime() - t0
  	fibTime = fibTime :+ t1
  }
  val time = fibTime                              //> time  : Array[Long] = Array(532667, 12846, 12404, 24990, 14229, 18742, 15752
                                                  //| , 13550, 13507, 15778, 14432, 14946, 20346, 22876, 20133, 36482, 38174, 4485
                                                  //| 0, 43568, 15427, 33709, 21452, 15151, 15760, 17162, 15312, 15804, 15689, 184
                                                  //| 42, 21824, 20180, 52256, 21251, 25821, 20063, 21738, 21216, 22724, 22158, 21
                                                  //| 600, 23553, 21173, 22279, 24137, 22900, 28356, 29498, 23706, 24079, 22405, 1
                                                  //| 7865, 25338, 73386, 29509, 27065, 21075, 21640, 23718, 25329, 21067, 19336, 
                                                  //| 28473, 22464, 22319, 40900, 20854, 75572, 32516, 69948, 33810, 37273, 32221,
                                                  //|  32406, 34505, 39509, 54000, 35763, 60718, 25218, 52753, 42603, 35624, 37036
                                                  //| , 18515, 65067, 31793, 32774, 33651, 25480, 17894, 14954, 15159, 34912, 1598
                                                  //| 0, 14530, 19993, 44601, 16160)
 	
 	
  val file = new File("/Users/rohith/scala-eclipse-work/19AIE212-DAA/ShellSort.csv")
                                                  //> file  : java.io.File = /Users/rohith/scala-eclipse-work/19AIE212-DAA/ShellSo
                                                  //| rt.csv
	val bw = new BufferedWriter(new FileWriter(file))
                                                  //> bw  : java.io.BufferedWriter = java.io.BufferedWriter@4e04a765
	for(n<-1 until time.length){
		bw.write(time(n).toString+"\n")
	}
	bw.close()

}