import java.io._
import util.Random

object ShellSort {

def shellSort(a:Array[Int]){
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
  }
		
val end = 100
val begn = 50
val inc = 10
val nVal = (begn to end by inc).toArray

def timer(fn:Array[Int]=>Unit,begn:Int,end:Int,inc:Int)={
var fibTime:Array[Long] = Array()


 for(i <- begn to end by inc) {
   var avgTime:Array[Long] = Array()
  	for (_ <-0 to 5) {
	    var x = Array.fill(i)(1000).map(Random.nextInt)
	  	val t0 = System.nanoTime()
	  	fn(x)
	  	val t1 = System.nanoTime() - t0
	  	avgTime = avgTime :+ t1
  	}
  	fibTime = fibTime :+ (avgTime.reduce(_+_)/avgTime.length)
  }
 fibTime
  }
  
   val time =  timer(shellSort,begn,end,inc)
   
   val time_1 =  timer(insertSort,begn,end,inc)
 	
 //	val theorVal = nVal.map(i=>i*scala.math.log10(i))
 	
  val file = new File("/Users/rohith/scala-eclipse-work/19AIE212-DAA/ShellvsInsert.csv")
	val bw = new BufferedWriter(new FileWriter(file))
	for(n<-1 until time.length){
		bw.write(nVal(n).toString+","+time(n).toString+","+time_1(n).toString+"\n")
	}
	bw.close()

}                                                 //> shellSort: (a: Array[Int])Unit
}