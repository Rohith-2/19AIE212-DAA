import java.io._
object Exporting {
 def fib(n:Int):Int = if(n<=1)n else fib(n-1)+fib(n-2)
                                                  //> fib: (n: Int)Int
  
  //val memo = (0 to 100).toArray.map{x => -1}
	val memo = Array.fill[BigInt](100)(-1)    //> memo  : Array[BigInt] = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
                                                  //| , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
                                                  //| , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
                                                  //| , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
                                                  //| , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
                                                  //| , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
  memo(0) = 0
  memo(1) = 1
  
  def fibM(n:Int):BigInt = {
  	val f = memo(n)
  	if(f>=0) f
  	else {
  		memo(n) = fibM(n-1)+fibM(n-2)
  		memo(n)
  	}
  }                                               //> fibM: (n: Int)BigInt
                                                  //> Welcome to the Scala worksheet
   def timeit(n:Int):Array[Array[Long]]={
   var fibTime:Array[Long] = Array()
   var fibMTime:Array[Long] = Array()
   for(i <- 0 to n){
   
  	val t0 = System.nanoTime()
  	fib(i)
  	val t1 = System.nanoTime() - t0
  	fibTime = fibTime :+ t1
  	
  	val T0 = System.nanoTime()
  	fibM(i)
  	val T1 = System.nanoTime() - T0
  	fibMTime = fibMTime :+ T1
  }
  val Time =Array(fibTime,fibMTime)
  Time
  }                                               //> timeit: (n: Int)Array[Array[Long]]
  
  val time = timeit(25).transpose                 //> time  : Array[Array[Long]] = Array(Array(1153, 14400), Array(331, 2221), Arr
                                                  //| ay(300, 10664), Array(338, 10902), Array(523, 14431), Array(858, 5513), Arra
                                                  //| y(1547, 5387), Array(2419, 5346), Array(5695, 8862), Array(10213, 9414), Arr
                                                  //| ay(10827, 5403), Array(18945, 5095), Array(26330, 5010), Array(41963, 5020),
                                                  //|  Array(46196, 5374), Array(5992, 5666), Array(15335, 5552), Array(14907, 482
                                                  //| 7), Array(21634, 4610), Array(34199, 4576), Array(55247, 4602), Array(88856,
                                                  //|  4722), Array(143885, 4768), Array(212884, 5626), Array(157127, 4617), Array
                                                  //| (253719, 8190))
 
  val file = new File("/Users/rohith/scala-eclipse-work/19AIE212-DAA/Time.csv")
                                                  //> file  : java.io.File = /Users/rohith/scala-eclipse-work/19AIE212-DAA/Time.cs
                                                  //| v
	val bw = new BufferedWriter(new FileWriter(file))
                                                  //> bw  : java.io.BufferedWriter = java.io.BufferedWriter@5f2050f6
	for(n<-1 until time.length){
	bw.write(time(n)(0).toString+","+time(n)(1).toString+"\n")
	}
	bw.close()
}