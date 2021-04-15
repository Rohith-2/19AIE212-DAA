object Dynamic_Fibonacci {
  
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
  
  fibM(43)                                        //> res0: BigInt = 433494437
  
  //Bottom Up -- DP
 var fibMap:Map[Int,BigInt] = Map(0->0,1->1)      //> fibMap  : Map[Int,BigInt] = Map(0 -> 0, 1 -> 1)
 def fibBU(n:Int)={
 	var k = fibMap.size
 	while(k<=n){
 		val fibk = fibMap(k-1)+fibMap(k-2)
 		fibMap += (k->fibk)
 		k += 1
 	}
 	fibMap(n)
 }                                                //> fibBU: (n: Int)BigInt

fibBU(43)                                         //> res1: BigInt = 433494437
	
  
}