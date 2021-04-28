import util.Random

object Shell {

  def Sort(a:Array[Int]){
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
}                                                 //> Sort: (a: Array[Int])Unit
var x = Array.fill(10)(1000).map(Random.nextInt)  //> x  : Array[Int] = Array(690, 992, 42, 593, 552, 919, 730, 634, 815, 823)
Sort(x)
x                                                 //> res0: Array[Int] = Array(42, 552, 634, 690, 690, 690, 690, 730, 815, 823)

}