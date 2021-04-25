object ShellSort {
	
	var x = Array(2,3,1,5,9,4)                //> x  : Array[Int] = Array(2, 3, 1, 5, 9, 4)
	
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
ShellSort(x)
x.foreach{println}                                //> 1
                                                  //| 2
                                                  //| 3
                                                  //| 4
                                                  //| 5
                                                  //| 9
}