import util.Random

object insert {

 def Sort(l:Array[Int]) = {
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
  }                                               //> insertSort: (l: Array[Int])Unit
/*  
var x = Array.fill(10)(1000).map(Random.nextInt)  //> x  : Array[Int] = Array(68, 53, 29, 76, 754, 853, 375, 555, 120, 595)
Sort(x)
x                                                 //> res0: Array[Int] = Array(29, 53, 68, 76, 120, 375, 555, 595, 754, 853)
*/
}