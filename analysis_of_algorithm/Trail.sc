object Trail {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val a = Array(1,2,3,4,5,6)                      //> a  : Array[Int] = Array(1, 2, 3, 4, 5, 6)
  //Array.concat(a.slice(2,a.length),a.slice(0,2))
	a.takeRight(2)                            //> res0: Array[Int] = Array(5, 6)
	a                                         //> res1: Array[Int] = Array(1, 2, 3, 4, 5, 6)
}