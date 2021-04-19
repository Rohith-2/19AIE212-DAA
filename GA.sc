object GA {
  // best fit target solution
  val target = "rohith ramakrishnan"              //> target  : String = rohith ramakrishnan
  val len = target.length                         //> len  : Int = 19

  //Step 0 : Set Genes and representation
  val alphabet = "abcdefghijklmnopqrstuvwxyz ".toCharArray
                                                  //> alphabet  : Array[Char] = Array(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                  //|  p, q, r, s, t, u, v, w, x, y, z,  )

  def r = scala.util.Random.nextInt(alphabet.length)
                                                  //> r: => Int
	//Step 2 : Fitness Function
  def fitness(indi: String) = {
    val cntArray = for (i <- 0 until len) yield (if (target(i) == indi(i)) 1 else 0)
    cntArray.reduce(_ + _) / target.length.toFloat * 100
  }                                               //> fitness: (indi: String)Float

  // Generating random index over pGood
  def rt = scala.util.Random.nextInt(len)         //> rt: => Int
  def die = scala.util.Random.nextInt(6)          //> die: => Int

  def breed(p1: String, p2: String) = {
    val cPoint = rt

    // Crossover...
    var child1 = p1.substring(0, cPoint) + p2.substring(cPoint)
    var child2 = p2.substring(0, cPoint) + p1.substring(cPoint)

    // Mutation... probabilistic
    die match {
      case 1 => (child1.replace(child1.charAt(rt), alphabet(r)), child2)
      case 2 => (child1, child2.replace(child2.charAt(rt), alphabet(r)))
      case _ => (child1, child2)
    }

  }                                               //> breed: (p1: String, p2: String)(String, String)
	
	
  // Step : 1 - Generate random population
  var p = (0 to 1000).map { i =>
    var indi = ""
    (for (j <- 0 until len) yield alphabet(r).toString).reduce(_ + _)
  }.map(indi => (fitness(indi), indi))            //> p  : scala.collection.immutable.IndexedSeq[(Float, String)] = Vector((5.263
                                                  //| 158," bozxwhyadlokpbzmmr"), (5.263158," fwstiyzxbvlvhbgeey"), (10.526316,ez
                                                  //| hipjmsjlpuhqpoxqp), (0.0,agojntt udqdgqfkdeu), (5.263158,ysnjsmnj qurmasvkf
                                                  //| d), (0.0,qftvhjpugkld xhapmg), (10.526316,ktpia uxmgdejlskvrv), (0.0,"qwwo 
                                                  //| rgenddenak dz "), (0.0,bznyuln mpbfaulmsei), (10.526316,fjhgjwkh cxxttnkiai
                                                  //| ), (0.0,xjial okyidpzndqare), (5.263158,yqrbwugghuyhrounwhi), (0.0,jeigo vb
                                                  //| mzcpty pstr), (10.526316,bcndylgkxkarxiklewa), (5.263158,pnyqofhssynyvpsprd
                                                  //| c), (10.526316,qlbkirgqdfaohkg nsi), (0.0,uuvdholkhxswdclmbcw), (10.526316,
                                                  //| vojgraxaddbfbgcoqaq), (5.263158,tmovpjdre ofbtwrysc), (0.0,lybehtmddwefwyzz
                                                  //| l p), (0.0,csnyjcrtkfemhltiowb), (0.0,slljqnfpwaseeotybta), (0.0,oimdrtubgh
                                                  //| vfwq vefj), (0.0,f cnfnrdznpxvvfs uz), (5.263158,vadhizhkjexznyzhyfb), (0.0
                                                  //| ," vfjxiwzwxpb fqapdd"), (10.526316,mrvavuutgoygdis m a), (0.0,dxagntktzazc
                                                  //| qcpecgp), (5.263158,wpd
                                                  //| Output exceeds cutoff limit.

  var avgFit = p.map(_._1).reduce(_ + _) / p.length.toFloat
                                                  //> avgFit  : Float = 3.8382797

  def select = {

    // Selection
    val pGood = p.filter(_._1 > (avgFit + 1))
    def rg = scala.util.Random.nextInt(pGood.length)

    // New p
    p = (0 to 500).flatMap { j =>
      val (c1, c2) = breed(pGood(rg)._2, pGood(rg)._2)
      List((fitness(c1), c1), (fitness(c2), c2))
    }

    avgFit = p.map(_._1).reduce(_ + _) / p.length.toFloat
    avgFit

  }                                               //> select: => Float

  var i = 0;                                      //> i  : Int = 0

  while (avgFit < 95) {
    i += 1
    println(i + " " + select)
  }                                               //> 1 7.380007
                                                  //| 2 11.813196
                                                  //| 3 16.514277
                                                  //| 4 22.166183
                                                  //| 5 27.434574
                                                  //| 6 33.301685
                                                  //| 7 38.92217
                                                  //| 8 43.696884
                                                  //| 9 49.091278
                                                  //| 10 54.391174
                                                  //| 11 59.748787
                                                  //| 12 64.549736
                                                  //| 13 69.718605
                                                  //| 14 74.62473
                                                  //| 15 79.42557
                                                  //| 16 84.452484
                                                  //| 17 88.91196
                                                  //| 18 93.801796
                                                  //| 19 97.92524

  p.filter(t => t._2 == target).length            //> res0: Int = 828

}