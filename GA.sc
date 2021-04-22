object GA1 {
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
  }.map(indi => (fitness(indi), indi))            //> p  : scala.collection.immutable.IndexedSeq[(Float, String)] = Vector((10.52
                                                  //| 6316,rynzcylnkiqorstyrui), (0.0,tsyxxdzxj cvujowlli), (0.0,hrdvbcfa ymspswo
                                                  //| zmz), (0.0,yrnjachirnmtivyluqp), (5.263158,pgscgcjyvsekhwu rfd), (0.0,dgfck
                                                  //| btqkvwvkhcrexy), (5.263158,mbmuahnhtlpptkvthdj), (5.263158,wsiptbchjfbigko 
                                                  //| zgu), (0.0,qqssmemllvk ywqrcnm), (0.0,"xatvizqnledassdyos "), (0.0," jatsbu
                                                  //| hrtql obbymb"), (0.0,vioyapkb ubdkqicl v), (0.0,nmwgnxckkgvywbmgfdx), (0.0,
                                                  //| qdkgbgzvjangdttopwd), (0.0,apatgpaxkoplxv bhxo), (5.263158,yotelyxvllbpunkc
                                                  //| fnj), (5.263158,drajvvpgotawmpjiequ), (10.526316,vbsmnv jcijpkztmnpo), (5.2
                                                  //| 63158,toebgzwexffwdnpblpt), (5.263158,xpexefgpim cmvqsywp), (0.0,xcjtajluhc
                                                  //| xvov plpt), (5.263158,cqwhlawsaxofmyvcold), (10.526316,aorctywqsslvn nxuhl)
                                                  //| , (0.0,zdjaz vdinvgomoxdlc), (0.0,bmlr ttteefgxzyrovd), (5.263158,fwoakzcxn
                                                  //| qetrajzisk), (0.0,llrdvjhwevgyblvxpgq), (0.0,ypzrzwqiroxokzuvfhd), (0.0,aao
                                                  //| cqmmsqfnruwmsksc), (0.0
                                                  //| Output exceeds cutoff limit.

  var avgFit = p.map(_._1).reduce(_ + _) / p.length.toFloat
                                                  //> avgFit  : Float = 3.5490944

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
  }                                               //> 1 7.2697
                                                  //| 2 11.655621
                                                  //| 3 17.102564
                                                  //| 4 21.97183
                                                  //| 5 27.382067
                                                  //| 6 32.897243
                                                  //| 7 38.312855
                                                  //| 8 43.85969
                                                  //| 9 49.07553
                                                  //| 10 54.291393
                                                  //| 11 59.775047
                                                  //| 12 64.20834
                                                  //| 13 69.36666
                                                  //| 14 73.98912
                                                  //| 15 78.805824
                                                  //| 16 83.9798
                                                  //| 17 89.25341
                                                  //| 18 93.738785
                                                  //| 19 98.3612

  p.filter(t => t._2 == target).length            //> res0: Int = 852

}