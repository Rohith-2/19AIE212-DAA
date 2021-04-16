object GA {
  // best fit target solution
  val target = "rohith ramakrishnan"              //> target  : String = rohith ramakrishnan
  val len = target.length                         //> len  : Int = 19

  val alphabet = "abcdefghijklmnopqrstuvwxyz ".toCharArray
                                                  //> alphabet  : Array[Char] = Array(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                  //|  p, q, r, s, t, u, v, w, x, y, z,  )

  def r = scala.util.Random.nextInt(alphabet.length)
                                                  //> r: => Int

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

  // Generate random population
  var p = (0 to 1000).map { i =>
    var indi = ""
    (for (j <- 0 until len) yield alphabet(r).toString).reduce(_ + _)
  }.map(indi => (fitness(indi), indi))            //> p  : scala.collection.immutable.IndexedSeq[(Float, String)] = Vector((0.0,"
                                                  //|  lnk cwojxgzuyppobf"), (5.263158,xjkrbwjdppov iopcui), (0.0,"djxauesfhjrig 
                                                  //| zzyx "), (0.0,sagbn lcwdoqmlomkhd), (0.0,hlbbomeldww   hpdsg), (5.263158,qu
                                                  //| imxrbykw iumqunjw), (0.0,swfgmlxqbajz lhrgxw), (0.0,azwhgexleetdlco dfm), (
                                                  //| 5.263158,wrrcmraillxaitthrym), (5.263158,nepqrwluwmutohkfvlo), (0.0,v rjyeg
                                                  //| y qwqlpybjhu), (0.0,"  s wnp fvqchpjjzgz"), (0.0,jtkfscolbaldudbfwzd), (0.0
                                                  //| ,syfxpmpystbfvzeaird), (0.0,iddtrfmvqzzmeekgtji), (0.0,btkacpqqcybgs mxfbl)
                                                  //| , (5.263158," pwlaqasankqnntnh s"), (5.263158,ufkywclrrwkcaeijbfq), (0.0,me
                                                  //| oaispduxyaqlptahe), (0.0,hieasonxkpbojrzprrc), (10.526316,befutnymacecvhnxz
                                                  //| mw), (5.263158,"kxsykipahgcymilfgm "), (0.0,"tumbmqcznxeqvrtwwd "), (0.0,af
                                                  //| edpvuqzjztoc zv q), (0.0,tnbxcviwnlhykrxw ms), (0.0,uemmmzetvpoinz ylsk), (
                                                  //| 5.263158,tjxx cwsuk qkkgyqao), (15.789473,gkuujn yqzfkqnsgtwj), (5.263158,"
                                                  //| iqqmsmqzeqe krrhpx "), 
                                                  //| Output exceeds cutoff limit.

  var avgFit = p.map(_._1).reduce(_ + _) / p.length.toFloat
                                                  //> avgFit  : Float = 3.5753849

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
  }                                               //> 1 6.8337283
                                                  //| 2 11.198638
                                                  //| 3 16.887201
                                                  //| 4 22.329008
                                                  //| 5 27.686687
                                                  //| 6 32.860443
                                                  //| 7 37.95566
                                                  //| 8 43.035053
                                                  //| 9 48.20882
                                                  //| 10 53.36167
                                                  //| 11 58.687737
                                                  //| 12 63.75659
                                                  //| 13 68.75209
                                                  //| 14 73.75274
                                                  //| 15 79.25235
                                                  //| 16 83.96928
                                                  //| 17 88.6441
                                                  //| 18 93.76507
                                                  //| 19 98.2929

  p.filter(t => t._2 == target).length            //> res0: Int = 844

}