object GA {
  val target = "amrita"                           //> target  : String = amrita
  val len = target.length                         //> len  : Int = 6
  
  val alphabet = "artmiejoukc".toCharArray        //> alphabet  : Array[Char] = Array(a, r, t, m, i, e, j, o, u, k, c)
  
  def r = scala.util.Random.nextInt(alphabet.length)
                                                  //> r: => Int
  
  var p = (0 to 100).map{ i=>
  	val indi = ""
  	(for(i <- 0 until len) yield alphabet(r).toString).reduce(_+_)
  }                                               //> p  : scala.collection.immutable.IndexedSeq[String] = Vector(kijrki, kiruum, 
                                                  //| acrucm, mitjom, mjurau, mjirjj, eataro, ueitiu, ecjakj, oaokro, attkmr, rcee
                                                  //| ot, imcuuc, rkkojr, rcuiju, ctciok, kukrim, rjckom, iracij, oriuta, akeuke, 
                                                  //| rkcuct, uciicr, rrrama, utammt, uracro, tuuaro, cumttt, ameikj, mmmait, eccr
                                                  //| ru, eekkrc, aaiomo, iiiacc, aueaur, ktrurr, cjiakr, jratoe, mratii, maajjr, 
                                                  //| irteer, jkiarm, oatajr, jatijc, aumcea, tmikma, majcrc, ckuecc, jiktao, cojk
                                                  //| ut, ajrrie, jrrarj, kiiuui, tiiuea, rujioc, ojimet, ekjjtc, cekret, raeotu, 
                                                  //| urtiat, ckkajc, jrcrur, tajumi, ceceru, recrjt, oictre, ateirr, eeutje, icao
                                                  //| oe, cumkkm, etetiu, rmcock, urciuj, ueuiuc, rjeter, joiacu, cjaako, eeuaee, 
                                                  //| reieor, oorjaa, uoeuec, aamatj, oojkjt, juiceu, toueuu, arecrt, eckcoe, iojt
                                                  //| cu, kamcau, ujkrar, rraoij, iaorum, eccitk, ocjoer, jaeoui, aorecr, reimru, 
                                                  //| eucuem, ujoaku, mjoitt, tkcjca)
  def fitness(indi:String) = {
  	  	val cntArray = for(i <- 0 until len)
  	  		 yield (if(target(i)==indi(i)) 1 else 0 )
  	  	cntArray.reduce(_+_)
  }                                               //> fitness: (indi: String)Int
  
  p.map(indi => (fitness(indi),indi)).sortBy(_._1).reverse.filter(_._1>=2)
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, String)] = Vector((3,ameik
                                                  //| j), (2,mjoitt), (2,aorecr), (2,eccitk), (2,aamatj), (2,oorjaa), (2,ateirr), 
                                                  //| (2,ajrrie), (2,tmikma), (2,aumcea), (2,rrrama), (2,oriuta), (2,acrucm))
  }