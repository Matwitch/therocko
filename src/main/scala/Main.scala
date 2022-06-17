package nazva


case class Bag[A] private[Bag] (val map: Map[A, Int]):
  override def toString(): String =
  {
    this.map.toString();
  } 

  def size(): Int = 
    this.map.foldLeft(0)(_+_._2)

object Bag:
  
  def apply[A](xs: A*): Bag[A] =
    Bag[A](Seq(xs*).groupBy(identity).mapValues(_.size).toMap);

  def add[A](b: Bag[A], xs: A*): Bag[A] =
    val temp_m = Seq(xs*).groupBy(identity).mapValues(_.size).toMap;
    val mergedList  = (b.map.toList) ++ (temp_m.toList); 
    Bag[A](mergedList.groupBy(_._1).map{case (k,v) => k -> v.map(_._2).sum});

  def remove[A](b: Bag[A], as: A): Bag[A] =
    val mergedList = (b.map.toList) ++ (Map(as -> -1).toList); 
    val mx = mergedList.groupBy(_._1).map{case (k,v) => k -> v.map(_._2).sum};

    mx(as) match 
      case 0 => Bag(mx - as)
      case _ => Bag(mx)

  def toSet[A](b: Bag[A]): Set[A] =
    b.map.keySet;

  
  def toList[A](b: Bag[A]): List[A] =
    def run[A](result: List[A], xs: Bag[A]): List[A] = 
      if (xs == Bag()) then result;
      else
        run((xs.map.keySet.head)::result, Bag.remove(xs, xs.map.keySet.head));

    run(List[A](), b); 


@main def hello: Unit = 
  { 
    
  }
 
  