package nazva
import java.{util => ju}



case class Bag[A] private[Bag] (val map: Map[A, Int]):
  override def toString(): String =
  {
    this.map.toString();
  } 


object Bag:
  private def run[A](result: Map[A, Int], xs: Seq[A]): Map[A, Int] = 
          var temp: Map[A, Int] = result;
          xs match
            case Seq(a, b*) => 
                val t: Option[Int] = result.get(a);
                t match
                  case None => 
                    temp += (a -> 1);
                  case _ =>
                    temp -= a;
                    temp += (a -> (t.get + 1));
                    
                run(temp, b);
            case _ => result;

  def apply[A](xs: A*): Bag[A] = 
      val m : Map[A, Int] = run(Map.empty[A, Int], xs);
      Bag[A](m);


  def add[A](b: Bag[A], as: A*): Bag[A] =
    val m : Map[A, Int] = run(b.map, as);
    Bag[A](m);

  def remove[A](b: Bag[A], as: A): Bag[A] =
    var temp = b.map
    val t: Option[Int] = temp.get(as);
    t match
      case None => b;
      case _ =>
        temp -= as;
        if t.get > 1 then
          temp += (as -> (t.get - 1));

        Bag[A](temp);

  def toSet[A](b: Bag[A]): Set[A] =
    b.map.keySet;

  
  def toList[A](b: Bag[A]): List[A] =
    def run[A](result: List[A], xs: Bag[A]): List[A] = 
      if (xs == Bag()) then result;
      else
        run((xs.map.keySet.head)::result, Bag.remove(xs, xs.map.keySet.head));

    run(List[A](), b); 


@main def hello: Unit = 
  var t = Bag('a', 'a', 'b', 'c', 'c', 'c', '2')
  println(Bag.toSet(t));
  println(Bag.toList(t));
  println(t);
  t = Bag.add(t, 'f');
  println(t);
  t = Bag.remove(t, 'f');
  println(t);
  val d = Bag();
  println(d);

  println(List(1,4,3,10,0).sorted)
 
