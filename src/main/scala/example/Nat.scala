package example

import scala.annotation.tailrec

sealed trait List [+A]
case object Nil extends List[Nothing]
case class Const[+A](h:A,t:List[A]) extends List[A]

object List extends App {

  // A* seq [A]
  def const[A](h:A, t:List[A]):List[A] = Const(h,t)

  def apply[A](as: A*) : List[A] ={
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  /*
  Tema 2 Construcción de listas
   */
  /*
  Ejercicio 1 Implemente la función take que se encarga de tomar dos parámetros.
  El primero un valor entero positivo n y el segundo una lista de valores de
  cualquier tiempo. Y esta función se encarga de tomar los n primeros valores, si
  existen de la lista
   */

  def take[A](n:Int,lst:List[A]):List[A] = (n,lst) match {
    case (0,_) => Nil
    case (n,Nil) => Nil
    case (n,Const(h,t)) => Const(h,take(n-1,t))
  }

  /*
  Ejercicio 2. Implemente la función init que tiene toma una lista y toma los valores
  iniciales excepto el último.
   */

  def init[A](lst:List[A]):List[A] = lst match {
    case Const(h,Nil) => Nil
    case Const(h,t) => Const(h,init(t))
  }

  /*
  Ejercicio 3. Implemente la función split, recibe dos parámetros n y una lista;
divide la primera lista en n elementos y los restantes quedan en la segunda lista.
   */

  def split[A](pos:Int,lst:List[A]):(List[A],List[A]) = {
    @tailrec
    def splitAux[A](pos:Int,lst:List[A],lst2:List[A]):(List[A],List[A]) = (pos,lst) match {
      case (a,Nil) => (lst2,Nil)
      case (0,lst) => (lst2,lst)
      case (a,Const(h,t)) => splitAux(a-1,t,addEnd(h,lst2))
    }
    splitAux(pos,lst,Nil)
  }

  /*
  Ejercicio 4. Implemente la función zip esta función fusiona dos listas de tipos diferentes
  en una lista de pares del mismo tamaño. La siguiente es la firma de la función:
   */

  def zip[A,B](lst1:List[A],lst2:List[B]):List[(A,B)] = (lst1,lst2) match {
    case (Const(h,t),Nil) => Nil
    case (Nil,Const(h,t)) => Nil
    case (Const(h,t),Const(h2,t2)) => Const((h,h2),zip(t,t2))
  }

  /*
  Ejercicio 5. Implemente la función unzip esta lista separa una lista de tuplas
  en dos listas distintas.
   */

  def addEnd[A](h:A, t:List[A]):List[A] = t match {
    case Const(h1,t2) => Const(h1,addEnd(h,t2))
    case Nil => Const(h,Nil)
  }

  def unzip[A,B](lst:List[(A,B)]):(List[A],List[B]) = {
    @tailrec
    def unzipRec[A,B](lst:List[(A,B)],l1:List[A],l2:List[B]):(List[A],List[B]) = lst match {
      case Nil => (l1,l2)
      case Const(h,t) => unzipRec(t,addEnd(h._1,l1),addEnd(h._2,l2))
    }
    unzipRec(lst,Nil,Nil)
  }

  /*
  Ejercicio 6. Implemente la función reverse. Toma una lista y devuelve una
  versión invertida de la misma.
   */
  def reverse[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case Const(h,t) => addEnd(h,reverse(t))
  }
  //println(reverse(Const(3,Const(2,Const(1,Nil)))))
  /*
  Ejercicio 7. Implemente la función intersperse. Esta se encarga de entremezclar
  un valor entre los elementos originales de la lista.
   */
  def intersperse[A](elem:A,lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case Const(h,Nil) => Const(h,Nil)
    case Const(h,t) => Const(h,Const(elem,intersperse(elem,t)))
  }

  /*
  Ejercicio 8. Implemente la función concat. Es función recibe una lista de lista
  valores de un tipo A y la transforma en una lista de valores de tipo A.
   */

  def append[A](lst1:List[A],lst2:List[A]):List[A] = (lst1, lst2) match {
    case (Nil,Nil) => Nil
    case (l1,Nil) => l1
    case (Nil,l2) => l2
    case (Const(h,t),l2) => Const(h,append(t,l2))
  }

  def concat[A](lst:List[List[A]]):List[A] = lst match {
    case Nil => Nil
    case Const(h,t) => append(h,concat(t))
  }

  val l1 = List(1,2,3,4,5,6,7,8)
  val ll = List(4L,5L,6L,7L,8L)
  val lmm = List(4.7,5.1,6.2,7.4,8.7)
  val lb = List(true,false)

}