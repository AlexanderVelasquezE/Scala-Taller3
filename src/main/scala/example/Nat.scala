package example

import scala.annotation.tailrec

sealed trait Nat
case object Cero extends Nat
case class Suc(nat:Nat) extends Nat

object Nat extends App {

  /*
  Tema 3 Naturales
   */
  /*
  Ejercicio 9. Implemente la función addNat. Esta función recibe dos naturales y
  se encarga de sumarlos produciendo un valor correcto
   */

  def addNat(nat1:Nat,nat2:Nat):Nat = nat1 match {
    case Cero => nat2
    case Suc(nat) => addNat(nat,Suc(nat2))
  }
  /*
  println(addNat(Cero,Suc(Cero)))
  println(addNat(Suc(Suc(Cero)),Suc(Cero)))
  println(addNat(Suc(Suc(Cero)),Suc(Suc(Cero))))
*/
  /*
    Ejercicio 10. Implemente la función prodNat. Esta función realiza la multipli-
  cación de dos valores naturales.
   */

  def prodNat(nat1:Nat,nat2:Nat):Nat = {
    if (nat1 == Cero || nat2 == Cero) {
      return Cero
    }
    else if(nat1 == Suc(Cero)){
      return nat2
    }
    else if(nat2 == Suc(Cero)){
      return nat1
    }
    else{
      @tailrec
      def prodNatR(nat1:Nat,nat2:Nat, result:Nat):Nat = nat1 match {
        case Cero => result
        case Suc(n) => prodNatR(n,nat2,addNat(result,nat2))
      }
      prodNatR(nat1,nat2,Cero)
    }
  }
  /*
  println(prodNat(Cero,Suc(Cero)))
  println(prodNat(Suc(Suc(Cero)),Suc(Cero)))
  println(prodNat(Suc(Suc(Suc(Cero))),Suc(Suc(Cero))))
  println(prodNat(Suc(Suc(Suc(Suc(Suc(Cero))))),Suc(Suc(Suc(Suc(Cero))))))
  */

}