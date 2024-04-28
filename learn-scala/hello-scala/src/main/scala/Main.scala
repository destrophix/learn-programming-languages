import List.apply
import List.tail
import List.setHead
import List.drop
import List.dropWhile
import List.sumViaFoldRight
import List.productViaFoldRight
import List.lengthViaFoldRight
import List.sumViaFoldLeft
import scala.annotation.tailrec
import List.reverseViaFoldLeft
import List.flatMap
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A] (ls: List[A]):  List[A] = ls match
    case Nil => sys.error("message")
    case Cons(x,xs) => xs
  
  def setHead[A] (ls: List[A], a: A): List[A] = ls match
    case Nil => Nil
    case Cons(x,xs) => Cons(a,xs)

  def drop[A] (ls: List[A], n: Int): List[A] = ls match
    case Nil => Nil
    case Cons(x,xs) if n > 0 => drop(xs,n-1)
    case ls => ls
  
  def dropWhile[A] (ls: List[A], fn: A => Boolean): List[A] = ls match
    case Nil => Nil
    case Cons(x,xs) if fn(x) => dropWhile(xs,fn)
    case ls => ls 

  def foldRight[A, B] (as: List[A], acc: B, f: (A, B) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs,acc,f))

  def foldLeft[A,B] (as: List[A], acc: B, f: (B,A) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs,f(acc,x), f)
  
  def sumViaFoldRight(ns: List[Int]): Int = foldRight(ns,0,(x,y) => x + y)
  
  def productViaFoldRight(ns: List[Int]): Int = foldRight(ns,1,(x,y) => {
    if(x==0){return 0}
    x * y
  })

  def lengthViaFoldRight(ns: List[Int]): Int = foldRight(ns,0, (x,len) => len+1)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns,0,_+_)

  def productViaFoldLeft(ns: List[Int]): Int = foldLeft(ns,1,_*_)

  def lengthViaFoldLeft(ns: List[Int]): Int = foldLeft(ns,0,(x,len) => len + 1)

  def reverseViaFoldLeft[A](ns: List[A]): List[A] = foldLeft(ns,Nil,(acc,x)=> Cons(x,acc))

  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    // foldLeft(reverseViaFoldLeft(l), acc, (b, a) => f(a, b))
    foldLeft(l, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)
  
  def foldLeftViaFoldRight[A, B](l: List[A], acc: B, f: (B,A) => B): B =
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)
  
  def append[A](a1: List[A], a2: List[A]): List[A] =
    // foldRight(a1, a2, (a, acc) => Cons(a,acc))  1,2 3,4
    foldLeft(reverseViaFoldLeft(a1),a2,(acc,a)=> Cons(a,acc))

  def flatMap[A](ls: List[List[A]]): List[A] =
    foldRight(ls,Nil,append)

  
@main def hello(): Unit =
  val ex1: List[Double] = List.Nil
  val ex2: List[Int] = List.Cons(1,List.Nil)
  val ex3: List[Int] = apply(1,2,3,4,5)
  val ex4: List[List[Int]] = apply(apply(1,2),apply(3,4),apply(5,6))
  // println(tail(ex3))
  // println(setHead(ex3,14))
  // println(drop(ex3,7))
  // println(dropWhile(ex3,x => x < 4))
  // println(sumViaFoldRight(ex3))
  // println(productViaFoldRight(ex3))
  // println(lengthViaFoldRight(ex3))
  // println(sumViaFoldLeft(ex3))
  // println(reverseViaFoldLeft(ex3))
  // println(ex4)
  println(flatMap(ex4))


