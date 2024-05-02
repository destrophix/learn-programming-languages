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
import List.addOne
import List.convertDoubleToStr
import List.map
import List.filter
import List.filterUsingFlatMap
import List.addTwoLists
import List.matchList
import List.hasSubsequence
import Tree.fold
import Tree.mapViaFold
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def head[A] (ls: List[A]):  A = ls match
    case Nil => sys.error("message")
    case Cons(x,xs) => x

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

  def addOne(ls: List[Int]): List[Int] =
    foldRight(ls,Nil,(x,acc) => Cons(x+1,acc))

  def convertDoubleToStr(ls: List[Double]): List[String] =
    foldRight(ls, Nil, (x, acc) => Cons(x.toString(),acc))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as,Nil,(x,acc) => Cons(f(x),acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil,(x,acc) => if(f(x)) Cons(x,acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil, (x,acc) => append(f(x),acc))

  def filterUsingFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as,x => if(f(x)) apply(x) else Nil)
  
  def addTwoLists[A, B, C](a1: List[A], a2: List[B], f: (A,B) => C): List[C] =
    if(a1 == Nil) return Nil
    Cons(f(head(a1),head(a2)),addTwoLists(tail(a1),tail(a2),f))

  def matchList[A](a1: List[A], a2: List[A]): Boolean = (a1,a2) match
    case (Nil,_) => true
    case (_,Nil) => false
    case (Cons(x,xs),Cons(y,ys)) if x==y => matchList(xs,ys)
    case _ => false

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match
    case (_,Nil) => true
    case (Nil,_) => false
    case (_,_) if matchList(sub,sup) => true
    case (Cons(x,xs), a) => hasSubsequence(xs,a)

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(value) => 1
    case Branch(left, right) => 1 + left.size + right.size

  def depth: Int = this match
    case Leaf(value) => 1
    case Branch(left, right) =>1+ left.depth.max(right.depth)
  

object Tree:
  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(value) => value
    case Branch(left, right) => left.maximum.max(right.maximum)
  
  def map[A,B] (t: Tree[A], f: A => B): Tree[B] = t match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left,f),map(right,f))

  def fold[A,B] (t: Tree[A],g: A => B, f: (B,B) => B): B = t match
    case Leaf(value) => g(value)
    case Branch(left, right) => f(fold(left,g,f), fold(right,g,f)) 
  
  def mapViaFold[A,B] (t: Tree[A], g: A => B): Tree[B] =
    fold(t,x => Leaf(g(x)),(l,r) => Branch(l,r))
    

@main def hello(): Unit =
  val ex1: List[Double] = List.Nil
  val ex2: List[Int] = List.Cons(1,List.Nil)
  val ex3: List[Int] = apply(1,2,3,4,5)
  val ex4: List[List[Int]] = apply(apply(1,2),apply(3,4),apply(5,6))
  val ex5: List[Double] = apply(1.0,2.0,3.0,4.5)
  val ex6: List[Int] = apply(2,4,9,16,25)
  val ex7: Tree[Int] = Tree.Branch(Tree.Branch(Tree.Leaf(1),Tree.Leaf(2)), Tree.Branch(Tree.Leaf(3),Tree.Leaf(4)))
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
  // println(flatMap(ex4))
  // println(addOne(ex3))
  // println(convertDoubleToStr(ex5))
  // println(map(ex3, x => x*x))
  // println(filterUsingFlatMap(ex3,x => x%2==0))
  // println(flatMap(ex3, x => apply(x,x)))
  // println(addTwoLists(ex3,ex6,_+_))
  // println(hasSubsequence(apply(1,2,3),apply(2,4)))
  // println(ex7.maximum)
  // println(ex7.depth)
  // println(Tree.map(ex7,x => x*x))
  // println(Tree.fold(ex7,x => x, (x,y)=> x+y)) // sum
  // println(fold(ex7,x => 1,(x,y) => 1 + x +y)) // size
  // println(fold(ex7,x => x, (x, y) => x.max(y))) // maximum
  // println(fold(ex7,x => 1, (x, y) => 1 +x.max(y))) // depth
  println(mapViaFold(ex7,x => x*x)) // map
