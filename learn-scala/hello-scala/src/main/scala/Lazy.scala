import Option._
import LazyList.apply
import List.matchList
import LazyList.cons
import LazyList.empty

enum LazyList[+A]:
    case Empty
    case Cons(h: ()=> A, t: () => LazyList[A])

    def headOption: Option[A] = this match
        case Empty => None
        case Cons(h,_) => Some(h())

    def toList: List[A] = this match
        case Empty => List.Nil
        case Cons(h, t) => List.Cons(h(),t().toList)
    
    def take (n: Int): LazyList[A] = this match
        case Empty => Empty
        case Cons(h, t) if n > 0 => cons(h(),t().take(n-1))
        case _ => Empty 

    def drop(n: Int): LazyList[A] = this match
        case Empty => Empty
        case Cons(_, t) if n > 0 => t().drop(n-1)
        case ls => ls 

    def takeWhile(p: A => Boolean): LazyList[A] = this match
        case Empty => Empty
        case Cons(h, t) if(p(h())) => cons(h(),t().takeWhile(p))
        case _ => Empty
    
    def foldRight[B](acc: B)(f: (A,=> B) => B): B = this match
        case Cons(h, t) => f(h(),t().foldRight(acc)(f))
        case _ => acc
    
    def exists(f: A=>Boolean):Boolean =this.foldRight(false)((a, b) => f(a) || b)

    def forAll(f: A=>Boolean):Boolean = this.foldRight(true)((a,b) => f(a) && b)

    def takeWhile2(p: A=> Boolean): LazyList[A] = this.foldRight(Empty)((x,y) => if(p(x)) then cons(x,y) else Empty)

object LazyList:
    def cons[A](hd: =>A, tl: => LazyList[A]): LazyList[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(()=> head, () => tail)

    def empty[A]: LazyList[A] = Empty

    def apply[A](as: A*): LazyList[A] =
        if as.isEmpty then empty
        else cons(as.head,apply(as.tail*))

def if2[A](cond: Boolean, onTrue: =>A, onFalse: =>A): Int = if cond then 1 else 2

def fun[A] (a:A): A = {println("printing value");a}

enum Test[+A]:
    case Empty
    case Cons(h: () => A, t: Test[A])

    def headOption:Option[A] = this match
        case Empty => None
        case Cons(h, t) => Some(h())

    def tail:Test[A] = this match
        case Empty => Empty
        case Cons(h, t) => t
        

@main def lazyEval(): Unit =
    val ex1: LazyList[Int] = apply(1,2,3,4)
    val ex2: LazyList[Int] = LazyList.Cons(()=> {println("head1");1 },() => LazyList.Cons(()=>{println("head2");2},()=> LazyList.Empty))
    // println(ex2.tailOption)
    // println(ex1.toList)
    // println(ex1.take(3))
    // println(ex1.drop(2))
    // println(ex1.takeWhile(x => x%2==1).toList)
    // println(ex2.take(2))

    // if2(32 < 22,
    //     fun('a'),
    //     fun('b')
    // )
    // println(ex1)
    val ex3: Test[Int] = Test.Cons(()=>{println("value 1");1},Test.Cons(()=>{println("value 2");2},Test.Cons(()=>{println("value 3");3},Test.Empty)))
    // println(ex3.headOption)
    // println(ex3.tail)
    // println(ex1.exists((x) => x==33))
    // println(ex1.forAll(x => x<3))
    println(ex1.takeWhile2(x => x%2==1).toList)

