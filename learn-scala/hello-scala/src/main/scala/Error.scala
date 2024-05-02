import List.apply
enum Option[+A]:
    case Some(get:A)
    case None

    def map[B](f: A => B): Option[B]= this match
        case Some(get) => Some(f(get)) 
        case None => None
    
    def flatMap[B](f: A => Option[B]): Option[B]= this match
        case Some(get) => f(get)
        case None => None
    
    def flatMap2[B](f: A => Option[B]): Option[B] =
        this.map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match
        case Some(get) => get
        case None => default
    
    def orElse[B >: A](ob: => Option[B]): Option[B] =
        this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
        this.flatMap(x => if f(x) then Some(x) else None)

    def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then Option.None
    else Option.Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa,bb)))

def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case List.Nil => Option.Some(List.Nil)
    case List.Cons(head, tail) => map2(head,sequence(tail))(List.Cons(_,_))

def sequence2[A](as: List[Option[A]]): Option[List[A]] = as match
    case List.Nil => Option.Some(List.Nil)
    case List.Cons(head, tail) => head.flatMap(hh => sequence(tail).map(List.Cons(hh,_)))

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence2(List.map(as,f))

def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldLeft(as,Option.Some(List.Nil),(acc,x) => map2(f(x),acc)(List.Cons(_,_)))

def sequence3[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(x => x)

enum Either[+E,+A]:
    case Left(value: E)
    case Right(value: A)

    def map[B] (f: A => B): Either[E,B] = this match
        case Left(value) => Left(value)
        case Right(value) => Right(f(value))
    
        
@main def error(): Unit =
    val ex1: Option[Int] = Option.Some(4)
    val ex2: Option[Int] = Option.None
    val ex3: List[Option[Int]] = apply(Option.Some(1), Option.Some(2), Option.Some(3))
    val ex4: List[Int] = apply(1,2,3,4)
    val ex5: Either[String, Int] = Either.Right(2)
    val ex6: Either[String,Int] = Either.Left("not valid")
    // println(Option.Some(2).map(x => x * x))
    // println(Option.Some(2).getOrElse(0))
    // println(Option.None.getOrElse(0))
    // println(Option.Some(4).flatMap(x => Option.Some(x*x)))
    // println(Option.Some(4).flatMap(x => Option.None))
    // println(Option.None.flatMap(x => Option.Some(x*x)))
    // println(Option.None.flatMap(x => Option.Some(x*x)))
    // println(ex1.getOrElse(0))
    // println(ex2.getOrElse(0))
    // println(ex1.orElse(Option.Some(0)))
    // println(ex2.orElse(Option.Some(0)))
    // println(sequence(ex3))
    // println(sequence2(ex3))
    // println(sequence3(ex3))
    // println(traverse(ex4)(x => Option.Some(x)))
    // println(traverse2(ex4)(x => Option.Some(x)))
    // println(ex5.map(x => x * x))
    // println(ex6.map(x => x * x))
    
