package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(value) => f(value)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int = fold(_ => 1, _ + _ + 1)
  
  def depthViaFold: Int = fold(_ => 0, _ max _ + 1)
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = ???

  extension (t: Tree[Int]) def maximum: Int = t match
      case Leaf(value)  => value
      case Branch(l, r) => l.maximum max r.maximum

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(identity, _ max _)

  @main def testTree: Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

    println(s"      tree: ${tree}")
    println(s"       map: ${tree.map(_ * 2)}")
    println(s"mapViaFold: ${tree.mapViaFold(_ * 2)}")
    println(s"       map: ${tree.map(_ + 3)}")
    println(s"mapViaFold: ${tree.mapViaFold(_ + 3)}")
    println(s"       map: ${tree.map(_ * 2).map(_ - 1)}")
    println(s"mapViaFold: ${tree.mapViaFold(_ * 2).map(_ - 1)}")
  }