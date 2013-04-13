object session {
  type Set = Int => Boolean

  def singletonSet(elem: Int): Set = Set(elem)

  def union(s: Set, t: Set): Set = (i: Int) => s(i) || t(i)

  def a = singletonSet(1)
  def b = singletonSet(1)

  def filter(s: Set, p: Int => Boolean): Set = {
    (x: Int) => s(x) && p(x)
  }
  val bound = 1000
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound || a > bound) true
      else if ( contains(s,a)) p(a) && iter(a+1)
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean =  !forall(s, x => !p(x))


  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def loop(a: Int, newSet: Set): Set= {
      if(a < -bound || a > bound) newSet
      else if (contains(s,a)) loop(a+1,union(newSet,Set(f(a))))
      else loop(a+1,newSet)
    }
    loop(-bound, Set())
  }
  def makeString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(makeString(s))
  }
  exists(union(singletonSet(1), singletonSet(2)), x => x == 1 )
  forall(union(singletonSet(1), singletonSet(1)), x => x == 1 )

  printSet(map(union(singletonSet(1), singletonSet(2)),x => x+1))

  val result = false && false



}
