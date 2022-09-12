//Q1 and Q2

case class RationalNumber(c: Int, d: Int){
    if(d == 0) throw new IllegalArgumentException("Denominator cannot be 0")
    
    def this(c: Int) = this(c, 1)

    private def GCD(a: Int, b: Int): Int = if (b == 0) a.abs else GCD(b, (a % b))
    
    private def gcd = GCD(d, c)
    private def numer = c / gcd
    private def denom = d / gcd
    
    def -(that: RationalNumber) = RationalNumber(numer * that.denom - that.numer * denom, denom * that.denom)
    override def toString = s"$numer/$denom"
    
}

object MyApp extends App{
        val x =  RationalNumber(3, 4)
        val y =  RationalNumber(5, 8)
        val z =  RationalNumber(2, 7)

        println(x - y - z)
}