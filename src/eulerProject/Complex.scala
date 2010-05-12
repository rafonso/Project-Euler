package eulerProject

case class Complex(re: Long, im: Long) {
  
  def this(re: Long) = this(re, 0)
  
  def +(other: Complex) = Complex(this.re + other.re, this.im + other.im)
  
  def -(other: Complex) = Complex(this.re - other.re, this.im - other.im)
  
  def *(other: Complex) = Complex((this.re * other.re - this.im * other.im), (this.re * other.im + this.im * other.re))
  
  
  
  def conjugate = Complex(re, -im)
  
  def module = Math.sqrt(this.re * this.re + this.im * this.im)
  
  override def toString = (re + " + " + im + "i")
}

object Complex {
  
  val I = Complex(0, 1)
  
//   def implcit realToComplex(re: Long): Complex = Complex(re, 0)
  
}