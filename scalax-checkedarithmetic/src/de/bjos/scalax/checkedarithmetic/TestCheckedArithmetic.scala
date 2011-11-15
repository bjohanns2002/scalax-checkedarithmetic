package de.bjos.scalax.checkedarithmetic
import scala.util.control.ControlThrowable

object TestCheckedArithmetic {

  private val checked = new CheckedArithmetic with ReportExceptor
  import checked._
  import TestAdaptor._

  private final val RINGBUFFERSIZE = 16
  
  class CalculationException(msg: String) extends Exception(msg)
  class BoundaryException(msg: String) extends Exception(msg)
  
  object ContinueException extends ControlThrowable

  trait TestAdaptor[T <: AnyVal] {
    
    val tracebuffer = new Array[String](RINGBUFFERSIZE)
    private var ringidx: Int = 0
    
    type ValType = T
    def getRandom: T
    def mulstep(a: T, b: T): T
    def addstep(a: T, b: T): T
    def divstep(a: T, b: T): T
    def substep(a: T, b: T): T
    
    def boundaryOverflow(t: BigInt): Boolean

    def step(x: String): Unit = {
      tracebuffer.synchronized {  
        ringidx = ((ringidx +1)%RINGBUFFERSIZE)
        tracebuffer(ringidx) = x
      }
    }
    
    def flush(): String = {
      var result: Seq[String] = null
      tracebuffer.synchronized {  
        result = for (i <- 1 to RINGBUFFERSIZE) yield {
          ringidx = (ringidx +1)%RINGBUFFERSIZE
          if (tracebuffer(ringidx) == null) "" else tracebuffer(ringidx)
        }
      }
      result.foldLeft("")((a,b) => { if (b.length > 0) a + "\n" + b else a })
    }
    
    def checkStep(a: ValType, b: ValType, f:(ValType, ValType) => ValType, operator: String, ref: BigInt): ValType = {
      try {
          step("checkStep1: " + a + " " + operator + " " + b + " = " + ref)  
          val test = f(a,b)
          val bigtest = bigValue(test)
          step("checkStep2: test = " + test + " => bigtest = " + bigtest)
          if (bigtest != ref) throw new CalculationException("failed: " + a + operator + b + " <> " + ref + " got [" + test + "] instead")
          test
      }
      catch {
        case x: ArithmeticException => {
            step("checkStep3: overflow - testing boundaries")
            if (!boundaryOverflow(ref)) throw new BoundaryException("failed: " + a + "+" + b + " was wrongly reported to overflow but yields [" + ref + "]") 
            step("checkStep4: overflow - ok")
            throw ContinueException
        }
      }
    }
    
    def bigValue(test: AnyVal): BigInt = {
      test match {
        case x: Byte => BigInt(x)
        case y: Short => BigInt(y)
        case z: Int => BigInt(z)
        case k: Long => BigInt(k)
      }
    }

      // BigInt(a) * BigInt(b)
    def refmul (a: ValType, b: ValType): BigInt = {
       step("refmul: " + a + " * " + b)
       val result = bigValue(a) * bigValue(b)
       step("result: " + result)
       result
    }
      // BigInt(a) * BigInt(b)
    def refadd (a: ValType, b: ValType): BigInt = {
       step("refadd: " + a + " + " + b)
       val result = bigValue(a) + bigValue(b)
       step("result: " + result)
       result
    }    
      // BigInt(a) - BigInt(b)
    def refsub (a: ValType, b: ValType): BigInt = {
       step("refsub: " + a + " - " + b)
       val result = bigValue(a) - bigValue(b)
       step("result: " + result)
       result
    }    
      // BigInt(a) / BigInt(b)
    def refdiv (a: ValType, b: ValType): BigInt = {
       step("refdiv: " + a + " / " + b)
       val result = bigValue(a) / bigValue(b)
       step("result: " + result)
       result
    }    

  }
  
  
  object TestAdaptor {
    val checker = new CheckedArithmetic{}
    import checker._
    
    private final val IntMinValue: Long  = Int.MinValue.toLong
    private final val IntMaxValue: Long  = Int.MaxValue.toLong
    private final val ShortMinValue: Long  = Short.MinValue.toLong
    private final val ShortMaxValue: Long  = Short.MaxValue.toLong
    private final val ByteMinValue: Long  = Byte.MinValue.toLong
    private final val ByteMaxValue: Long  = Byte.MaxValue.toLong
    
    def getRandomLong: Long = {      
      (BigInt((math.random * Long.MaxValue).toLong - (math.random * Long.MaxValue).toLong)
      +
       BigInt((math.random * Int.MaxValue).toLong - (math.random * Int.MaxValue).toLong)
      ).toLong
    }
    
    implicit object IntAdaptor extends TestAdaptor[Int] {

      def getRandom: ValType = {
        getRandomLong.toInt
      }
      
      def boundaryOverflow(t: BigInt): Boolean = {
        !((t >= IntMinValue) && (t <= IntMaxValue))
      }

      def mulstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x *? y, "*", refmul(a,b))
      def addstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x +? y, "+", refadd(a,b))
      def substep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x -? y, "-", refsub(a,b))
      def divstep(a: ValType, b: ValType): ValType = {
        if (b == 0) throw ContinueException
        checkStep(a, b, (x, y) => x /? y, "/", refdiv(a,b))
      }
    }

    implicit object LongAdaptor extends TestAdaptor[Long] {

      def getRandom: ValType = {
        getRandomLong.toLong
      }
      
      def boundaryOverflow(t: BigInt): Boolean = {
        !((t >= Long.MinValue) && (t <= Long.MaxValue))
      }

      def mulstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x *? y, "*", refmul(a,b))
      def addstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x +? y, "+", refadd(a,b))
      def substep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x -? y, "-", refsub(a,b))
      def divstep(a: ValType, b: ValType): ValType = {
        if (b == 0) throw ContinueException
        checkStep(a, b, (x, y) => x /? y, "/", refdiv(a,b))
      }
    }

    implicit object ShortAdaptor extends TestAdaptor[Short] {

      def getRandom: ValType = {
        getRandomLong.toShort
      }
      
      def boundaryOverflow(t: BigInt): Boolean = {
        !((t >= ShortMinValue) && (t <= ShortMaxValue))
      }

      def mulstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x *? y, "*", refmul(a,b))
      def addstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x +? y, "+", refadd(a,b))
      def substep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x -? y, "-", refsub(a,b))
      def divstep(a: ValType, b: ValType): ValType = {
        if (b == 0) throw ContinueException
        checkStep(a, b, (x, y) => x /? y, "/", refdiv(a,b))
      }
    }    

    implicit object ByteAdaptor extends TestAdaptor[Byte] {

      def getRandom: ValType = {
        getRandomLong.toByte
      }
      
      def boundaryOverflow(t: BigInt): Boolean = {
        !((t >= Byte.MinValue) && (t <= Byte.MaxValue))
      }

      def mulstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x *? y, "*", refmul(a,b))
      def addstep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x +? y, "+", refadd(a,b))
      def substep(a: ValType, b: ValType): ValType = checkStep(a, b, (x, y) => x -? y, "-", refsub(a,b))
      def divstep(a: ValType, b: ValType): ValType = {
        if (b == 0) throw ContinueException
        checkStep(a, b, (x, y) => x /? y, "/", refdiv(a,b))
      }
    }    

  }
  
  def main(args: Array[String]): Unit = {

  val a  = 10 *? 1000
  val b  = 100 -? 5 *? 5L
  val c  = (5 +? 5) *? 20 /? 10 -? 3
  val d  = 1 + 2 *? 3 +? 4 * 5 -? 2 /? 2 + 1
  val ac = 10 * 1000
  val bc = 100 - 5 * 5L
  val cc = (5 + 5) * 20 / 10 - 3
  val dc = 1 + 2 *  3 +  4 * 5 -  2 /  2 + 1
  if ((a != ac) || (b != bc) || (c != cc) || (d != dc)) { throw new AssertionError("priority violation") }  

  
  try {
      val result = (Int.MaxValue -? 1) ::
      (Int.MinValue +? 1) ::
      (15 +? (Int.MaxValue /? 16) *? 16 ) ::
      (15 + (Int.MaxValue/16) * 16 ) ::
      ((Int.MinValue/16) *? 16) ::
      (Long.MaxValue -? 1) ::
      (Long.MinValue +? 1) ::
      (Long.MaxValue +? -1) ::
      (Long.MinValue -? -1) ::
      (Int.MaxValue.toLong *? Int.MinValue.toLong) ::
      (Int.MaxValue.toLong *? Int.MinValue) ::
      ((Int.MaxValue.toLong +? 1) *? (Int.MinValue.toLong -? 1)) ::
      ((Int.MaxValue.toLong +? 1) *? (Int.MaxValue.toLong +? 1)) ::
      ((Int.MaxValue.toLong+0x3504f335) *? (Int.MaxValue.toLong+0x3504f334)) ::
      (0x0b504f334L *? 0x0b504f333L +? 0xac5930a3L) ::
      (0x07ffffffffffffffL *? 0x10 +? 15) ::
      ((Int.MaxValue.toLong+0x3504f335L) *? (Int.MaxValue.toLong+0x3504f334L)) ::
      (-0x0b504f334L *? 0x0b504f333L -? 0xac5930a4L) ::
      (0x07ffffffffffffffL *? -0x10 -? 16) ::
      (-1 *? 1161596684771610851L) ::
      Nil
      val result2 = (BigInt(Int.MaxValue) - BigInt(1)) ::
      (BigInt(Int.MinValue) + BigInt(1)) ::
      (BigInt(15) + (BigInt(Int.MaxValue) / BigInt(16)) * BigInt(16)) ::
      (BigInt(15) + (BigInt(Int.MaxValue) / BigInt(16)) * BigInt(16)) ::
      ((BigInt(Int.MinValue)/BigInt(16)) * BigInt(16)) ::
      (BigInt(Long.MaxValue) - BigInt(1)) ::
      (BigInt(Long.MinValue) + BigInt(1)) ::
      (BigInt(Long.MaxValue) + BigInt(-1)) ::
      (BigInt(Long.MinValue) - BigInt(-1)) ::
      (BigInt(Int.MaxValue.toLong) * BigInt(Int.MinValue.toLong)) ::
      (BigInt(Int.MaxValue.toLong) * BigInt(Int.MinValue)) ::
      ((BigInt(Int.MaxValue.toLong) + BigInt(1)) * (BigInt(Int.MinValue.toLong) - BigInt(1))) ::
      ((BigInt(Int.MaxValue.toLong) + BigInt(1)) * (BigInt(Int.MaxValue.toLong) + BigInt(1))) ::
      ((BigInt(Int.MaxValue.toLong)+BigInt(0x3504f335)) * (BigInt(Int.MaxValue.toLong)+BigInt(0x3504f334))) ::
      (BigInt(0x0b504f334L) * BigInt(0x0b504f333L) + BigInt(0xac5930a3L)) ::
      (BigInt(0x07ffffffffffffffL) * BigInt(0x10) + BigInt(15)) ::
      ((BigInt(Int.MaxValue.toLong)+BigInt(0x3504f335L)) * (BigInt(Int.MaxValue.toLong)+BigInt(0x3504f334L))) ::
      (BigInt(-0x0b504f334L) * BigInt(0x0b504f333L) - BigInt(0xac5930a4L)) ::
      (BigInt(0x07ffffffffffffffL) * BigInt(-0x10) - BigInt(16)) ::
      (BigInt(-1) * BigInt(1161596684771610851L)) ::
      Nil

      val check = result.zip(result2)
      check.foreach (t => {
        println("checking: " + t._1 + " <> " +t._2)
        t._1 match {
          case x: Byte => if (BigDecimal(x) != t._2) throw new AssertionError("miscalulated: " + t._1 + " <> " + t._2)
          case x: Short => if (BigDecimal(x) != t._2) throw new AssertionError("miscalulated: " + t._1 + " <> " + t._2)
          case x: Int => if (BigDecimal(x) != t._2) throw new AssertionError("miscalulated: " + t._1 + " <> " + t._2)
          case x: Long => if (BigDecimal(x) != t._2) throw new AssertionError("miscalulated: " + t._1 + " <> " + t._2)
          case y => throw new AssertionError("unexpected value type: " + y.getClass().getName) 
        }
      })
    }
    catch {
      case ex: Throwable => {
        println("Check failed")
        ex.toString
        ex.printStackTrace
      }
    }
    val result2 = 
      shouldfail("test1", (Int.MaxValue +? 1)) ::
      shouldfail("test2", (Int.MinValue -? 1)) ::
      shouldfail("test3",(16 +? (Int.MaxValue /? 16) *? 16 )) ::
      shouldfail("test4 - failure is ok:",(16 + (Int.MaxValue/16) * 16 )) ::
      shouldfail("test5",((Int.MinValue/16) *? 17)) ::
      shouldfail("test6",(Long.MaxValue +? 1)) ::
      shouldfail("test7",(Long.MinValue -? 1)) ::
      shouldfail("test8",(Long.MaxValue -? -1)) ::
      shouldfail("test9",(Long.MinValue +? -1)) ::
      shouldfail("test10",(Long.MaxValue *? Long.MinValue)) ::
      shouldfail("test11",((Long.MaxValue -? 0xf000L) *? Int.MinValue)) ::
      shouldfail("test12",((Int.MaxValue.toLong +? 0x80000001L) *? (Int.MinValue.toLong -? 1L))) ::
      shouldfail("test13",((Long.MaxValue) *? (Long.MaxValue))) ::
      shouldfail("test14",((Int.MaxValue.toLong+0x3504f335) *? (Int.MaxValue.toLong+0x3504f335))) ::
      shouldfail("test15",(0x0b504f334L *? 0x0b504f333L +? 0xac5930a4L)) ::
      shouldfail("test16",(0x0b504f334L *? 0x0b504f334L)) ::
      shouldfail("test17",(0x07ffffffffffffffL *? 0x11 -? 15)) ::
      shouldfail("test18",((Int.MaxValue.toLong +? 0x3504f335L) *? (Int.MaxValue.toLong +? 0x3504f335L))) ::
      shouldfail("test19",(-0x0b504f334L *? 0x0b504f333L -? 0xac5930a5L)) ::
      shouldfail("test20",(0x07ffffffffffffffL *? -0x11L +? 16L)) ::
      shouldfail("test21",(-5L *? -2036460136663654390L)) ::
      Nil
    result2.foreach {println}
    
    val report1 = randomcheck[Int](10000000)
    val report2 = randomcheck[Long](10000000)
    val report3 = randomcheck[Short](1000000)
    val report4 = randomcheck[Byte](100000)
    System.out.println("Int random check:")
    report1.foreach{println}
    System.out.println("Long random check:")
    report2.foreach{println}
    System.out.println("Short random check:")
    report3.foreach{println}
    System.out.println("Byte random check:")
    report4.foreach{println}

  }
  
  def debugcaseL(a:Long, b:Long): Long = {
    try {
      LongAdaptor.addstep(a, b)
    }
    catch {
      case x: Exception => {
        x.printStackTrace();
        0L
      }
    }
  }
  
  def randomcheck[T <: AnyVal](times: Int)(implicit driver: TestAdaptor[T]): Vector[String] = {
     var detectedOverflows: Int = 0
     var calculationErrors: Int = 0
     var boundaryErrors: Int = 0
     var errorlog = Vector.empty[String]
     var i = times
     var lastresult: T = driver.getRandom
     val stats: Array[Int] = new Array[Int](4)
     while (i > 0) {
       i = i-1
       if (i%500000 == 0) System.out.println("running test: " + i)
       val step: Int = (math.random*4).toInt
       stats(step) = stats(step)+1
       try {
         step match {
           case 0 => lastresult = driver.addstep(lastresult, driver.getRandom)
           case 1 => lastresult = driver.substep(lastresult, driver.getRandom)
           case 2 => lastresult = driver.mulstep(lastresult, driver.getRandom)
           case 3 => lastresult = driver.divstep(lastresult, driver.getRandom)
         }
       }
       catch {
         case ContinueException => detectedOverflows = detectedOverflows + 1
         case ex: BoundaryException => {
           System.out.println(ex.getMessage())
           errorlog = ex.getMessage +: errorlog
           boundaryErrors = boundaryErrors+1
           // for now: we stop on errors
           System.err.println(driver.flush())
           sys.exit(1)
         }
         case ex: CalculationException => {
           System.out.println(ex.getMessage())
           errorlog = ex.getMessage +: errorlog
           calculationErrors = calculationErrors +1
           // for now: we stop on errors
           System.err.println(driver.flush())
           sys.exit(1)
         }
         case ex => throw ex
       }
     }
     errorlog ++: Vector(
         "ran " + times + " tests",
         "ran " + stats(0) + " additions, " + stats(1) + " subtractions, " + stats(2) + " multiplications, " + stats(3) + " divisions",
         "detected overflow: " + detectedOverflows,
         "calculation errors: " + calculationErrors, 
         "boundary errors: " + boundaryErrors)     
  }
  
  def shouldfail(name: String, eval: =>Long): String = {
    try {
      val test: Long = eval
      name + " failed! (" + test + ")"
    }
    catch {
      case u: ArithmeticException => name + " succeeded"
      case ex => name + " failed: " + ex.toString
    }
  }
}  
