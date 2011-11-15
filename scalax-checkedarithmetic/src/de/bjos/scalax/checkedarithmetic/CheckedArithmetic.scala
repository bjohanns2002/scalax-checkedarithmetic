package de.bjos.scalax.checkedarithmetic

/**
 * 

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

    - Redistributions of source code must retain
      this list of conditions and the following disclaimer.
    - Redistributions in binary form must reproduce
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    - Neither the name of the author nor the names of contributors 
      may be used to endorse or promote products derived from 
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

This is non profit open source code which is neither expected to be
error free nor intended for use in mission critical systems.

By using this software in source or binary format you agree to the
conditions and disclaimer above.

 * 
 *
 */

import scala.util.control.ControlThrowable

/**
 * provides ArithmeticExceptions with filled in partially erased stack trace
 * (to hide the "lower level" parts) and offending arguments along with
 * the intended target value range
 * 
 * Usage:
 * In client code:
 * 
 *   private val checked = new CheckedArithmetic with ReportExceptor
 *   import checked._
 */

trait ReportExceptor {
  self: CheckedArithmetic => 
  val modulname = classOf[CheckedArithmetic].getName
  val embeddedname = this.getClass().getName
  override def overflow(msg: => String = ""): ArithmeticException = {
    val result = new java.lang.ArithmeticException(msg)
    val sttrc = result.getStackTrace().filterNot(
        _.getClassName().startsWith(modulname)
        ).filterNot(
        _.getClassName().startsWith(embeddedname)
        ).tail
    result.setStackTrace(sttrc);
    result
  }
  override def overflow(
      a: Long, b: Long, op: String, range: String
  ): ArithmeticException = overflow(a + op + b + " :" + range)
}

/**
 * provides checked arithmetics by enriching the integral data types
 * with checked versions of the basic arithmetic operators.
 * 
 * The "checked" operators +?, -?, *? and /? share the same priority
 * as their unchecked parts and can be used as drop in replacements.
 * 
 * They differ however in their behaviour:
 * 
 * +?, -? and *? return the result in the range of the widest operand.
 * I.e. 10L +? 10 = 20L   and 10 +? 10L = 20L
 * 
 * /? returns the result in the range of the dividend.
 * 
 * Each of the checked operators checks if the result can be savely
 * cast into the defined result range and raises an arithmetic exception
 * when not. The exact flavour of exception can be customized by
 * overriding the overflow method.
 * 
 * The default exception is ArithmeticException with ControlThrowable
 * 
 * Basic usage:
 *
 *   private val checked = new CheckedArithmetic{} // standard overflow method
 *   import checked._                              // activate implicits
 *
 *   val a = 10 *? 1000
 *   val b = 100 -? 5 *? 5L
 *   val c = (5 +? 5) *? 20 /? 10 -? 3
 *
 * Current status:
 *   * work in (slow) progress
 *   * largely untested (!) especially the Long checks may be faulty 
 *   * needs some real test coverage
 *   
 * Known issues:
 *   
 *   a) unary negation is not supported (val a = -?10)
 *      impact: negating the result of a calculation will be unchecked
 *      error : when the result equals <range>.MinValue the negation will silently overflow
 *      cure  : negate by subtracting from 0 (val a = 0 -? 10)
 *
 *   b) slows down most calculations by a factor of rougly 2 (a little more)
 *   
 *   c) Long multiplication has a worse time profile with a slowdown of ~10
 *      cause : the long division used to check for overflows is slow
 *      cure  : non yet (some bright ideas - anybody?)
 *   
 *   d) debugging is somewhat tedious due to inlining
 *   
 *   e) just tested and profiled on sun jre 1.6 other jre might profile far worse
 *   
 *   
 * Well this looks not DRY at all...
 *   
 *   I know - it was once, using type classes as implementers for the generic
 *   "front end".
 *   This approach was ~ 10x slower than the current implementation.
 *   While I agree that performance is not always an argument I wasn't
 *   willing to pay that price for overflow checking.
 */

trait CheckedArithmetic {
  private val arithexception = new ArithmeticException with ControlThrowable
  def overflow(msg: => String = ""): ArithmeticException = arithexception
  def overflow(a: Long, b: Long, op: String, range: String): ArithmeticException = arithexception

  /*
   * Short
   */

  implicit def toCheck(q: Byte) = new CheckedByte(q)
  
  @inline
  private final def plus_byte(a: Byte, b: Byte): Byte = {
    val r: Int = a.toInt + b.toInt
    if ((r > Byte.MaxValue) || (r < Byte.MinValue)) throw overflow(a,b,"+","Byte")
    r.toByte
  }
  
  @inline
  private final def minus_byte(a: Byte, b: Byte): Byte = {
    val r: Int = a.toInt - b.toInt
    if ((r > Byte.MaxValue) || (r < Byte.MinValue)) throw overflow(a,b,"-","Byte")
    r.toByte
  }

  @inline
  private final def mul_byte(a: Byte, b: Byte): Byte = {
        val r: Int = a.toInt * b.toInt
        val i: Byte = r.toByte
        if (r != i) throw overflow(a,b,"*","Byte")
        i
  }
  
  final class CheckedByte(val a: Byte) {

    final def +?(b: Byte): Byte = {
      plus_byte(a,b)
    }
  
    final def -?(b: Byte): Byte = {
      minus_byte(a,b)
    }
    
    final def *?(b: Byte): Byte = {
      mul_byte(a,b)
    }
    
    final def /?(b: Byte): Byte = {
      if ((a == Byte.MinValue) && (b == -1)) throw overflow(a,b,"/","Byte")     
      (a/b).toByte
    }

    final def +?(b: Short): Short = {
      plus_short(a,b)
    }
  
    final def -?(b: Short): Short = {
      minus_short(a,b)
    }
    
    final def *?(b: Short): Short = {
      mul_short(a,b)
    }
    
    final def /?(b: Short): Byte = {
      if ((a == Byte.MinValue) && (b == -1)) throw overflow(a,b,"/","Byte")      
      ((a.toInt)/b).toByte
    }
    
    
    final def +?(b: Int): Int = {
      plus_int(a,b)
    }
  
    final def -?(b: Int): Int = {
      minus_int(a,b)
    }
    
    final def *?(b: Int): Int = {
      mul_int(a,b)
    }
    
    final def /?(b: Int): Byte = {
      if ((a == Byte.MinValue) && (b == -1)) throw overflow(a,b,"/","Byte")      
      ((a.toInt)/b).toByte
    }
    
    
    final def +?(b: Long): Long = {
      plus_long(a,b)
    }
  
    final def -?(b: Long): Long = {
      plus_long(a,-b)
    }
    
    final def *?(b: Long): Long = {
      mul_long(a,b)
    }
    
    final def /?(b: Long): Byte = {
      if ((a == Byte.MinValue) && (b == -1L)) throw overflow(a,b,"/","Byte")      
      ((a.toLong)/b).toByte
    }
    
  }
  
  
  /*
   * Short
   */

  implicit def toCheck(q: Short) = new CheckedShort(q)
  
  @inline
  private final def plus_short(a: Short, b: Short): Short = {
    val r: Int = a.toInt + b.toInt
    if ((r > Short.MaxValue) || (r < Short.MinValue)) throw overflow(a,b,"+","Short")
    r.toShort
  }
  
  @inline
  private final def minus_short(a: Short, b: Short): Short = {
    val r: Int = a.toInt - b.toInt
    if ((r > Short.MaxValue) || (r < Short.MinValue)) throw overflow(a,b,"-","Short")
    r.toShort
  }

  @inline
  private final def mul_short(a: Short, b: Short): Short = {
        val r: Int = a.toInt * b.toInt
        val i: Short = r.toShort
        if (r != i) throw overflow(a,b,"*","Short")
        i
  }
  
  final class CheckedShort(val a: Short) {

    final def +?(b: Short): Short = {
      plus_short(a,b)
    }
  
    final def -?(b: Short): Short = {
      minus_short(a,b)
    }
    
    final def *?(b: Short): Short = {
      mul_short(a,b)
    }
    
    final def /?(b: Short): Short = {
      if ((a == Short.MinValue) && (b == -1)) throw overflow(a,b,"/","Short")      
      (a/b).toShort
    }

    final def +?(b: Int): Int = {
      plus_int(a,b)
    }
  
    final def -?(b: Int): Int = {
      minus_int(a,b)
    }
    
    final def *?(b: Int): Int = {
      mul_int(a,b)
    }
    
    final def /?(b: Int): Short = {
      if ((a == Short.MinValue) && (b == -1)) throw overflow(a,b,"/","Short")      
      ((a.toInt)/b).toShort
    }
    
    
    final def +?(b: Long): Long = {
      plus_long(a,b)
    }
  
    final def -?(b: Long): Long = {
      plus_long(a,-b)
    }
    
    final def *?(b: Long): Long = {
      mul_long(a,b)
    }
    
    final def /?(b: Long): Short = {
      if ((a == Short.MinValue) && (b == -1L)) throw overflow(a,b,"/","Short")      
      ((a.toLong)/b).toShort
    }

    final def asByte(): Byte = {
      if ((a > Byte.MaxValue) || (a < Byte.MinValue)) throw overflow(a + " as Byte")
      a.toByte
    }
    
  }
  
  /*
   * Integer
   */
  
  implicit def toCheck(q: Int) = new CheckedInt(q)
  private final val IntMaxValue: Long = Int.MaxValue.toLong
  private final val IntMinValue: Long = Int.MinValue.toLong
  
  @inline
  private final def plus_int(a: Int, b: Int): Int = {
    val r: Long = a.toLong + b.toLong
    if ((r > IntMaxValue) || (r < IntMinValue)) throw overflow(a,b,"+","Int")
    r.toInt
  }
  
  @inline
  private final def minus_int(a: Int, b: Int): Int = {
     if (b == Int.MinValue) throw overflow(a,b,"-","Int")
     plus_int(a, -b)
  }

  @inline
  private final def mul_int(a: Int, b: Int): Int = {
        if (b == 0 || a == 0) return 0
        if (a == 1) return b
        if (b == 1) return a
        val r: Long = a.toLong * b.toLong
        if ((r > IntMaxValue) || (r < IntMinValue)) throw overflow(a,b,"*","Int")
        r.toInt
  }


  final class CheckedInt(val a: Int) {

    final def +?(b: Int): Int = {
      plus_int(a,b)
    }
  
    final def -?(b: Int): Int = {
      minus_int(a,b)
    }
    
    final def *?(b: Int): Int = {
      mul_int(a,b)
    }
    
    final def /?(b: Int): Int = {
      if ((a == Int.MinValue) && (b == -1)) throw overflow(a,b,"/","Int")
      a/b
    }

    final def +?(b: Long): Long = {
      plus_long(a,b)
    }
  
    final def -?(b: Long): Long = {
      minus_long(a,b)
    }
    
    final def *?(b: Long): Long = {
      mul_long(a,b)
    }
    
    final def /?(b: Long): Int = {
      if ((a == Int.MinValue) && (b == -1L)) throw overflow(a,b,"/","Int")
      ((a.toLong)/b).toInt
    }

    final def asShort(): Short = {
      if ((a > Short.MaxValue) || (a < Short.MinValue)) throw overflow(a + " as Short")
      a.toShort
    }
    
    final def asByte(): Byte = {
      if ((a > Byte.MaxValue) || (a < Byte.MinValue)) throw overflow(a + " as Byte")
      a.toByte
    }
    
  }

  /*
   * Long
   */
  
  implicit def toCheck(q: Long) = new CheckedLong(q)

  @inline
  protected final def plus_long(a: Long, b: Long): Long = {
        val r: Long = a + b
        if (a >= b) {
          if ((a > 0) && ((Long.MaxValue-a) < b)) throw overflow(a,b,"+","Long")
          if ((a < 0) && ((Long.MinValue-a) > b)) throw overflow(a,b,"+","Long")
        }
        else {
          if ((b > 0) && ((Long.MaxValue-b) < a)) throw overflow(a,b,"+","Long")
          if ((b < 0) && ((Long.MinValue-b) > a)) throw overflow(a,b,"+","Long")        
        }
        r
    }

  @inline
  protected final def minus_long(a: Long, b: Long): Long = {
        val r: Long = a - b
        if ((b > 0) && (r > a)) throw overflow(a,b,"-","Long")
        if ((b < 0) && (r < a)) throw overflow(a,b,"-","Long")
        r
    }
  
  
  @inline
  protected final def mul_long(a: Long, b: Long): Long = {
        if (b == 0 || a == 0) return 0L
        // fast estimate - we pay the extra cycles to avoid the
        // slow long division for a and b in int range
        // -a * b > MinVal
        // b < MinVal/-a
        // a * b < MaxVal
        // b < MaxVal/a
        if ((a < 0) && (b < 0)) {
          if (( a >= IntMinValue) && ( b >= IntMinValue)) a*b
          else if (Long.MaxValue/a <= b) a*b 
          else throw overflow(a,b,"*","Long")
        }
        else if ((a > 0) && (b > 0)) {
          if (( a <= IntMaxValue) && ( b <= IntMaxValue)) a*b
          else if (Long.MaxValue/a >= b) a*b
          else throw overflow(a,b,"*","Long")          
        }
        else {
          val p1 = if (a < b) a else b
          val p2 = if (a < b) b else a
          if (p1 == -1L && p2 <= Long.MaxValue) a*b
          else if ((p1 >= IntMinValue) && ( p2 <= IntMaxValue)) a*b
          else if (Long.MinValue/p1 >= p2) a*b
          else throw overflow(a,b,"*", "Long")
        }

    }

  final class CheckedLong(val a: Long) {
    final def +?(b: Long): Long = {
      plus_long(a,b)
    }

    final def -?(b: Long): Long = {
      minus_long(a, b)
    }
    
    final def *?(b: Long): Long = {
      mul_long(a,b)
    }

    final def /?(b: Long): Long = {
      if ((a == Long.MinValue) && (b == -1)) throw overflow(a,b,"/","Long")
      a/b
    }

    final def asShort(): Short = {
      if ((a > Short.MaxValue) || (a < Short.MinValue)) throw overflow(a + " as Short")
      a.toShort
    }
    
    final def asByte(): Byte = {
      if ((a > Byte.MaxValue) || (a < Byte.MinValue)) throw overflow(a + " as Byte")
      a.toByte
    }
    
    final def asInt(): Int = {
      if ((a > Int.MaxValue) || (a < Int.MinValue)) throw overflow(a + " as Int")
      a.toInt      
    }
  }
  
}

