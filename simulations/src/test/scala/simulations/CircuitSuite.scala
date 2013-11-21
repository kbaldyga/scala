//package simulations
//
//import org.scalatest.FunSuite
//
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//import org.scalacheck.Gen._
//import org.scalacheck.Prop.forAll
//import org.scalacheck.{Arbitrary, Gen, Properties}
//import org.scalacheck.Arbitrary.arbitrary
//
//@RunWith(classOf[JUnitRunner])
//class CircuitSuite extends CircuitSimulator with FunSuite {
//  val InverterDelay = 1
//  val AndGateDelay = 3
//  val OrGateDelay = 5
//  
//  test("andGate example") {
//    val in1, in2, out = new Wire
//    andGate(in1, in2, out)
//    in1.setSignal(false)
//    in2.setSignal(false)
//    run
//    
//    assert(out.getSignal === false, "and 1")
//
//    in1.setSignal(true)
//    run
//    
//    assert(out.getSignal === false, "and 2")
//
//    in2.setSignal(true)
//    run
//    
//    assert(out.getSignal === true, "and 3")
//  }
//  
//    test("orGate example") {
//    val in1, in2, out = new Wire
//    orGate(in1, in2, out)
//    in1.setSignal(false)
//    in2.setSignal(false)
//    run
//    assert(out.getSignal === false, "or 1")
//    
//    in1.setSignal(true)
//    run
//    assert(out.getSignal === true, "or 2")
//    
//    in1.setSignal(false)
//    run
//    assert(out.getSignal === false, "or 3")
//    
//    in2.setSignal(true)
//    run
//    assert(out.getSignal === true, "or 4")
//  }
//  
//  test("orGate2 example") {
//    val in1, in2, out = new Wire
//    orGate2(in1, in2, out)
//    in1.setSignal(false)
//    in2.setSignal(false)
//    run
//    assert(out.getSignal === false, "or 1")
//    
//    in1.setSignal(true)
//    run
//    assert(out.getSignal === true, "or 2")
//    
//    in1.setSignal(false)
//    run
//    assert(out.getSignal === false, "or 3")
//    
//    in2.setSignal(true)
//    run
//    assert(out.getSignal === true, "or 4")
//  }
//  
//
//  //
//  // to complete with tests for orGate, demux, ...
//  //
//  lazy val genWire: Gen[Wire] = for {
//    s <- arbitrary[Boolean]
//    w <- { val w = new Wire; w.setSignal(s); w }
//  } yield w
//
//  implicit lazy val arbWire: Arbitrary[Wire] = Arbitrary(genWire)
//
//  lazy val genWireList: Gen[List[Wire]] = for {
//    n <- choose(0, 8) // Avoid lists larger than 8 wires.
//    l <- listOfN(n, arbitrary[Wire])
//  } yield l
//
//  implicit lazy val arbWireList: Arbitrary[List[Wire]] = Arbitrary(genWireList)
//
//  property("and gate") = forAll {
//    (in1: Wire, in2: Wire) =>
//      val out = new Wire
//      andGate(in1, in2, out)
//      run
//      out.getSignal == (in1.getSignal & in2.getSignal)
//  }
//
//  property("or gate") = forAll {
//    (in1: Wire, in2: Wire) =>
//      val out = new Wire
//      orGate(in1, in2, out)
//      run
//      out.getSignal == (in1.getSignal | in2.getSignal)
//  }
//
//  property("or2 gate") = forAll {
//    (in1: Wire, in2: Wire) =>
//      val out = new Wire
//      orGate2(in1, in2, out)
//      run
//      out.getSignal == (in1.getSignal | in2.getSignal)
//  }
//
//  def signalToBit(s: Boolean) =
//    if (s) 1 else 0
//
//  def controlToBitNumber(ws: List[Wire]) =
//    ws.foldLeft(0) { (acc, w) => 2 * acc + signalToBit(w.getSignal) }
//
//  def countOneBits(ws: List[Wire]) =
//    ws.map(w => signalToBit(w.getSignal)).sum
//
//  property("demux") = forAll {
//    (wIn: Wire, wControl: List[Wire]) =>
//      val numBitsControl = wControl.length
//      val numBitsOutput = 1 << numBitsControl
//      val wOutput = List.fill(numBitsOutput){ new Wire }
//
//      demux(wIn, wControl, wOutput)
//      run
//
//      val numBitInOut = numBitsOutput - 1 - controlToBitNumber(wControl)
//      val targetBitShouldBeInput = wOutput(numBitInOut).getSignal == wIn.getSignal
//
//      targetBitShouldBeInput && countOneBits(wOutput) == signalToBit(wIn.getSignal)
//  }
//
//}
package simulations

import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Arbitrary.arbitrary

import Circuit._

object CircuitSuite extends Properties("Circuit") {

  lazy val genWire: Gen[Wire] = for {
    s <- arbitrary[Boolean]
    w <- { val w = new Wire; w.setSignal(s); w }
  } yield w

  implicit lazy val arbWire: Arbitrary[Wire] = Arbitrary(genWire)

  lazy val genWireList: Gen[List[Wire]] = for {
    n <- choose(0, 8) // Avoid lists larger than 8 wires.
    l <- listOfN(n, arbitrary[Wire])
  } yield l

  implicit lazy val arbWireList: Arbitrary[List[Wire]] = Arbitrary(genWireList)

  property("and gate") = forAll {
    (in1: Wire, in2: Wire) =>
      val out = new Wire
      andGate(in1, in2, out)
      run
      out.getSignal == (in1.getSignal & in2.getSignal)
  }

  property("or gate") = forAll {
    (in1: Wire, in2: Wire) =>
      val out = new Wire
      orGate(in1, in2, out)
      run
      out.getSignal == (in1.getSignal | in2.getSignal)
  }

  property("or2 gate") = forAll {
    (in1: Wire, in2: Wire) =>
      val out = new Wire
      orGate2(in1, in2, out)
      run
      out.getSignal == (in1.getSignal | in2.getSignal)
  }

  def signalToBit(s: Boolean) =
    if (s) 1 else 0

  def controlToBitNumber(ws: List[Wire]) =
    ws.foldLeft(0) { (acc, w) => 2 * acc + signalToBit(w.getSignal) }

  def countOneBits(ws: List[Wire]) =
    ws.map(w => signalToBit(w.getSignal)).sum

  property("demux") = forAll {
    (wIn: Wire, wControl: List[Wire]) =>
      val numBitsControl = wControl.length
      val numBitsOutput = 1 << numBitsControl
      val wOutput = List.fill(numBitsOutput){ new Wire }

      demux(wIn, wControl, wOutput)
      run

      val numBitInOut = numBitsOutput - 1 - controlToBitNumber(wControl)
      val targetBitShouldBeInput = wOutput(numBitInOut).getSignal == wIn.getSignal

      targetBitShouldBeInput && countOneBits(wOutput) == signalToBit(wIn.getSignal)
  }
}