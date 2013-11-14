package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("times bla bla bla") {
    new TestTrees {
      val times = Huffman.times(List('a','b','a'))
      assert(times.length == 2)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("singleton is singleton") {
    assert(singleton(List()) == false)
    assert(singleton(List(new Leaf('a',1))) == true)
    assert(singleton(List(new Leaf('a', 1), new Leaf('b', 2))) == false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("encode simple tree") {
    new TestTrees {
    	val encoded = createCodeTree("aaaaaaaabbbcdefgh".toList)
    	println(encoded)
    	assert(true)
    	val decoded = decode(encoded, List(0,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded = encode(t1)("ab".toList)
      val decoded = decode(t1, encoded)
      assert(decoded === "ab".toList)
    }
  }
  
  test("decode and quickencode a very short text should be identity") {
    new TestTrees {
      val encoded = quickEncode(t1)("ab".toList)
      val decoded = decode(t1, encoded)
      assert(decoded === "ab".toList)
    }
  }
  
  test("convert creates codetable for a tree") {
    new TestTrees {
      val converted = convert(t1)
      assert(converted.head._1 == 'a' && converted.head._2 == List(0))
    }
  }
}
