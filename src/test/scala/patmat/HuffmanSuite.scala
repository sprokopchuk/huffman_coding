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
    val listChars = List('a', 'b', 'a', 'b', 'a')
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


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times for list of chars") {
    new TestTrees {
      assert(times(listChars) === List(('a', 3), ('b', 2)) || times(listChars) === List(('b', 2), ('a', 3)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree of some chars") {
    val text = "text"
    val tree = Fork(Fork(Leaf('e',1),Leaf('x',1), List('e', 'x'), 2), Leaf('t',2), List('e', 'x', 't'), 4)
    assert(createCodeTree(text.toList) === tree)
  }

  test("decode text") {
    val text = "texttext"
    val tree = Fork(Fork(Leaf('e',2),Leaf('x',2),List('e', 'x'),4),Leaf('t',4),List('e', 'x', 't'),8)
    assert(decode(tree, List(1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1)) === text.toList)
  }

  test("convert tree into code table") {
    val tree = Fork(Fork(Leaf('e',2),Leaf('x',2),List('e', 'x'),4),Leaf('t',4),List('e', 'x', 't'),8)
    val codeTable = List(('e',List(0, 0)), ('x',List(0, 1)), ('t',List(1)))
    assert(convert(tree) === codeTable)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
