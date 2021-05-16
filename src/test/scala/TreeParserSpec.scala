import com.lawal.{ Node, TreeParser }
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class TreeParserSpec extends AnyWordSpec with Matchers with OptionValues {

  "Tree Parser" should {
    "parse empty tree correctly" in {
      val tree = ""
      val result = new TreeParser(identity).parseTree(tree)
      result shouldBe None
    }

    "parse single elem correctly" in {

      val tree =
        """
          | loner
          |""".stripMargin

      val expected = Node("loner")

      val result = new TreeParser(identity).parseTree(tree)

      result.value shouldBe expected

    }
    "parse boolean correctly" in {

      val tree =
        """
          | false
          |  true
          |       false
          |       true
          |  true
          |       false
          |""".stripMargin

      val expected =
        Node(
          false,
          List(
            Node(true, List(Node(false), Node(true))),
            Node(true, List(Node(false)))
          )
        )

      val result = new TreeParser[Boolean](_.toBoolean).parseTree(tree)

      result.value shouldBe expected
    }

    "parse nested tree with `-` has prefix char" in {
      val tree =
        """
        |a
        |--b
        |--c
        |--d
        |----e
        |----f
        |------g
        |------h
        |""".stripMargin

      val expected = Node(
        "a",
        List(
          Node("b"),
          Node("c"),
          Node(
            "d",
            List(
              Node("e"),
              Node("f", List("g", "h").map(id => Node(id)))
            ))
        ))

      val result = new TreeParser[String](identity).parseTree(tree, '-')

      result.value shouldBe expected

    }
    "parse nested tree correctly" in {

      val tree =
        """
          | usr
          |  java
          |       bin
          |       doc
          |  python
          |       bin
          |""".stripMargin

      val expected = Node(
        "usr",
        List(
          Node("java", List(Node("bin"), Node("doc"))),
          Node("python", List(Node("bin")))
        ))

      val result = new TreeParser[String](identity).parseTree(tree)

      result.value shouldBe expected

    }

    "parse tree in file correctly" in {
      val tree = Source.fromResource("tree.txt").mkString

      val expected = Node(
        "App-2.7.3",
        List(
          Node("configure"),
          Node("configure.in"),
          Node(
            "Demo",
            List(
              Node(
                "cgi",
                List(
                  Node("cgi0.sh"),
                  Node("cgi1.py"),
                  Node("cgi2.py"),
                  Node("cgi3.py"),
                  Node("README"),
                  Node("wiki.py")))))
        )
      )

      val result = new TreeParser[String](_.trim).parseTree(tree, '-')
      result.value shouldBe expected
    }

    "parse output of node show" in {
      val node = Node[Int](
        1,
        List(
          Node(11, List(Node(112), Node(113))),
          Node(21, List(Node(212)))
        ))

      val sb = new StringBuffer()
      val collector = (s: String) => {
        sb.append(s)
        sb.append(System.lineSeparator())
        ()
      }
      Node.show(node, '*', collector)
      val tree = sb.toString

      val result = new TreeParser[Int](_.toInt).parseTree(tree, '*')

      result.value shouldBe node

    }
  }
}
