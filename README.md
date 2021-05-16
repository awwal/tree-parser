This is a no-dependency utility lib that converts a simple string like 

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
Into a a Nested node object

    Node("a",
        List(
          Node("b"),
          Node("c"),
          Node("d",
            List(
              Node("e"),
              Node("f", List("g", "h").map(id => Node(id)))
        ))
    ))

Usage 

    val node:Node[String] = new TreeParser[String](identity).parseTree(tree, '-'

Parameter to the TreeParser can also be provided to convert the string to other types

    val node:Node[Int] = new TreeParser[Int](_.toInt).parseTree(tree, '*')


