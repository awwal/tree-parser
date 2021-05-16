package com.lawal

case class Node[A](id: A, items: List[Node[A]] = List.empty)

object Node {

  def show[A](node: Node[A], prefixChar: Char = ' ', printer: String => Unit = Console.println): Unit = {
    printHelper(0, node, prefixChar, printer)
  }

  private def printHelper[A](depth: Int, node: Node[A], prefixChar: Char, printer: String => Unit): Unit = {
    val prefixString: String = s"$prefixChar" * depth
    printer(s"$prefixString${node.id}")
    node.items.foreach { n =>
      printHelper(depth + 1, n, prefixChar, printer)
    }
  }
}
