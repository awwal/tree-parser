package com.lawal

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class LineItem(lineNumber: Int, content: String, identSize: Int)
case class ParentChild(parent: LineItem, child: LineItem)

class TreeParser[A](f: String => A) {

  private def createNormalizer(lines: List[LineItem]): Int => Int = {

    @tailrec
    def descend(normalized: Int, rem: List[LineItem], map: mutable.Map[Int, Int]): Unit = {
      rem match {
        case first :: second :: rest if first.identSize <= second.identSize =>
          if (map.contains(first.identSize)) {
            descend(normalized, second :: rest, map)
          } else {
            map(first.identSize) = normalized
            descend(normalized + 1, second :: rest, map)
          }
        case _ => ()
      }
    }

    val normalizer = mutable.Map[Int, Int]().withDefaultValue(0)
    descend(0, lines, normalizer)
    normalizer
  }

  def parseTree(s: String, prefixChar: Char = ' '): Option[Node[A]] = {

    val lines: List[(String, Int)] = s.split(System.lineSeparator()).filter(_.nonEmpty).zipWithIndex.toList

    val list: List[LineItem] = lines.map { case (line, lineNumber) =>
      val spaceCount = line.takeWhile(ch => ch == prefixChar).length
      val identifier = line.dropWhile(ch => ch == prefixChar)
      LineItem(lineNumber, identifier, spaceCount)
    }

    list match {
      case Nil          => None
      case first :: Nil => Some(Node(f(first.content)))
      case _            => processMultiNodeTree(list)
    }
  }

  private def processMultiNodeTree(list: List[LineItem]): Option[Node[A]] = {

    val normalizer = createNormalizer(list)

    val normalized = list.map { it =>
      it.copy(identSize = normalizer(it.identSize))
    }

    val parentTracker = mutable.Map[Int, LineItem]()

    val parentToChild = ListBuffer[ParentChild]()

    normalized.foreach { it =>
      val indentSize = it.identSize

      parentTracker(indentSize) = it

      val parentSize = indentSize - 1
      if (parentSize >= 0) {
        val parentItem = parentTracker(parentSize)
        parentToChild += ParentChild(parentItem, it)
      }
    }

    parentToChild.toList match {
      case Nil                  => None
      case l: List[ParentChild] => Some(toNode(l))
    }

  }

  private def toNode(parentToChild: List[ParentChild]): Node[A] = {

    //groupBy will use regular map and will not keep ordering
    val parentToChildren = parentToChild
      .foldLeft(mutable.LinkedHashMap[LineItem, ListBuffer[LineItem]]())((pcs, pc) => {
        if (pcs.contains(pc.parent)) {
          val childList = pcs(pc.parent)
          childList.append(pc.child)
        } else {
          pcs.put(pc.parent, ListBuffer(pc.child))
        }
        pcs
      })
      .withDefaultValue(new ListBuffer[LineItem]())

    mapToNode(parentToChildren.head._1, parentToChildren)
  }

  def mapToNode(item: LineItem, parentToChildren: mutable.Map[LineItem, ListBuffer[LineItem]]): Node[A] = {
    val nodes = parentToChildren(item).map { child =>
      mapToNode(child, parentToChildren)
    }
    Node(f(item.content), nodes.toList)
  }
}
