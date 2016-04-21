package net.liftweb.util

import net.liftweb.json._

import scala.xml.{Text, UnprefixedAttribute, Node}

object VDom {
  val pcdata = "#PCDATA"
  case class VNode(tag:String, attributes:Map[String, String] = Map(), children:List[VNode] = List(), text:Option[String] = None)

  trait VNodeTransform
  case class VNodeInsert(position:Int, node:VNode) extends VNodeTransform
  case class VNodeDelete(position:Int) extends VNodeTransform
  case class VNodeReorder(permutation:Map[Int, Int]) extends VNodeTransform

  case class VNodeTransformTree(transforms:List[VNodeTransform], children:List[VNodeTransformTree])

  object typeHints extends TypeHints {
    val classToHint:Map[Class[_], String] = Map(
      classOf[VNodeInsert] -> "insert",
      classOf[VNodeDelete] -> "delete",
      classOf[VNodeReorder] -> "reorder"
    )
    val hint2Class:Map[String, Class[_]] = classToHint.map { case (c, h) => h -> c }.toMap
    override val hints: List[Class[_]] = classToHint.keysIterator.toList
    override def hintFor(clazz: Class[_]):String = classToHint(clazz)
    override def classFor(hint: String) = hint2Class.get(hint)
  }
  val formats = new Formats {
    override val dateFormat: DateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = VDom.typeHints
    override val typeHintFieldName = "type"
  }

  def diff(a:Node, b:Node):VNodeTransformTree = {
    val aChildren = a.nonEmptyChildren.filter(isntWhitespace).toList
    val bChildren = b.nonEmptyChildren.filter(isntWhitespace).toList

   // val additions = bChildren.zipWithIndex.drop(aChildren.length)
   //   .map { case (n, i) => VNodeInsert(i, VNode.fromXml(n)) }
    val before = aChildren//.zipWithIndex // if before < after, Insert
    val after = bChildren//.zipWithIndex // if before > after, Delete
                                      // if before == after, same or reorder
    val additions = if (before.size < after.size) {
      after.diff(before).map {
        case c => VNodeInsert(after.indexOf(c), VNode.fromXml(c))
      }
    }
    else List()

    val deletions = if (before.size > after.size) {
      before.diff(after).map {
        case c => VNodeDelete(before.indexOf(c))
      }
    }
    else List()

    val deletions2 = aChildren.diff(bChildren)
      .map(c => VNodeDelete(aChildren.indexOf(c)))
    println("old deletions: " + deletions2)
    println("new deletions: " + deletions)

    val transforms = additions ++ deletions

    val children = aChildren.zip(bChildren)
      .collect {
        case (ca, cb) if ca != cb => diff(ca, cb)     // This != check probably would benefit from memoizing
        case _ => VNodeTransformTree(List(), List())  // No changes for this node, make a placeholder
      }

    VNodeTransformTree(transforms, children)
  }

  def compare(a:Node, b:Node):Float =
    if(a == b) 1f
    else 0f

  private def isText(n:Node) = n.label == pcdata
  private def isntWhitespace(n:Node) = !isText(n) || !n.text.trim.isEmpty

  object VNode {
    def text(t:String):VNode = VNode("#text", Map(), List(), Some(t))
    def fromXml(n:Node):VNode = {
      if(n.label == pcdata) text(n.text)
      else {
        val attrs:Map[String, String] = n.attributes
          .collect { case UnprefixedAttribute(k, Text(v), _) => k -> v }
          .toMap
        val children:List[VNode] = n.nonEmptyChildren
          .filter(isntWhitespace)
          .map(fromXml)
          .toList

        VNode(n.label, attrs, children)
      }
    }
  }

  object VDomHelpers extends VDomHelpers
  trait VDomHelpers {
    def node(child:VNodeTransformTree*):VNodeTransformTree = VNodeTransformTree(List(), child.toList)
    def text(t:String) = VNode(pcdata, Map(), List(), Some(t))

    implicit class EnhancedVNodeTransformTree(t:VNodeTransformTree) {
      def withTransforms(transform:VNodeTransform*) = t.copy(transforms = transform.toList)
    }
  }

}
