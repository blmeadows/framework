package net.liftweb.util

import net.liftweb.json._

import scala.xml.{Text, UnprefixedAttribute, Node}

object VDom {
  val pcdata = "#PCDATA"
  case class VNode(tag:String, attributes:Map[String, String] = Map(), children:List[VNode] = List(), text:Option[String] = None)

  trait VNodeTransform
  case class VNodeInsert(position:Int, node:VNode) extends VNodeTransform
  case class VNodeDelete(position:Int) extends VNodeTransform
  case class VNodeReorder(before:Int, after:Int) extends VNodeTransform

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

    val aChildren = a.nonEmptyChildren.filter(isntWhitespace).toList // before
    val bChildren = b.nonEmptyChildren.filter(isntWhitespace).toList // after

    val additions = if (aChildren.size < bChildren.size) {
      bChildren.diff(aChildren).map {
        case c => VNodeInsert(bChildren.indexOf(c), VNode.fromXml(c))
      }
    }
    else Nil

    val deletions = if (aChildren.size > bChildren.size) {
      aChildren.diff(bChildren).map {
        case c => VNodeDelete(aChildren.indexOf(c))
      }
    }
    else Nil

    val indexes = if(aChildren.size == bChildren.size) {
      aChildren.zip(bChildren).collect {
        case (ca, cb) => bChildren.indexOf(ca)
        case _ => -1
      }
    }
    else Nil

    val reorderings = sort(indexes.filterNot(_ == -1).toArray).filterNot(_ == -1).map(a => VNodeReorder(a, a-1))

    val transforms =  additions ++ deletions ++ reorderings

    val children = aChildren.zip(bChildren)
      .collect {
        case (ca, cb) if ca != cb => diff(ca, cb)     // This != check probably would benefit from memoizing
        case _ => VNodeTransformTree(List(), List())  // No changes for this node, make a placeholder
      }

    VNodeTransformTree(transforms, children)
  }

  def sort(input: Array[Int]): List[Int] = {
    var changes = List(-1)
    val limit = input.length - 1
    for (a <- 1 to limit) {
      for (b <- limit to a by -1) {
        if (input(b) < input(b-1)) { // need to get b value anytime in this loop!
          changes = changes:::List(b)
          val x = input(b)
          input(b) = input(b - 1)
          input(b - 1) = x
        }
      }
    }
    changes
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
