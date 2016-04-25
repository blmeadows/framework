package net.liftweb.http

import java.io.{FileWriter, File}

import com.gargoylesoftware.htmlunit.BrowserVersion._
import com.gargoylesoftware.htmlunit.WebClient
import net.liftweb.http.js.JE
import net.liftweb.json.Extraction
import net.liftweb.util.{VDom, Html5}
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import net.sourceforge.htmlunit.corejs.javascript. { ScriptableObject, Function => JsFunction }
import com.gargoylesoftware.htmlunit.html.{DomNode, DomElement, HtmlElement, HtmlPage}

import scala.xml._
import scala.collection.JavaConverters._

object UpdateDOMSpec extends Specification with XmlMatchers {
  implicit val formats = VDom.formats

  "UpdateDOM Spec".title

  def updateAndCompare(before:Node, after:Node) = {
    val lift_js = this.getClass.getClassLoader.getResource("toserve/lift.js")
    val jq_js   = this.getClass.getClassLoader.getResource("toserve/jquery-1.4.4.js")
    def html(body:Node) =
      <html>
        <head>
          <meta charset="UTF-8"/>
          <title>Home</title>
          <script type="application/javascript" language="javascript" src={jq_js.toURI.toASCIIString}></script>
          <script type="application/javascript" language="javascript" src={lift_js.toURI.toASCIIString}></script>
        </head>
        {body}
      </html>

    def toXml(e:DomNode):Node = {
      val attrs:MetaData = (0 until e.getAttributes.getLength).foldLeft(Null:MetaData) {
        case (acc, i) =>
          val attr = e.getAttributes.item(i)
          new UnprefixedAttribute(attr.getNodeName, Text(attr.getNodeValue), acc)
      }
      val children = e.getChildren.asScala.map(toXml).toSeq
      if(e.getNodeName == "#text") Text(e.getTextContent)
      else Elem(null, e.getNodeName, attrs, TopScope, true, children:_*)
    }

    val diff = VDom.diff(before, after)

    val js = JE.Call("lift.updateBody", Extraction.decompose(diff)).toJsCmd

    val file = File.createTempFile("test", "html")
    try {
      val w = new FileWriter(file)
      w.write("<!DOCTYPE html>")
      Html5.write(html(before), w, true, true)
      w.close()

      val client = new WebClient(CHROME)
      val options = client.getOptions()
      options.setHomePage(WebClient.URL_ABOUT_BLANK.toString())
      options.setJavaScriptEnabled(true)

      client.getPage(file.toURI.toURL)
      val window = client.getCurrentWindow().getTopWindow
      val page:HtmlPage = window.getEnclosedPage().asInstanceOf[HtmlPage] // asInstanceOf because ... java...

      def exec(js:String):String = {
        val toRun = "function() {\n"+js+"\n};"
        val result = page.executeJavaScript(toRun)
        val func:JsFunction = result.getJavaScriptResult().asInstanceOf[JsFunction]

        val exeResult = page.executeJavaScriptFunctionIfPossible(
          func,
          window.getScriptableObject(),
          Array.empty,
          page.getDocumentElement()
        )

        exeResult.getJavaScriptResult.toString
      }

      exec(js)

      Utility.trim(toXml(page.getBody)) must beEqualTo(Utility.trim(after))
      // toXml(page.getBody) must beEqualToIgnoringSpaceOrdered(after) // need specs2 3.7.3
      // switched from beEqualToIgnoringSpace because it doesn't check inside the elements
      // resulting in false positives
    } finally {
      file.delete()
    }
  }

  "UpdateDOM" should {
    "append an element" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 3</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      updateAndCompare(before, after)
    }

    "append two elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      updateAndCompare(before, after)
    }

    "remove an element" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
            </ul>
          </div>
        </body>

      updateAndCompare(before, after)
    }

    "find reordered elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 1</li>
            </ul>
          </div>
        </body>

      updateAndCompare(before, after)
    }//.pendingUntilFixed("Not doing reordering yet")

    "find more reordered elements" in {
      val before =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 1</li>
              <li>Message 2</li>
              <li>Message 3</li>
              <li>Message 4</li>
              <li>Message 5</li>
            </ul>
          </div>
        </body>

      val after =
        <body data-lift-content-id="main">
          <div>
            <hr/>
            <ul>
              <li>Message 3</li>
              <li>Message 2</li>
              <li>Message 1</li>
              <li>Message 5</li>
              <li>Message 4</li>
            </ul>
          </div>
        </body>

      updateAndCompare(before, after)
    }//.pendingUntilFixed("Not doing reordering yet")
  }

}
