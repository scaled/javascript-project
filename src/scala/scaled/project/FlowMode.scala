//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.major.ReadingMode
import scaled.util.{Errors, Process}

@Minor(name="flow", tags=Array("project"), stateTypes=Array(classOf[FlowProject.Tag]),
       desc="""A minor mode that provides Flow-related fns.""")
class FlowMode (env :Env, major :ReadingMode) extends MinorMode(env) {

  override def keymap = super.keymap.
    bind("show-type-at-point", "M-t", "C-c C-d").
    bind("visit-element", "M-.");

  //
  // FNs

  @Fn("Describes the type at the point.")
  def showTypeAtPoint () {
    flowAt(view.point(), "type-at-pos", Seq("--strip-root", curPath)).onSuccess(lines => {
      val maxWidth = view.width()-2
      val wrapped = if (!lines.exists(_.length > maxWidth)) lines
      else {
        val wbuf = Seq.builder[String]
        for (line <- lines) {
          if (line.length <= maxWidth) wbuf += line
          else for (seg <- line.grouped(maxWidth)) wbuf += seg
        }
        wbuf.build
      }
      view.popup() = Popup.text(wrapped, Popup.UpRight(view.point()))
    })
  }

  @Fn("""Navigates to the referent of the element at the point.""")
  def visitElement () {
    flowAt(view.point(), "get-def", Seq(curPath)).map(lines => lines(0) match {
      case GetDefRes(file, startl, startc, endl, endc) =>
        if (file == "") throw Errors.feedback(s"No file for definition: ${lines(0)}")
        window.visitStack.push(env.view) // push current loc to the visit stack
        val view = window.focus.visitFile(Store(file))
        view.point() = Loc(startl.toInt-1, startc.toInt-1)
      case _ =>
        view.popup() = Popup.text(Seq("Failed to parse get-def result:") ++ lines,
                                  Popup.UpRight(view.point()))
    })
  }

  private def curPath =
    (buffer.store.file || (throw Errors.feedback("Buffer is not visiting a file."))).toString

  private def flowAt (p :Loc, cmd :String, args :Seq[String]) :Future[Seq[String]] =
    flow(Seq(cmd) ++ args ++ Seq(s"${p.row+1}", s"${p.col+1}"))

  private def flow (args :Seq[String]) :Future[Seq[String]] =
    Process.exec(editor.exec, Seq("flow") ++ args).map(res => {
      if (res.exitCode == 0) res.stdout
      else throw Errors.feedback(fail(res, args).mkString("\n"))
    })

  private def fail (res :Process.Result, cmd :Seq[String]) = {
    val sb = Seq.builder[String]()
    sb += s"flow failed (${res.exitCode}):"
    sb += cmd.mkString("[", " ", "]")
    sb ++= res.stdout
    sb ++= res.stderr
    sb.build()
  }

  private val GetDefRes = """([^:]*):(\d+):(\d+),(\d+):(\d+)""".r
}
