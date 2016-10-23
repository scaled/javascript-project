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
    bind("show-type-at-point", "M-t", "C-c C-d");

  //
  // FNs

  @Fn("Describes the type at the point.")
  def showTypeAtPoint () = {
    val path = buffer.store.file || (throw Errors.feedback("Buffer is not visiting a file."))
    val p = view.point()
    flow(Seq("type-at-pos", "--strip-root", path.toString(), s"${p.row+1}", s"${p.col+1}")).
      onSuccess(lines => view.popup() = Popup.text(lines, Popup.UpRight(view.point())))
  }

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
}
