//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman.{Pacman, RepoId, Filez}
import scaled.util.{BufferBuilder, Chars, Errors, SubProcess}

object FlowCompiler {
  // matches: "/foo/bar/baz.kt:LL: some error message"
  val outputM = Matcher.regexp("""^([^:]+):(\d+)$""")
}

abstract class FlowCompiler (proj :Project) extends Compiler(proj) {
  import FlowCompiler._

  /** Options to pass to `flow`. */
  def flowOpts :SeqV[String] = Seq()

  val log = proj.metaSvc.log

  override def describeEngine = "flow"

  override def describeOptions (bb :BufferBuilder) {
    bb.addKeyValue("flow: ", if (flowOpts.isEmpty) "<none>" else flowOpts.mkString(" "))
  }

  protected def compile (buffer :Buffer, file :Option[Path]) = {
    // now call down to the project which may copy things back into the output dir
    willCompile()

    val result = Promise[Boolean]()
    val cmd = Seq("flow") ++ flowOpts
    SubProcess(SubProcess.Config(cmd.toArray, cwd=proj.root.path),
               proj.metaSvc.exec, buffer, result.succeed)
    result
  }

  /** A hook called just before we initiate compilation. */
  protected def willCompile () {}

  protected def nextNote (buffer :Buffer, start :Loc) = {
    buffer.findForward(outputM, start) match {
      case Loc.None => Compiler.NoMoreNotes
      case ploc => try {
        val file = proj.root.path.resolve(outputM.group(1))
        val eline = outputM.group(2).toInt-1
        val ecol = 0 // outputM.group(3).toInt-1
        val ekind = "error" // outputM.group(4)
        // every line after the path with leading whitespace is part of the message
        val desc = Seq.builder[String]()
        var pnext = ploc.nextStart
        while (pnext < buffer.end && buffer.line(pnext).length > 0) {
          desc += buffer.line(pnext).asString
          pnext = pnext.nextStart
        }
        (Compiler.Note(Store(file), Loc(eline, ecol), desc.build(), ekind == "error"), pnext)
      } catch {
        case e :Exception => log.log("Error parsing error buffer", e) ; Compiler.NoMoreNotes
      }
    }
  }
}
