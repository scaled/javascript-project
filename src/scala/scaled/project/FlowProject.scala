//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, SimpleFileVisitor, FileVisitResult}
import scaled._
import scaled.util.Close
import scaled.pacman.{Config => PConfig}

object FlowProject {

  // val ConfigFile = ".flowconfig"
  val ProjectFile = ".flowproject"

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("flow", true, classOf[FlowProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, ProjectFile)) 1 else -1
  }

  // used to trigger flow-mode
  class Tag
}

class FlowProject (ps :ProjectSpace, r :Project.Root) extends AbstractFileProject(ps, r) {
  import FlowProject._

  class MetaData (config :PConfig) {
    val name = configS("name")
    val sourceDirs = configSL("sourceDir")
    val ignoreNames = configSL("ignoreName")
    val ignoreRegexes = configSL("ignoreRegex")
    config.finish()
    private def configS (name :String) = config.resolve(name, PConfig.StringP)
    private def configSL (name :String) = config.resolve(name, PConfig.StringListP)
  }

  // use our ignores when enumerating sources
  override def onSources (op :Path => Unit) :Unit = onFiles(sourceDirs, op)

  private[this] val meta = new Close.Ref[MetaData](toClose) {
    protected def create = new MetaData(new PConfig(Files.readAllLines(configFile)))
  }
  private def rootPath = root.path
  private def configFile = rootPath.resolve(ProjectFile)

  // hibernate if the config file changes, which will trigger reload
  metaSvc.service[WatchService].watchFile(configFile, file => hibernate())
  // note that we don't 'close' our watches, we'll keep them active for the lifetime of the editor
  // because it's low overhead; I may change my mind on this front later, hence this note

  override protected def computeMeta (oldMeta :Project.Meta) = {
    val sb = FileProject.stockIgnores
    meta.get.ignoreNames.foreach { sb += FileProject.ignoreName(_) }
    meta.get.ignoreRegexes.foreach { sb += FileProject.ignoreRegex(_) }
    ignores() = sb

    addComponent(classOf[Compiler], new FlowCompiler(this) {
      // override def flowOpts = FlowProject.this.flowOpts
      // override protected def willCompile () = copyResources()
    })

    Future.success(oldMeta.copy(
      name = meta.get.name,
      sourceDirs = meta.get.sourceDirs.map(rootPath.resolve(_)).toSeq
    ))
  }

  override def addToBuffer (buffer :RBuffer) {
    super.addToBuffer(buffer)
    buffer.state[Tag]() = new Tag
  }

  def onFiles (dirs :SeqV[Path], op :Path => Unit) :Unit =
    dirs.filter(Files.exists(_)) foreach { dir =>
      // TODO: should we be following symlinks? likely so...
      Files.walkFileTree(dir, new SimpleFileVisitor[Path]() {
        override def visitFile (file :Path, attrs :BasicFileAttributes) = {
          if (!attrs.isDirectory) op(file)
          FileVisitResult.CONTINUE
        }
        override def preVisitDirectory (dir :Path, attrs :BasicFileAttributes) = {
          if (ignore(dir)) FileVisitResult.SKIP_SUBTREE
          else FileVisitResult.CONTINUE
        }
      })
    }
}
