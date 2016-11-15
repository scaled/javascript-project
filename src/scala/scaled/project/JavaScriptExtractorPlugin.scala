//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import codex.extract.Extractor
import codex.extract.TokenExtractor
import codex.extract.Writer
import codex.model._
import scaled._

@Plugin(tag="codex-extractor")
class JavaScriptExtractorPlugin extends ExtractorPlugin {

  override val suffs = Set("js")

  override def extractor (project :Project, suff :String) = Some(new TokenExtractor() {
    var rootPath = project.root.path.toString
    override def openUnit (source :Source, writer :Writer) = {
      val ref = super.openUnit(source, writer)
      val fileName = source.fileName;
      var path = source.relativePath(rootPath)
      path = path.substring(0, math.max(0, path.length-fileName.length-1))
      val modref = ref.plus(path);
      writer.openDef(modref, path, Kind.MODULE, Flavor.PACKAGE, true,
                     Access.PUBLIC, 0, 0, 0)
      writer.emitSig(path);
      modref
    }
    override def closeUnit (source :Source, writer :Writer) {
      writer.closeDef()
      super.closeUnit(source, writer)
    }
  })
}
