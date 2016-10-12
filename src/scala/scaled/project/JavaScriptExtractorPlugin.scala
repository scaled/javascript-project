//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import codex.extract.Extractor
import codex.extract.TokenExtractor
import scaled._

@Plugin(tag="codex-extractor")
class JavaScriptExtractorPlugin extends ExtractorPlugin {

  override val suffs = Set("js")

  override def extractor (project :Project, suff :String) = Some(new TokenExtractor())
}
