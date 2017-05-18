//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import codex.extract._
import codex.model._
import com.google.common.io.CharStreams
import java.io.{FileReader, InputStream, InputStreamReader, OutputStreamWriter, PrintWriter, Reader}
import java.nio.file.{Path, Paths}
import com.eclipsesource.json._
import scaled._

@Plugin(tag="codex-extractor")
class JavaScriptExtractorPlugin extends ExtractorPlugin {

  override val suffs = Set("js")

  // TODO: read the beginning of the file and look for @flow annotation?
  override def extractor (project :Project, suff :String) = flowExtractor(project)

  private def flowExtractor (project :Project) = Some(new FlowExtractor(project.root.path))

  private def tokenExtractor (project :Project) = Some(new TokenExtractor() {
    var rootPath = project.root.path.toString
    override def openUnit (source :Source, writer :Writer) = {
      val ref = super.openUnit(source, writer)
      // val fileName = source.fileName;
      var path = source.relativePath(rootPath)
      // path = path.substring(0, math.max(0, path.length-fileName.length-1))
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

object Flow {
  def readAST(root :Path, reader :Reader) :JsonObject = {
    val proc = new ProcessBuilder("flow", "ast").directory(root.toFile()).start()

    // read Flow's stderr to check for errors & stdout for the JSON AST
    val (stderrStream, stdoutStream) = (proc.getErrorStream(), proc.getInputStream())
    val (stderr, stdout) = (new StringBuffer(), new StringBuffer())
    def mkReader(stream :InputStream, buffer :StringBuffer) = new Thread() {
      override def run () {
        CharStreams.copy(new InputStreamReader(stream, "UTF-8"), buffer)
        stream.close()
      }
    }
    val stderrReader = mkReader(stderrStream, stderr)
    val stdoutReader = mkReader(stdoutStream, stdout)
    stderrReader.start()
    stdoutReader.start()

    // copy the source code to Flow's stdin
    val out = new OutputStreamWriter(proc.getOutputStream(), "UTF-8")
    CharStreams.copy(reader, out)
    out.flush()
    out.close()

    proc.waitFor()
    stderrReader.join()
    stdoutReader.join()
    // TODO: handle errors

    if (stderr.length > 0) println("STDERR: " + stderr)
    // println("STDOUT: " + stdout)

    Json.parse(stdout.toString()).asObject
  }

  def main(args :Array[String]) {
    val root = Paths.get(System.getProperty("user.dir"))
    val ast = readAST(root, new FileReader(args(0)))
    val source = CharStreams.toString(new FileReader(args(0)))
    val out = new PrintWriter(System.out)
    val writer = new DebugWriter(out, source)
    new FlowExtractor(root).process(ast, Ref.Global.ROOT, writer)
    out.flush()
  }
}

class FlowExtractor (root :Path) extends AbstractExtractor {

  override def process (source :Source, reader :Reader, writer :Writer) {
    writer.openUnit(source)

    // val fileName = source.fileName
    var path = source.relativePath(root.toString)
    // path = path.substring(0, math.max(0, path.length-fileName.length-1))
    val modRef = Ref.Global.ROOT.plus(path)
    writer.openDef(modRef, path, Kind.MODULE, Flavor.PACKAGE, true, Access.PUBLIC, 0, 0, 0)
    writer.emitSig(path)
    process(Flow.readAST(root, reader), modRef, writer)
    writer.closeDef()
    writer.closeUnit()
  }

  def process (ast :JsonObject, ref :Ref.Global, writer :Writer) {
    processBody(ast.get("body").asArray, ref, writer)
  }

  def processBody (body :JsonArray, ref :Ref.Global, writer :Writer) {
    for (stmt <- body.values.map(_.asObject)) {
      val loc = stmt.get("loc").asObject
      stmt.get("type").asString match {
        case "ExportNamedDeclaration" => {
          processDecls(stmt.get("declaration").asObject, true, ref, writer)
        }
        case "ImportDeclaration" => {} // TODO: usages
        case "VariableDeclaration" => processDecls(stmt, false, ref, writer)
        case "FunctionDeclaration" => processDecls(stmt, false, ref, writer)
        case "ClassDeclaration" => processDecls(stmt, false, ref, writer)
        case "TypeAlias" => processDecls(stmt, false, ref, writer)
        case "ClassProperty" => {
          val flavor = if (stmt.get("static").asBoolean) Flavor.STATIC_FIELD else Flavor.FIELD
          val declRef = openDecl(Kind.VALUE, flavor, stmt, "key", "value", true, ref, writer)
          writer.emitSig(appendIdentSig(stmt.get("key").asObject).toString)
          closeDecl(writer)
        }
        case "MethodDefinition" => {
          val flavor = if (stmt.get("static").asBoolean) Flavor.STATIC_METHOD else Flavor.METHOD
          val declRef = openDecl(Kind.FUNC, flavor, stmt, "key", "value", true, ref, writer)
          writer.emitSig(appendMethodSig(stmt).toString)
          closeDecl(writer)
        }
        case _ => println("TODO (stmt): " + stmt.get("type"))
      }
    }
  }

  def processDecls (decls :JsonObject, exported :Boolean, ref :Ref.Global, writer :Writer) {
    decls.get("type").asString match {
      case "VariableDeclaration" => {
        val kind = decls.get("kind").asString
        for (decl <- decls.get("declarations").asArray.map(_.asObject)) {
          val flavor = if (exported) Flavor.NONE else Flavor.LOCAL // TODO: flavor for exported vars
          val declRef = openDecl(Kind.VALUE, flavor, decl, "id", "init", exported, ref, writer)
          writer.emitSig(appendVarSig(kind, decl).toString)
          // { type: 'VariableDeclarator',
          //   loc:
          //    { source: null,
          //      start: { line: 6, column: 13 },
          //      end: { line: 16, column: 1 } },
          //   range: [ 101, 204 ],
          //   id:
          //    { type: 'Identifier',
          //      loc: { source: null, start: [Object], end: [Object] },
          //      range: [ 101, 115 ],
          //      name: 'REACTION_NAMES',
          //      typeAnnotation: null,
          //      optional: false },
          //   init:
          //    { type: 'ArrayExpression',
          //      loc: { source: null, start: [Object], end: [Object] },
          //      range: [ 118, 204 ],
          //      elements:
          //       [ [Object],
          //         [Object],
          //         [Object],
          //         [Object],
          //         [Object],
          //         [Object],
          //         [Object],
          //         [Object],
          //         [Object] ] } }
          closeDecl(writer)
        }
      }

      case "TypeAlias" => {
        // TODO: Flavor for type defs
        openDecl(Kind.TYPE, Flavor.NONE, decls, "id", "right", exported, ref, writer)
        writer.emitSig(appendAliasSig(decls).toString)
        // { type: 'TypeAlias',
        //   loc:
        //    { source: null,
        //      start: { line: 18, column: 7 },
        //      end: { line: 27, column: 2 } },
        //   range: [ 214, 694 ],
        //   id:
        //    { type: 'Identifier',
        //      loc: { source: null, start: [Object], end: [Object] },
        //      range: [ 219, 227 ],
        //      name: 'Question',
        //      typeAnnotation: null,
        //      optional: false },
        //   typeParameters: null,
        //   right:
        //    { type: 'ObjectTypeAnnotation',
        //      loc: { source: null, start: [Object], end: [Object] },
        //      range: [ 230, 693 ],
        //      exact: false,
        //      properties:
        //       [ [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object] ],
        //      indexers: [],
        //      callProperties: [] } }
        closeDecl(writer)
      }

      case "FunctionDeclaration" => {
        // TODO: flavor for top-level functions
        openDecl(Kind.FUNC, Flavor.NONE, decls, "id", "body", exported, ref, writer)
        writer.emitSig(appendFuncSig(decls).toString)
        // { type: 'FunctionDeclaration',
        //   loc:
        //    { source: null,
        //      start: { line: 26, column: 0 },
        //      end: { line: 47, column: 1 } },
        //   range: [ 1162, 1950 ],
        //   id:
        //    { type: 'Identifier',
        //      loc: { source: null, start: [Object], end: [Object] },
        //      range: [ 1171, 1181 ],
        //      name: 'makeButton',
        //      typeAnnotation: null,
        //      optional: false },
        //   params:
        //    [ { type: 'Identifier',
        //        loc: [Object],
        //        range: [Object],
        //        name: 'presence',
        //        typeAnnotation: null,
        //        optional: false },
        //      { type: 'Identifier',
        //        loc: [Object],
        //        range: [Object],
        //        name: 'name',
        //        typeAnnotation: null,
        //        optional: false },
        //      { type: 'Identifier',
        //        loc: [Object],
        //        range: [Object],
        //        name: 'id',
        //        typeAnnotation: null,
        //        optional: false } ],
        //   body:
        //    { type: 'BlockStatement',
        //      loc: { source: null, start: [Object], end: [Object] },
        //      range: [ 1202, 1950 ],
        //      body:
        //       [ [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object] ] },
        //   async: false,
        //   generator: false,
        //   predicate: null,
        //   expression: false,
        //   returnType: null,
        //   typeParameters: null }
        closeDecl(writer)
      }

      case "ClassDeclaration" => {
        val classRef = openDecl(Kind.TYPE, Flavor.CLASS, decls, "id", "body", exported, ref, writer)
        writer.emitSig(appendClassSig(decls).toString)
        processDecls(decls.get("body").asObject, false, classRef, writer)
        // { type: 'ClassDeclaration',
        //   loc:
        //    { source: null,
        //      start: { line: 49, column: 0 },
        //      end: { line: 121, column: 1 } },
        //   range: [ 1952, 4165 ],
        //   id:
        //    { type: 'Identifier',
        //      loc: { source: null, start: [Object], end: [Object] },
        //      range: [ 1958, 1968 ],
        //      name: 'UrlsDialog',
        //      typeAnnotation: null,
        //      optional: false },
        //   body:
        //    { type: 'ClassBody',
        //      loc: { source: null, start: [Object], end: [Object] },
        //      range: [ 1969, 4165 ],
        //      body:
        //       [ [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object],
        //         [Object] ] },
        //   superClass: null,
        //   typeParameters: null,
        //   superTypeParameters: null,
        //   implements: [],
        //   decorators: [] }
        closeDecl(writer)
      }

      case "ClassBody" => processBody(decls.get("body").asArray, ref, writer)

      case "ClassProperty" => println("TODO: ClassProperty") ; dump(decls)

      case "MethodDefinition" => println("TODO: MethodDefinition") ; dump(decls)

      case _ => println("TODO (decl): " + decls.get("type"))
    }
  }

  def openDecl (
    kind :Kind, flavor :Flavor, decl :JsonObject, idName :String, bodyName :String,
    exported :Boolean, ref :Ref.Global, writer :Writer
  ) :Ref.Global = {
    val id = decl.get(idName).asObject
    val idType = id.get("type").asString
    if (idType != "Identifier") {
      println(s"Don't know how to handle id type: ${idType}")
      return ref
    }
    val offset = id.get("range").asArray.get(0).asInt
    val name = id.get("name").asString
    val declRef = ref.plus(name)
    val access = if (exported) Access.PUBLIC else Access.LOCAL
    val (bodyStart, bodyEnd) = decl.get(bodyName) match {
      case null => (0, 0)
      case init if (init.isNull) => (0, 0)
      case init =>
        val range = init.asObject.get("range").asArray
        (range.get(0).asInt, range.get(1).asInt)
    }
    writer.openDef(declRef, name, kind, flavor, exported, access, offset, bodyStart, bodyEnd)
    declRef
  }

  def closeDecl (writer :Writer) {
    writer.closeDef()
  }

  def appendVarSig(kind :String, decl :JsonObject, into :StringBuffer = new StringBuffer) :StringBuffer = {
    if (kind.length > 0) into.append(kind).append(" ")
    appendIdentSig(decl.get("id").asObject, into)
  }

  def appendAliasSig(decl :JsonObject, into :StringBuffer = new StringBuffer) :StringBuffer = {
    into.append("type ")
    appendIdentSig(decl.get("id").asObject, into)
    // TODO: more of the type?
  }

  def appendFuncSig(decl :JsonObject, into :StringBuffer = new StringBuffer) :StringBuffer = {
    into.append("function ")
    appendIdentSig(decl.get("id").asObject, into)
    appendTypeParamsSig(decl.get("typeParameters"), into)
    appendParamsSig(decl, into)
    appendReturnTypeSig(decl, into)
    into
  }

  def appendMethodSig(decl :JsonObject, into :StringBuffer = new StringBuffer) :StringBuffer = {
    appendIdentSig(decl.get("key").asObject, into)
    val funcExpr = decl.get("value").asObject
    appendTypeParamsSig(funcExpr.get("typeParameters"), into)
    appendParamsSig(funcExpr, into)
    appendReturnTypeSig(funcExpr, into)
    into
  }

  def appendTypeParamsSig(typeParamsVal :JsonValue, into :StringBuffer) {
    if (!typeParamsVal.isNull()) {
      into.append("<")
      if (typeParamsVal.isArray()) {
        val params = typeParamsVal.asArray
        for (ii <- 0 until params.size) {
          if (ii > 0) into.append(",")
          appendTypeSig(params.get(ii).asObject, into)
        }
      } else if (typeParamsVal.isObject()) {
        appendTypeSig(typeParamsVal.asObject, into)
      } else {
        println("Unknown type params value:");
        dump(typeParamsVal)
        into.append("?")
      }
      into.append(">")
    }
  }

  def appendParamsSig(decl :JsonObject, into :StringBuffer) {
    into.append("(")
    val paramsVal = decl.get("params")
    if (!paramsVal.isNull()) {
      val params = paramsVal.asArray
      for (ii <- 0 until params.size) {
        if (ii > 0) into.append(", ")
        appendParamSig(params.get(ii).asObject, into)
      }
    }
    into.append(")")
  }

  def appendParamSig(param :JsonObject, into :StringBuffer) {
    param.get("type").asString match {
      case "Identifier" => appendIdentSig(param, into)
      case "AssignmentPattern" => {
        appendIdentSig(param.get("left").asObject)
        into.append("=...") // TODO: 'right'
      }
      case _ => println("param TODO!") ; dump(param)
    }
  }

  def appendReturnTypeSig(decl :JsonObject, into :StringBuffer) {
    val returnVal = decl.get("returnType")
    if (!returnVal.isNull()) {
      appendTypeSig(returnVal.asObject, into)
    }
  }

  def appendClassSig(decl :JsonObject, into :StringBuffer = new StringBuffer) :StringBuffer = {
    into.append("class ")
    appendIdentSig(decl.get("id").asObject, into)
    appendTypeParamsSig(decl.get("typeParameters"), into)
    // TODO: extends, implements?
    into
  }

  def appendIdentSig(ident :JsonObject, into :StringBuffer = new StringBuffer) :StringBuffer = {
    try {
      val name = ident.get("name").asString
      into.append(name)
      val typeAnno = ident.get("typeAnnotation")
      if (typeAnno != null && !typeAnno.isNull()) {
        appendTypeSig(typeAnno.asObject, into)
      }
      into
    } catch {
      case e :Throwable => println("ACK " + e) ; dump(ident) ; throw e
    }
  }
  def emitIdentSigUses(writer :Writer, identStart :Int, ident :JsonObject) {
  }

  def appendTypeSig(typeAnno :JsonObject, into :StringBuffer) :StringBuffer = {
    typeAnno.get("type").asString match {
      case "TypeAnnotation" => {
        into.append(" :")
        appendTypeSig(typeAnno.get("typeAnnotation").asObject, into)
      }
      case "Identifier" => appendIdentSig(typeAnno, into)
      case "StringTypeAnnotation" => into.append("string")
      case "BooleanTypeAnnotation" => into.append("boolean")
      case "NumberTypeAnnotation" => into.append("number")
      case "VoidTypeAnnotation" => into.append("void")
      case "AnyTypeAnnotation" => into.append("any")
      case "NullableTypeAnnotation" => {
        into.append("?")
        appendTypeSig(typeAnno.get("typeAnnotation").asObject, into)
      }
      case "GenericTypeAnnotation" => {
        try {
          appendTypeSig(typeAnno.get("id").asObject, into)
        } catch {
          case e :Throwable =>
            println("ACK " + e);
            dump(typeAnno)
            throw e
        }
        appendTypeTypeParamsSig(typeAnno.get("typeParameters"), into)
      }
      case "UnionTypeAnnotation" => {
        val types = typeAnno.get("types").asArray
        for (ii <- 0 until types.size) {
          if (ii > 0) into.append("|")
          appendTypeSig(types.get(ii).asObject, into)
        }
      }
      case "QualifiedTypeIdentifier" => {
        appendTypeSig(typeAnno.get("qualification").asObject, into)
        into.append(".")
        appendTypeSig(typeAnno.get("id").asObject, into)
        appendTypeTypeParamsSig(typeAnno.get("typeParameters"), into)
      }
      case "FunctionTypeAnnotation" => {
        into.append("(")
        val params = typeAnno.get("params").asArray
        for (ii <- 0 until params.size) {
          if (ii > 0) into.append(", ")
          appendTypeSig(params.get(ii).asObject, into)
        }
        into.append(") => ")
        appendTypeSig(typeAnno.get("returnType").asObject, into)
      }
      case "FunctionTypeParam" => {
        appendIdentSig(typeAnno.get("name").asObject, into)
        into.append(" :")
        // TODO: handle "optional"
        appendTypeSig(typeAnno.get("typeAnnotation").asObject, into)
      }
      case "TypeParameterInstantiation" => {
        appendTypeTypeParamsSig(typeAnno.get("params"), into)
      }
      case "ArrayTypeAnnotation" => {
        appendTypeSig(typeAnno.get("elementType").asObject, into)
        into.append("[]")
      }
      case _ => println("TODO: type") ; dump(typeAnno) ; into.append(s"<TODO: ${typeAnno.get("type")}")
    }
    into
  }

  def appendTypeTypeParamsSig(paramsVal :JsonValue, into :StringBuffer) {
    if (paramsVal != null && !paramsVal.isNull()) {
      if (paramsVal.isArray()) {
        val params = paramsVal.asArray
        into.append("<")
        for (ii <- 0 until params.size) {
          if (ii > 0) into.append(",")
          appendTypeSig(params.get(ii).asObject, into)
        }
        into.append(">")
      } else if (paramsVal.isObject()) {
        into.append("<")
        appendTypeSig(paramsVal.asObject, into)
        into.append(">")
      } else {
        println("Unknown type type params value:");
        dump(paramsVal)
      }
    }
  }

  def emitTypeSigUses(writer :Writer, identStart :Int, typeAnno :JsonObject) {
  }

  def dump(value :JsonValue) {
    println(value.toString(PrettyPrint.indentWithSpaces(2)))
  }
}
