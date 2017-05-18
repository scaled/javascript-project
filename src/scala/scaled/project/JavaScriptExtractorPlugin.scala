//
// Scaled JavaScript Project Support - JavaScript project support for Scaled project framework.
// http://github.com/scaled/javascript-project/blob/master/LICENSE

package scaled.project

import codex.extract._
import codex.model._
import com.eclipsesource.json._
import com.google.common.io.CharStreams
import java.io.{FileReader, InputStream, InputStreamReader, OutputStreamWriter, PrintWriter, Reader}
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer
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
    val path = Paths.get(args(0))
    val out = new PrintWriter(System.out)
    val source = CharStreams.toString(new FileReader(args(0)))
    new FlowExtractor(root).process(
      new Source.File(path), Files.newBufferedReader(path), new DebugWriter(out, source))
    out.flush()
  }
}

class SigBuilder {
  val buffer = new StringBuffer
  val uses = new ArrayBuffer[(Kind, Int, String)]

  def append (text :String) = {
    buffer.append(text)
    this
  }

  def appendRef (kind :Kind, name :String) = {
    val start = buffer.length
    this.append(name)
    this.uses += ((kind, start, name))
    this
  }

  def emit(writer :Writer) {
    writer.emitSig(buffer.toString)
    for ((kind, offset, name) <- uses) {
      writer.emitSigUse(Ref.Global.ROOT.plus(name), kind, offset, name)
    }
  }
}

class FlowExtractor (root :Path) extends AbstractExtractor {

  override def process (source :Source, reader :Reader, writer :Writer) {
    writer.openUnit(source)
    var path = source.relativePath(root.toString)
    val ast = Flow.readAST(root, reader);
    val range = ast.asObject.get("range").asArray
    val (start, end) = (range.get(0).asInt, range.get(1).asInt)
    val modRef = Ref.Global.ROOT.plus(path)
    writer.openDef(modRef, path, Kind.MODULE, Flavor.PACKAGE, true, Access.PUBLIC, 0, start, end)
    new SigBuilder().appendRef(Kind.MODULE, path).emit(writer)
    process(ast, modRef, writer)
    writer.closeDef()
    writer.closeUnit()
  }

  def process (ast :JsonObject, ref :Ref.Global, writer :Writer) {
    processBody(ast.get("body").asArray, ref, writer)
  }

  def processBody (body :JsonArray, ref :Ref.Global, writer :Writer) {
    for (stmt <- body.values.map(_.asObject)) {
      processStmt(stmt, false, ref, writer)
    }
  }

  def processStmt (stmt :JsonObject, exported :Boolean, ref :Ref.Global, writer :Writer) {
    stmt.get("type").asString match {
      case "ExportNamedDeclaration"|"ExportDefaultDeclaration" =>
        val decl = stmt.get("declaration")
        if (decl.isObject) {
          processStmt(decl.asObject, true, ref, writer)
        } else {
          if (stmt.get("specifiers") == null) {
            println("TODO: non-obj decl")
            dump(stmt)
          } // else it's a reexport 'export { foo } from ...' should we note it?
        }

      case "ImportDeclaration" => {} // TODO: usages

      case "VariableDeclaration" =>
        val kind = stmt.get("kind").asString
        for (decl <- stmt.get("declarations").asArray.map(_.asObject)) {
          val flavor = if (exported) Flavor.NONE else Flavor.LOCAL // TODO: flavor for exported vars
          val id = decl.get("id").asObject
          id.get("type").asString match {
            case "Identifier" =>
              val declRef = openDecl(Kind.VALUE, flavor, decl, "id", "init", exported, ref, writer)
              appendVarSig(kind, decl).emit(writer)
              closeDecl(writer)

            case "ObjectPattern" =>
              for (prop <- id.get("properties").asArray.map(_.asObject)) {
                val declRef = openDecl(Kind.VALUE, flavor, prop, "key", null, exported, ref, writer)
                appendIdentSig(Kind.VALUE, prop.get("key").asObject, new SigBuilder).emit(writer)
                closeDecl(writer)
              }
              // TODO: whole decl can have type annotation, we could extract the types and pair
              // them up with the decls, but blah

            case _ => println("TODO vardecl: " + id.get("type")) ; dump(stmt)
          }
        }

      case "TypeAlias" =>
        // TODO: Flavor for type defs
        openDecl(Kind.TYPE, Flavor.NONE, stmt, "id", "right", exported, ref, writer)
        appendAliasSig(stmt).emit(writer)
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

      case "FunctionDeclaration" =>
        // TODO: flavor for top-level functions
        val funcRef = openDecl(Kind.FUNC, Flavor.NONE, stmt, "id", "body", exported, ref, writer)
        appendFuncSig(stmt).emit(writer)
        processStmt(stmt.get("body").asObject, false, funcRef, writer)
        closeDecl(writer)

      case "ClassDeclaration" =>
        val classRef = openDecl(Kind.TYPE, Flavor.CLASS, stmt, "id", "body", exported, ref, writer)
        appendClassSig(stmt).emit(writer)
        processStmt(stmt.get("body").asObject, false, classRef, writer)
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

      case "ClassBody" => processBody(stmt.get("body").asArray, ref, writer)

      case "BlockStatement" => processBody(stmt.get("body").asArray, ref, writer)

      case "ClassProperty" =>
        val flavor = if (stmt.get("static").asBoolean) Flavor.STATIC_FIELD else Flavor.FIELD
        val declRef = openDecl(Kind.VALUE, flavor, stmt, "key", "value", true, ref, writer)
        val sig = new SigBuilder
        appendIdentSig(Kind.VALUE, stmt.get("key"), sig)
        val typeAnno = stmt.get("typeAnnotation")
        if (typeAnno != null && !typeAnno.isNull()) {
          appendTypeSig(typeAnno.asObject, sig)
        }
        sig.emit(writer)
        closeDecl(writer)

      case "MethodDefinition" =>
        val flavor = if (stmt.get("static").asBoolean) Flavor.STATIC_METHOD else Flavor.METHOD
        val declRef = openDecl(Kind.FUNC, flavor, stmt, "key", "value", true, ref, writer)
        appendMethodSig(stmt).emit(writer)
        closeDecl(writer)

      case "ExpressionStatement" => processExpr(stmt.get("expression").asObject, ref, writer)
      case "ReturnStatement" => processExpr(stmt.get("argument").asObject, ref, writer)

      case "ForOfStatement" => {} // TODO
      case "ForInStatement" => {} // TODO
      case "ForStatement" => {} // TODO
      case "IfStatement" => {} // TODO

      case _ => println("TODO (stmt): " + stmt.get("type"))
    }
  }

  def processExpr (expr :JsonObject, ref :Ref.Global, writer :Writer) {
    // TODO
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
    val access = if (!exported) Access.LOCAL else if (name.startsWith("_")) Access.PROTECTED else Access.PUBLIC
    val (bodyStart, bodyEnd) = if (bodyName == null) (0, 0) else decl.get(bodyName) match {
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

  def appendVarSig(kind :String, decl :JsonObject, into :SigBuilder = new SigBuilder) :SigBuilder = {
    if (kind.length > 0) into.append(kind).append(" ")
    decl.get("type").asString match {
      case "VariableDeclarator" => appendIdentSig(Kind.VALUE, decl.get("id"), into)
      case _ => println("TODO varsig") ; dump(decl)
    }
    into
  }

  def appendAliasSig(decl :JsonObject, into :SigBuilder = new SigBuilder) :SigBuilder = {
    into.append("type ")
    appendIdentSig(Kind.TYPE, decl.get("id"), into)
    // TODO: more of the type?
  }

  def appendFuncSig(decl :JsonObject, into :SigBuilder = new SigBuilder) :SigBuilder = {
    into.append("function ")
    appendIdentSig(Kind.FUNC, decl.get("id"), into)
    appendTypeParamsSig(decl.get("typeParameters"), into)
    appendParamsSig(decl, into)
    appendReturnTypeSig(decl, into)
    into
  }

  def appendMethodSig(decl :JsonObject, into :SigBuilder = new SigBuilder) :SigBuilder = {
    appendIdentSig(Kind.FUNC, decl.get("key"), into)
    val funcExpr = decl.get("value").asObject
    appendTypeParamsSig(funcExpr.get("typeParameters"), into)
    appendParamsSig(funcExpr, into)
    appendReturnTypeSig(funcExpr, into)
    into
  }

  def appendTypeParamsSig(typeParamsVal :JsonValue, into :SigBuilder) {
    if (!typeParamsVal.isNull()) {
      into.append("<")
      appendTypeTypeParamsSig(typeParamsVal, into);
      into.append(">")
    }
  }

  def appendParamsSig(decl :JsonObject, into :SigBuilder) {
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

  def appendParamSig(param :JsonObject, into :SigBuilder) {
    param.get("type").asString match {
      case "Identifier" => appendIdentSig(Kind.VALUE, param, into)
      case "AssignmentPattern" => {
        appendParamSig(param.get("left").asObject, into)
        into.append(" = ...") // TODO: 'right'
      }
      case "RestElement" => {
        into.append("...")
        appendParamSig(param.get("argument").asObject, into)
      }
      case "FunctionTypeParam" => {
        appendIdentSig(Kind.VALUE, param.get("name"), into)
        into.append(" :")
        // TODO: handle "optional"
        appendTypeSig(param.get("typeAnnotation").asObject, into)
      }
      case _ => println("param TODO!") ; dump(param)
    }
  }

  def appendReturnTypeSig(decl :JsonObject, into :SigBuilder) {
    val returnVal = decl.get("returnType")
    if (!returnVal.isNull()) {
      appendTypeSig(returnVal.asObject, into)
    }
  }

  def appendClassSig(decl :JsonObject, into :SigBuilder = new SigBuilder) :SigBuilder = {
    into.append("class ")
    appendIdentSig(Kind.TYPE, decl.get("id"), into)
    appendTypeParamsSig(decl.get("typeParameters"), into)
    // TODO: extends, implements?
    into
  }

  def appendIdentSig(kind :Kind, ident :JsonValue, into :SigBuilder) :SigBuilder = {
    try {
      if (ident.isObject) {
        val identObj = ident.asObject
        val name = identObj.get("name").asString
        // TODO: real ref? probably not
        into.appendRef(kind, name)
        val typeAnno = identObj.get("typeAnnotation")
        if (typeAnno != null && !typeAnno.isNull()) {
          appendTypeSig(typeAnno.asObject, into)
        }
      } else if (ident.isString) {
        val name = ident.asString
        into.appendRef(kind, name)
      } else {
        println("Unknown ident type " + ident)
      }
      into
    } catch {
      case e :Throwable => println("ACK " + e) ; dump(ident) ; throw e
    }
  }

  def appendTypeSig(typeAnno :JsonObject, into :SigBuilder) :SigBuilder = {
    typeAnno.get("type").asString match {
      case "TypeAnnotation" => {
        into.append(" :")
        appendTypeSig(typeAnno.get("typeAnnotation").asObject, into)
      }
      case "Identifier" => appendIdentSig(Kind.TYPE, typeAnno, into)
      case "StringTypeAnnotation" => into.appendRef(Kind.TYPE, "string")
      case "BooleanTypeAnnotation" => into.appendRef(Kind.TYPE, "boolean")
      case "NumberTypeAnnotation" => into.appendRef(Kind.TYPE, "number")
      case "VoidTypeAnnotation" => into.appendRef(Kind.TYPE, "void")
      case "AnyTypeAnnotation" => into.appendRef(Kind.TYPE, "any")
      case "NullLiteralTypeAnnotation" => into.appendRef(Kind.TYPE, "null")
      case "ExistsTypeAnnotation" => into.appendRef(Kind.TYPE, "*")
      case "NullableTypeAnnotation" => {
        into.append("?")
        appendTypeSig(typeAnno.get("typeAnnotation").asObject, into)
      }
      case "TupleTypeAnnotation" => {
        into.append("[")
        val types = typeAnno.get("types").asArray
        for (ii <- 0 until types.size) {
          if (ii > 0) into.append(", ")
          appendTypeSig(types.get(ii).asObject, into)
        }
        into.append("]")
      }
      case "GenericTypeAnnotation" => {
        appendTypeSig(typeAnno.get("id").asObject, into)
        appendTypeParamsSig(typeAnno.get("typeParameters"), into)
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
          appendParamSig(params.get(ii).asObject, into)
        }
        into.append(") => ")
        appendTypeSig(typeAnno.get("returnType").asObject, into)
      }
      case "FunctionTypeParam" => {
        appendIdentSig(Kind.VALUE, typeAnno.get("name"), into)
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
      case "TypeParameterDeclaration" => {
        appendTypeTypeParamsSig(typeAnno.get("params"), into)
      }
      case "TypeParameter" => {
        appendIdentSig(Kind.TYPE, typeAnno.get("name"), into)
        val bound = typeAnno.get("bound")
        if (bound != null && !bound.isNull) {
          appendTypeSig(bound.asObject, into)
        }
      }
      case _ =>
        println("TODO: type " + typeAnno.get("type"))
        // dump(typeAnno)
        into.append(s"<TODO: ${typeAnno.get("type")}")
    }
    into
  }

  def appendTypeTypeParamsSig(paramsVal :JsonValue, into :SigBuilder) {
    if (paramsVal != null && !paramsVal.isNull()) {
      if (paramsVal.isArray()) {
        val params = paramsVal.asArray
        for (ii <- 0 until params.size) {
          if (ii > 0) into.append(",")
          appendTypeSig(params.get(ii).asObject, into)
        }
      } else if (paramsVal.isObject()) {
        appendTypeSig(paramsVal.asObject, into)
      } else {
        into.append("?")
        println("Unknown type type params value:");
        dump(paramsVal)
      }
    }
  }

  def dump(value :JsonValue) {
    println(value.toString(PrettyPrint.indentWithSpaces(2)))
  }
}
