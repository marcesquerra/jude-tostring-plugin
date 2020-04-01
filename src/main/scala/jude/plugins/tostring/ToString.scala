package jude.plugins.tostring

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins._
import nsc.transform._

class JudeToString(val global: Global) extends Plugin {
  import global._

  val name = "tostring"
  val description =
    "renames the == and != so that the default implementations can't be used"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with TypingTransformers {
    val global: JudeToString.this.global.type = JudeToString.this.global
    val runsAfter = List[String]("parser")
    val phaseName = JudeToString.this.name
    def newPhase(_prev: Phase) = new JudeToStringPhase(_prev)

    val TO_STRING = "toString"
    val TO_STRINGJ = "'toString'"
    val SCALA_TO_STRING = "extern$u0020toString"

    class JudeToStringTransformer(unit: CompilationUnit)
        extends TypingTransformer(unit) {
      override def transform(tree: Tree) = tree match {
        case Apply(Select(lhs, TermName(TO_STRING)), rhs) =>
          Apply(
            Select(transform(lhs), TermName(TO_STRINGJ)),
            rhs.map(transform)
          )
        case Apply(Select(lhs, TermName(SCALA_TO_STRING)), rhs) =>
          Apply(
            Select(transform(lhs), TermName(TO_STRING)),
            rhs.map(transform)
          )
        case Ident(TermName(TO_STRING)) =>
          Ident(TermName(TO_STRINGJ))
        case Ident(TermName(SCALA_TO_STRING)) =>
          Ident(TermName(TO_STRING))
        case DefDef(
            modifiers,
            TermName(TO_STRING),
            tparams,
            params,
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName(TO_STRINGJ),
            tparams,
            params,
            retType,
            transform(rhs)
          )
        case DefDef(
            modifiers,
            TermName(SCALA_TO_STRING),
            tparams,
            params,
            retType,
            rhs
            ) =>
          DefDef(
            modifiers,
            TermName(TO_STRING),
            tparams,
            params,
            retType,
            transform(rhs)
          )
        case _ =>
          // I'll keep this in here. It's good to run experiments
          // println(s"""|
          //   |=================
          //   |$tree
          //   |-----------------
          //   |${showRaw(tree)}
          //   |=================
          //   |""".stripMargin)
          super.transform(tree)
      }
    }

    def newTransformer(unit: CompilationUnit) =
      new JudeToStringTransformer(unit)

    class JudeToStringPhase(prev: Phase) extends StdPhase(prev) {

      type PublicCompilationUnit = CompilationUnit
      override def name = JudeToString.this.name

      override def apply(unit: CompilationUnit): Unit =
        unit.body = new JudeToStringTransformer(unit).transform(unit.body)

    }
  }
}
