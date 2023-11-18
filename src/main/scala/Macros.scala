import scala.quoted.*

final case class Person(name: String, age: Int)

object Macros:

  inline def myMacro: Option[Person] =
    ${ myMacroImpl }

  def myMacroImpl(using Quotes): Expr[Option[Person]] =
    import quotes.reflect.*

    val termsNamesAndTypes: List[(Term, String, TypeRepr)] =
      List(
        (
          '{ Option("Navid") }.asTerm,
          "name",
          TypeRepr.of[String]
        ),
        (
          '{ Option(30) }.asTerm,
          "age",
          TypeRepr.of[Int]
        )
      )

    val optionCompanionTermRef = TypeRepr.of[Option].typeSymbol.companionModule.termRef

    def recurse(
      terms: List[(Term, String, TypeRepr)],
      args: List[Term] = Nil
    ): Term =
      terms match
        case (term, name, tpe) :: next =>
          val newMethodSymbol = Symbol.newMethod(
            parent = Symbol.spliceOwner,
            name = s"${name}_anon_fun",
            tpe = MethodType(paramNames = List(name))(
              paramInfosExp = _ => List(tpe),
              resultTypeExp = _ => TypeRepr.of[Option[Person]]
            )
          )

          Select.overloaded(
            qualifier = term,
            name = "flatMap",
            targs = List(TypeRepr.of[Person]),
            args = List(
              Block(
                stats = List(
                  DefDef(
                    symbol = newMethodSymbol,
                    rhsFn = trees =>
                      val ident = trees.flatten.head.asInstanceOf[Ident]
                      Some(recurse(next, args :+ ident))
                  )
                ),
                expr = Closure(Ident(newMethodSymbol.termRef), None)
              )
            )
          )
        case Nil                       =>
          Select.overloaded(
            Ident(optionCompanionTermRef),
            "apply",
            List(TypeRepr.of[Person]),
            List(
              Apply(
                fun = Select.unique(Ident(TypeRepr.of[Person].typeSymbol.companionModule.termRef), "apply"),
                args = args
              )
            )
          )

    val flatMaps = recurse(termsNamesAndTypes)
    println(flatMaps.show(using Printer.TreeCode))
    flatMaps.asExprOf[Option[Person]]
