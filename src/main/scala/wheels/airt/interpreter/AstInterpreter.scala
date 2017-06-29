package wheels.airt.interpreter

object AstInterpreter {

  lazy val interpret: Expr => Expr = interpretWith(Context.builtIn)

  lazy val interpretWith: Context => Expr => Expr = context => {
    case list: ExprList =>
      list.head match {
        case ExprIdentifier(identifier) =>
          if (special.contains(identifier)) {
            special(identifier)(context)(list.tail)
          } else {
            val f: ExprFunction = context.getAsExpFunction(identifier)
            val args: Seq[Expr] = list.tail.es.map(interpretWith(context))
            f(args)
          }
        case ih: ExprList =>
          interpretWith(context)(ih) match {
            case ExprNil =>
              list.tail.es.map(interpretWith(context)).last
            case efc: ExprFunction =>
              val args: Seq[Expr] = list.tail.es.map(interpretWith(context))
              efc(args)
            case other => other
          }
        case ih: ExprFunction =>
          val args: Seq[Expr] = list.tail.es.map(interpretWith(context))
          ih(args)
        case _ =>
          throw new RuntimeException(
            "undefined function `%s' in %s".format(list.head, context)
          )
      }

    case expr: ExprIdentifier => context.get(expr.s)

    case expr => expr
  }

  private val special: Map[String, Context => ExprList => Expr] = Map(
    "quote" -> { _ =>
      input =>
        input.head
    },
    "if" -> { context =>
      input =>
        val hv = interpretWith(context)(input.head)
        if (hv == ExprNil)
          interpretWith(context)(input.es(2))
        else
          interpretWith(context)(input.es(1))
    },
    "let" -> { context =>
      input =>
        val bindings = input.head.asInstanceOf[ExprList].es.
          map { case ExprList(Seq(id, v)) => id.asInstanceOf[ExprIdentifier].s -> interpretWith(context)(v) }
        interpretWith(
          context.derive.merge(bindings)
        )(input.tail)
    },
    "lambda" -> { context =>
      input =>
        ExprFunction { args =>
          interpretWith(
            input.head.asInstanceOf[ExprList].es.
              zip(args).
              foldLeft(context.derive) { (c, e) =>
                val (id, v) = e
                c.put(
                  id.asInstanceOf[ExprIdentifier].s,
                  v
                )
              }
          )(input.tail)
        }
    },
    "def" -> { context =>
      input =>
        val f = special("lambda")(context)(input.tail)
        context.put(
          input.head.asInstanceOf[ExprIdentifier].s,
          f
        )
        ExprNil
    }
  )

}
