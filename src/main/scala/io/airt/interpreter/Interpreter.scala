package io.airt.interpreter

object Interpreter {

  def interpret(input: Exp): Exp = interpret(input, Context.library)

  def interpret(input: Exp, context: Context): Exp = {
    input match {
      case e: ExpList => interpretList(e, context)
      case e: ExpSymbol => context.get(e.value)
      case _ => input
    }
  }

  def interpretList(input: ExpList, context: Context): Exp = {
    input.head match {
      case ih: ExpSymbol =>
        if (Special.contains(ih.value)) {
          Special(ih.value).apply(input, context)
        } else {
          val func: ExpFunction = context.getAsExpFunction(ih.value)
          val args: List[Exp] = input.tail.values.map(interpret(_, context))
          func.call(args)
        }
      case ih: ExpList =>
        interpretList(ih, context) match {
          case ExpNil =>
            input.tail.values.map(interpret(_, context)).last
          case efc: ExpFunction =>
            val args: List[Exp] = input.tail.values.map(interpret(_, context))
            efc.call(args)
        }
      case ih: ExpFunction =>
        val args: List[Exp] = input.tail.values.map(interpret(_, context))
        ih.call(args)
      case _ =>
        throw new scala.RuntimeException(
          String.format("undefined function `%s' in %s", input.head, context))
    }

  }

  val Special: Map[String, (ExpList, Context) => Exp] = Map(
    ("let", (input, context) => {
      interpret(input(2),
        input(1).
          asInstanceOf[ExpList].values.
          foldLeft(context.birth) { (c, e) =>
            c.put(e.asInstanceOf[ExpList].values
              .head.asInstanceOf[ExpSymbol].value,
              interpret(e.asInstanceOf[ExpList].values(1), context))
          }
      )
    }),
    ("lambda", (input, context) => {
      Exp((args: List[Exp]) => {
        interpret(input(2),
          input(1)
            .asInstanceOf[ExpList].values
            .zip(args)
            .foldLeft(context.birth)((c, e) => {
              c.put(e._1.asInstanceOf[ExpSymbol].value,
                e._2)
            })
        )
      })
    }),
    ("defun", (input, context) => {
      context.put(input(1).asInstanceOf[ExpSymbol].value,
        Special("lambda")
          .apply(input.tail, context))
      ExpNil
    }),
    ("quote", (input, _) => {
      input.values(1)
    }),
    ("if", (input, context) => {
      if (interpret(input.values(1), context).toBoolean)
        interpret(input.values(2), context)
      else
        interpret(input.values(3), context)
    })
  )

}
