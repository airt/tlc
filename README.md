
# hellolisp.scala
a mini Lisp interpreter in Scala

### supports

`let` `lambda` `defun` `quote` `if`

and some [library functions](https://github.com/airtial/hellolisp.scala/blob/master/src/main/scala/com/airtial/hellolisp/Library.scala)

### usage

    $ scala out/hellolisp.scala.jar lispcode.l

or

```scala
import com.airtial.hellolisp._

object Main {

  def main(args: Array[String]) = {
    val code = """((lambda (func x) (func x (+ x 1)))
                     (lambda (x y) (+ x y))
                     512)"""
    println(Hellolisp.eval(code))
  }

}
```
