# the-little-interpreter

a mini lisp interpreter in scala

## support

`let` `lambda` `defun` `quote` `if`

## usage

```bash
mvn package
cd target
scala the-little-interpreter-0.0.1.jar coo.l
```

or

```scala
import io.airt.interpreter._

object Main {

  def main(args: Array[String]) {
    val code = """
      ((lambda (func x) (func x (+ x 1)))
         (lambda (x y) (+ x y))
         512)
    """
    println(TLI.eval(code))
  }

}
```

## references

Thank you to [Mary Rose Cook](http://maryrosecook.com/) [(Little Lisp Interpreter)](https://www.recurse.com/blog/21-little-lisp-interpreter)

## license

MIT
