# the-little-interpreter

[![Build Status][build-badge]][build-status]

a mini lisp interpreter

## support

### ops

`quote` `if` `let` `lambda` `def`

## usage

```bash
mvn package
cd target
scala the-little-interpreter-*.jar coo.l
```

or

```scala
import wheels.airt.interpreter._

object App {

  def main(args: Array[String]) {
    val code = """
      ((lambda (func x) (func x (+ x 1)))
        (lambda (x y) (+ x y))
        512)
    """
    println(Main.eval(code))
  }

}
```

## references

Thank you to [Mary Rose Cook](http://maryrosecook.com/) [(Little Lisp Interpreter)](https://www.recurse.com/blog/21-little-lisp-interpreter)

## license

MIT

[build-badge]: https://img.shields.io/travis/airt/the-little-interpreter.svg
[build-status]: https://travis-ci.org/airt/the-little-interpreter
