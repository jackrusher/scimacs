# scimacs

The [Small Clojure Interpreter](https://github.com/babashka/sci) (sci)
compiled to a native library and integrated with emacs as a loadable
module.

**This code is very not ready for general consumption!** If you aren't
excited about figuring out how to build it on your platform, it's
probably not ready for you yet.

## Build

Requires Clojure, babashka, GraalVM version 22.3.1, and Rust.

If you are using a recent version of MacOS, have all the deps
installed, and are a relatively lucky person, it might be as easy as
doing this from the top-level of this project's directory structure
(making sure that the Java on your path is the GraalVM version
mentioned above):

``` shell
$ bb all
```

## Testing

There's a small `elisp` file that can be used to make sure things
work. It's invoked like this (`chmod` if you have to):

``` shell
$ ./test.el
```

Note that the shebang line in this file assumes a particular version
of emacs at the path where homebrew installs it. It's entirely likely
that you have a different version installed in a different place, so
you may prefer to use:

``` shell
$ emacs --script test.el
```

## Using

See `test.el` for how to load this module in emacs. It currently has a
_single function_ API. Additional Work will be required to make things
more pleasant to use from within emacs, including the addition of some
callback functions on the `sci` side to operate on buffers and such.
