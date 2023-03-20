# scimacs

The [Small Clojure Interpreter](https://github.com/babashka/sci) (sci)
compiled to a native library and integrated with emacs as a loadable
module.

**This code is very not ready for general consumption!** If you aren't
excited about figuring out how to build it on your platform, it's
probably not ready for you yet.

## Build

Requires Clojure, GraalVM version 22.3.1, and the Rust toolchain.

If you are using a recent version of MacOS, have all the deps
installed, and are a relatively lucky person, it might be as easy as
doing this from the top-level of this project's directory structure
(making sure that your default Java is the GraalVM one):

``` shell
$ cd clj
$ clj -T:build libscimacs
$ native-image --shared -cp target/classes/ -H:Name=LibScimacs --enable-preview -H:+ReportExceptionStackTraces -J-Dclojure.spec.skip-macros=true -J-Dclojure.compiler.direct-linking=true --initialize-at-build-time --verbose --no-fallback --no-server -J-Xmx3g
$ cd ..
$ cargo build
$ ln -s target/debug/libscimacs.dylib ./scimacs.dylib
```

The last step will give you a loadable module suitable for use with
emacs in the top-level of the project.

## Testing

There's a small `elisp` file that can be used to make sure things
work. It's invoked like this (`chmod` if you have to):

``` shell
$ ./test.el
```

Note that the shebang line in this file assumes particular version of
emacs at the place where homebrew installs it. It's entirely likely
that you have a different version installed in a different place, say
you may prefer to use:

``` shell
$ emacs --script test.el
```

## Using

See `test.el` for the current _single function_ API of this
library. Additional Work will be required to make things more pleasant
to use from within emacs, including the addition of some callback
functions to operate on buffers and such.
