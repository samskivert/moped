# Moped

![Travis build status](https://travis-ci.org/samskivert/moped.svg?branch=master)

Moped (my own private editor) is a modern programmer's editor, written in Scala 3. It is derived
from the [Scaled] project, which I created and used for many years. Both Moped and Scaled aim to be
hackable, an editor that does exactly what you want because you can change any part of the code any
time. But Moped acknowledges a reality that I optimistically ignored with Scaled, which is that
this editor will only ever have a user base of one, and it optimizes for that.

Thus Moped aims for maximum simplicity. No dynamically loaded modules, maintained in separate git
repositories and integrated via a custom project build system. No custom collections library
designed to be usable by other JVM languages besides Scala. Just a single simple editor project
that does exactly what I need in the easiest to maintain way.

It will be ironic if this project gains traction with a larger user base than Scaled did, but I
won't hold my breath. Scaled has been a great editor for me for the last eight years, Moped can be
a great editor for me for the next eight years.

Another simplification that Moped leverages is the growing popularity of [LSP]. Instead of defining
its own mechanism for language intelligence which was later adapted to work with LSP, Moped is
designed to use LSP from the start.

## Try it

Moped is built with [SBT]. You can build and package it by running:

```
sbt stage
```

Which will create a distribution in `target/universal/stage`.

You can run it from there via the `bin/moped` or `bin/moped.bat` files, optionally passing the names
of files to open. Or you can run it and open files via the usual Emacs `C-x C-f`.

## Using Moped

For the multiple decades before I used Scaled or Moped, I used Emacs. So Moped's "UI" follows
Emacs: most of the basic editing key bindings are the same. It has a status line and `M-x` is used
to access commands.

At any time, you can invoke `M-x describe-mode` (or `C-h m`) to see all of the key bindings and
config vars for the the active major and minor modes. You can cross-reference that with the
[Emacs reference card] to see basic editing commands organized more usefully than alphabetic order.

## Development

As mentioned above, Moped is built with [SBT]. While developing you can run the in-development
editor from within SBT and pass the name of a file to edit. To run Moped on its own source, try
something like:

```
run src/main/scala/impl/Moped.scala
```

at the SBT console.

There's not much documentation on the editor internals, but you can start by looking at the
implementation of a major mode. `src/main/scala/major/EditingMode.scala` shows how most of the
basic editing commands are implemented as manipulations of the internal `Buffer` data structure.

`src/main/scala/major/ScalaMode.scala` shows the Scala language support, which uses `Grammar` (an
implementation of TextMate's grammars) for syntax highlighting, `Commenter` to handle some common
code comment manipulations, `Indenter` which provides a framework for handling code indentation,
and a few other bits and bobs.

Moped integrates with Language Servers, though it doesn't come with built-in integration for most
project types at the moment. Instead you have to create a `.langserver` file in the root directory
of your project and provide a command to execute to start the language server.

For Moped itself, you can install [Metals] and then configure it to use that as the language
server for its own source code. Run:

```
coursier bootstrap org.scalameta:metals_2.13:0.11.10 -o metals -f
```

in the root project directory, and then `sbt bloopInstall`, and create a `.langserver` file that
contains the following:

```
suff: scala
serverCmd: metals/metals
serverArg: -Dmetals.http=true
```

That should cause Moped to start up a language server whenever any file in its project is opened,
and all of the `lang-mode` fns should actually work.

## License

Moped is released under the New BSD License. The most recent version of the code is available at
https://github.com/samskivert/moped

[Scaled]: https://github.com/scaled/scaled
[LSP]: https://langserver.org/
[Emacs reference card]: http://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf
[SBT]: https://www.scala-sbt.org/
