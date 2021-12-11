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
its own mechanism for language intellegince which was later adapted to work with LSP, Moped is
designed to use LSP from the start.

## Try it

TBD

## Using Moped

For the multiple decades before I used Scaled or Moped, I used Emacs. So Moped's "UI" follows
Emacs: most of the basic editing key bindings are the same. It has a status line and M-x is used to
access commands.

At any time, you can invoke `M-x describe-mode` (or `C-h m`) to see all of the key bindings and
config vars for the the active major and minor modes. You can cross-reference that with the
[Emacs reference card] to see basic editing commands organized more usefully than alphabetic order.

## Development

TBD

## License

Moped is released under the New BSD License. The most recent version of the code is available at
https://github.com/samskivert/moped

[Scaled]: https://github.com/scaled/scaled
[LSP]: https://langserver.org/
[Emacs reference card]: http://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf
