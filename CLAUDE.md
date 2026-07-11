# Moped

Moped (my own private editor) is samskivert's personal programmer's editor, written in Scala 3. It
is a from-scratch rewrite of the [Scaled] project, aimed at a user base of one and optimized for
being easy to hack on rather than for generality. See `README.md` for the fuller pitch, build
instructions, and key bindings (it's Emacs-like: `M-x`, standard Emacs motion/editing keys, etc.).

A few orientation points:

- Built with [SBT]; `sbt compile`/`sbt test`/`sbt run <file>` are the normal dev loop.
- Buffer text lives in `Buffer`/`Line` (`src/main/scala/Buffer.scala`, `Line.scala`), addressed by
  `Loc(row, col)` where `col` is a UTF-16 character offset (matching JVM `String` indexing).
- Major modes live in `src/main/scala/major/`. Most now use tree-sitter for syntax highlighting via
  `SitterCodeMode`/`Sitter` (`src/main/scala/grammar/Sitter.scala`); a few older modes still use the
  TextMate-style `Grammar`/`Scoper` machinery (`src/main/scala/grammar/`).
- LSP integration lives in `src/main/scala/project/` (`LangMode.scala`, `LangClient.scala`, etc.).
- JavaFX rendering (buffer view, cursor, popups) lives in `src/main/scala/impl/` (`BufferArea.scala`,
  `LineViewImpl.scala`, `WindowImpl.scala`).

## Debug/automation socket

Moped starts a small command server on `localhost:32324` (see `src/main/scala/impl/Server.scala`)
that a running instance listens on. It supports commands meant for driving and inspecting the
editor from a script — in particular, for **Claude to use during debugging sessions in this repo**,
without needing OS-level input-injection or screen-recording permissions (screenshots are rendered
in-process via JavaFX's own snapshot mechanism, not the OS screen-capture APIs, so they work in
sandboxed/no-display-permission environments).

### Port separation — read this before testing

A dev/test instance launched via `sbt run <file>` listens on port **32324** (the default). The
*packaged* app (`/Applications/Moped.app`) is configured to use port **32325** instead (via the
`-Dmoped.port=32325` JVM option baked in at packaging time). This means:

- If you (Claude) launch a test instance with `sbt run <file>`, it always talks to 32324, which is
  never the user's own daily-driver app.
- If the user has their packaged app open as their real editor, it's on 32325 and your `sbt run`
  test instances won't collide with it or accidentally forward commands into it.
- **Before running `sbt run` for a debugging session, check `lsof -i :32324` first anyway.** If a
  previous test instance of yours is already listening there and never got cleaned up, a new
  `sbt run` will just forward file-opens to the *stale one* rather than starting fresh (the `open`
  command's fire-and-forget forwarding logic doesn't care which instance is listening). Kill it if
  you don't recognize it as something you meant to keep around, and never kill a process on 32325
  or anything you haven't confirmed you started yourself.

### Commands

Send newline-terminated commands to the socket; most respond with a single line (`ok` or
`error: ...`) except `open`, which is fire-and-forget. Use `./moped-cmd.sh` (repo root) rather than
hand-writing a client each time:

```
./moped-cmd.sh point                          # report point as "row,col"
./moped-cmd.sh mark                           # report mark as "row,col", or "none"
./moped-cmd.sh line 0                         # report the text of line 0 (handy to check the
                                               # actual buffer content, independent of rendering)
./moped-cmd.sh invoke forward-char            # invoke any registered Fn by name
./moped-cmd.sh invoke backward-word
./moped-cmd.sh type "hello world"              # insert literal text at point, as if typed
./moped-cmd.sh click 120 45                   # simulate a mouse press at pixel (x, y)
./moped-cmd.sh click 120 45 2                 # double-click (selects the word); 3 for triple
                                               # (selects the line); both also affect a following drag
./moped-cmd.sh drag 200 45                    # simulate dragging to pixel (x, y)
./moped-cmd.sh release 200 45                 # simulate releasing the mouse button
./moped-cmd.sh screenshot /tmp/shot.png        # PNG of the whole window
./moped-cmd.sh screenshot /tmp/shot.png 0 0 400 200   # PNG cropped to a pixel region
```

`click`/`drag`/`release` coordinates are in the same pixel space `screenshot` captures, so the
usual loop is: screenshot, eyeball (or pixel-measure) where you want to click/drag, then send the
gesture. A click-drag-release sequence sets the mark and point and shows the ephemeral
mouse-selection highlight, same as a real mouse gesture (see `BufferArea.mousePressed`/
`mouseDragged`/`mouseReleased` and `BufferView.dragSelection`).

`MOPED_PORT` (env var) overrides the port the script targets, if you need to reach a non-default
instance.

These all act on "some open window" (`WorkspaceManager.anyWindow`/`WindowImpl.focusedDispatcher`)
— there's no multi-window targeting, since the dev use case is a single test instance with a single
window. Fn names are the kebab-case form of the `@Fn`-annotated method name (e.g.
`deleteBackwardChar` → `delete-backward-char`); grep `@Fn(` in `src/main/scala/major/` if you need
to find one.

### Extending this protocol

This is a debugging tool for you, not user-facing functionality — **if the current command set
makes some debugging task awkward or slow, improve it.** Reasonable ways to extend it:

- New commands in `Server.process` (`src/main/scala/impl/Server.scala`) — e.g. querying buffer
  contents/styles/tags at a location (there's precedent for this kind of introspection: styles are
  queryable in-process via `Buffer.stylesAt`), a way to select a specific window/buffer by name
  instead of always "any window", or a way to wait for/assert on some condition instead of you
  polling in a loop.
- New flags on `screenshot`, e.g. rendering just a specific `Node` (the modeline, a popup) instead
  of the whole window.
- If you add a command, update the doc comment on `Server` *and* the usage comment in
  `moped-cmd.sh` *and* this file, so the next session (you, most likely) doesn't have to
  rediscover it.

The socket only binds to localhost and only exposes what's already reachable through the editor's
own `Fn` mechanism (nothing you couldn't do by hand at the keyboard), so extending it further in
this spirit is low-risk. It's still real code execution against a real (if disposable) editor
process, so keep destructive commands (e.g. anything that writes files, closes windows) reasonably
deliberate.

[Scaled]: https://github.com/scaled/scaled
[SBT]: https://www.scala-sbt.org/
