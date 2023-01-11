//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import org.junit.Assert._
import org.junit._
import moped._

class NDFTest {

  val input = """name: Markdown GFM
scopeName: text.html.markdown
foldStart: (?x)\
  (<(?i:head|body|table|thead|tbody|tfoot|tr|div|select|fieldset|style|script|ul|ol|form|dl)\b.*?>\
  |<!--(?!.*-->)\
  |\{\s*($|\?>\s*$|//|/\*(.*\*/\s*$|(?!.*?\*/)))\
  )
foldStop: (?x)\
  (</(?i:head|body|table|thead|tbody|tfoot|tr|div|select|fieldset|style|script|ul|ol|form|dl)>\
  |^\s*-->\
  |(^|\s)\}\
  )
repository:
 ampersand:
  single:
   name: meta.other.valid-ampersand.markdown
   pattern: &(?!([a-zA-Z0-9]+|#[0-9]+|#x[0-9a-fA-F]+);)
 block_quote:
  multi:
   name: markup.quote.markdown
   begin: \G[ ]{1,3}(>)[ ]?
   bcaps: 1=punctuation.definition.blockquote.markdown
   end: (?x)^\
    (?= \s*$\
    | [ ]{1,3}(?<marker>[-*_])([ ]{1,2}\k<marker>){2,}[ \t]*+$\
    | [ ]{1,3}>\
    )
   patterns:
    multi:
     begin: (?x)\G\
      (?= [ ]{1,3}>\
      )
     end: ^
     patterns:
      include: #block_quote
    multi:
     begin: (?x)\G\
      (?= ([ ]{4}|\t)\
      | [#]{1,6}\s*+\
      | [ ]{1,3}(?<marker>[-*_])([ ]{1,2}\k<marker>){2,}[ \t]*+$\
      )
     end: ^
     patterns:
      include: #block_raw
      include: #heading
      include: #separator
    multi:
     begin: (?x)\G\
      (?! $\
      | [ ]{1,3}>\
      | ([ ]{4}|\t)\
      | [#]{1,6}\s*+\
      | [ ]{1,3}(?<marker>[-*_])([ ]{1,2}\k<marker>){2,}[ \t]*+$\
      )
     end: $|(?<=\n)
     patterns:
      include: #inline
      include: text.html.basic
 block_raw:
  single:
   name: markup.raw.block.markdown
   pattern: \G([ ]{4}|\t).*$\n?
 bold:
  multi:
   name: markup.bold.markdown
   begin: (?x)\
      (?<!\w)(\*\*|__)(?=\S)        # Open\
      (?=\
       (\
           <[^>]*+>       # HTML tags\
         | (?<raw>`+)([^`]|(?!(?<!`)\k<raw>(?!`))`)*+\k<raw>\
                 # Raw\
         | \\[\\`*_{}\[\]()#.!+\->]?+   # Escapes\
         | \[\
        (\
                (?<square>     # Named group\
           [^\[\]\\]    # Match most chars\
                  | \\.      # Escaped chars\
                  | \[ \k<square>*+ \]  # Nested brackets\
                )*+\
         \]\
         (\
          (       # Reference Link\
           [ ]?     # Optional space\
           \[[^\]]*+\]    # Ref name\
          )\
           | (       # Inline Link\
           \(      # Opening paren\
            [ \t]*+    # Optional whtiespace\
            <?(.*?)>?   # URL\
            [ \t]*+    # Optional whtiespace\
            (     # Optional Title\
             (?<title>['"])\
             (.*?)\
             \k<title>\
            )?\
           \)\
          )\
         )\
        )\
         | (?!(?<=\S)\1).      # Everything besides\
                 # style closer\
       )++\
       (?<=\S)\1        # Close\
      )\
     
   bcaps: 1=punctuation.definition.bold.markdown
   end: (?<=\S)(\1)
   ecaps: 1=punctuation.definition.bold.markdown
   patterns:
    multi:
     begin: (?=<[^>]*?>)
     end: (?<=>)
     patterns:
      include: #tag-kbd
      include: text.html.basic
    include: #escape
    include: #ampersand
    include: #bracket
    include: #raw
    include: #italic
    include: #strikethrough
    include: #image-inline
    include: #link-inline
    include: #link-inet
    include: #link-email
    include: #image-ref
    include: #link-ref-literal
    include: #link-ref
 bracket:
  single:
   name: meta.other.valid-bracket.markdown
   pattern: <(?![a-z/?\$!])
 escape:
  single:
   name: constant.character.escape.markdown
   pattern: \\[-`*_#+.!(){}\[\]\\>]
 fenced-c:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(c)\s*$
   end: (\1)\n
   patterns:
    include: source.c
 fenced-c++:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(c\+\+|cpp)\s*$
   end: (\1)\n
   patterns:
    include: source.c++
 fenced-code-blocks:
  include: #fenced-html
  include: #fenced-xml
  include: #fenced-diff
  include: #fenced-perl
  include: #fenced-php
  include: #fenced-css
  include: #fenced-less
  include: #fenced-java
  include: #fenced-c
  include: #fenced-c++
  include: #fenced-yaml
  include: #fenced-sql
  include: #fenced-shell
  include: #fenced-sass
  include: #fenced-scala
  include: #fenced-obj-c
  include: #fenced-coffee
  include: #fenced-js
  include: #fenced-ruby
  include: #fenced-python
  include: #fenced-undefined
 fenced-coffee:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(coffee)\s*$
   end: (\1)\n
   patterns:
    include: source.coffee
 fenced-css:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(css)\s*$
   end: (\1)\n
   patterns:
    include: source.css
 fenced-diff:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(diff)\s*$
   end: (\1)\n
   patterns:
    include: source.diff
 fenced-html:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(html|html5)\s*$
   end: (\1)\n
   patterns:
    include: text.html.basic
 fenced-java:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(java)\s*$
   end: (\1)\n
   patterns:
    include: source.java
 fenced-js:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(js|json|javascript)\s*$
   end: (\1)\n
   patterns:
    include: source.js
 fenced-less:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(less)\s*$
   end: (\1)\n
   patterns:
    include: source.css.less
 fenced-obj-c:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(objective-c)\s*$
   end: (\1)\n
   patterns:
    include: source.objc
 fenced-perl:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(perl)\s*$
   end: (\1)\n
   patterns:
    include: source.perl
 fenced-php:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(php)\s*$
   end: (\1)\n
   patterns:
    include: source.php
 fenced-python:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(py|python)\s*$
   end: (\1)\n
   patterns:
    include: source.python
 fenced-ruby:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(ruby)\s*$
   end: (\1)\n
   patterns:
    include: source.ruby
 fenced-sass:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(sass|scss)\s*$
   end: (\1)\n
   patterns:
    include: source.sass
 fenced-scala:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(scala)\s*$
   end: (\1)\n
   patterns:
    include: source.scala
 fenced-shell:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(sh|shell)\s*$
   end: (\1)\n
   patterns:
    include: source.shell
 fenced-sql:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(sql)\s*$
   end: (\1)\n
   patterns:
    include: source.sql
 fenced-undefined:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*(`{3,}|~{3,}))(.*?)\s*$
   end: (\1)\n
 fenced-xml:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(xml)\s*$
   end: (\1)\n
   patterns:
    include: text.xml
 fenced-yaml:
  multi:
   name: markup.raw.block.markdown markup.raw.block.fenced.markdown
   begin: ^(\s*[`~]{3,})(yaml)\s*$
   end: (\1)\n
   patterns:
    include: source.yaml
 heading:
  multi:
   name: markup.heading.markdown
   contentName: entity.name.section.markdown
   begin: \G(#{1,6})(?!#)\s*(?=\S)
   bcaps: 1=punctuation.definition.heading.markdown
   end: \s*(#*)$\n?
   ecaps: 1=punctuation.definition.heading.markdown
   patterns:
    include: #inline
 image-inline:
  single:
   name: meta.image.inline.markdown
   pattern: (?x:\
    \!       # Images start with !\
    (\[)((?<square>[^\[\]\\]|\\.|\[\k<square>*+\])*+)(\])\
           # Match the link text.\
    ([ ])?      # Space not allowed\
    (\()      # Opening paren for url\
     (<?)(\S+?)(>?)   # The url\
     [ \t]*     # Optional whitespace\
     (?:\
        ((\().+?(\)))  # Match title in parens…\
      | ((").+?("))  # or in quotes.\
     )?      # Title is optional\
     \s*      # Optional whitespace\
    (\))\
    )
   caps: 1=punctuation.definition.string.begin.markdown 10=string.other.link.description.title.markdown 11=punctuation.definition.string.markdown 12=punctuation.definition.string.markdown 13=string.other.link.description.title.markdown 14=punctuation.definition.string.markdown 15=punctuation.definition.string.markdown 16=punctuation.definition.metadata.markdown 2=string.other.link.description.markdown 4=punctuation.definition.string.end.markdown 5=invalid.illegal.whitespace.markdown 6=punctuation.definition.metadata.markdown 7=punctuation.definition.link.markdown 8=markup.underline.link.image.markdown 9=punctuation.definition.link.markdown
 image-ref:
  single:
   name: meta.image.reference.markdown
   pattern: \!(\[)((?<square>[^\[\]\\]|\\.|\[\k<square>*+\])*+)(\])[ ]?(\[)(.*?)(\])
   caps: 1=punctuation.definition.string.begin.markdown 2=string.other.link.description.markdown 4=punctuation.definition.string.begin.markdown 5=punctuation.definition.constant.markdown 6=constant.other.reference.link.markdown 7=punctuation.definition.constant.markdown
 inline:
  include: #escape
  include: #ampersand
  include: #bracket
  include: #raw
  include: #bold
  include: #italic
  include: #strikethrough
  include: #line-break
  include: #image-inline
  include: #link-inline
  include: #link-inet
  include: #link-email
  include: #image-ref
  include: #link-ref-literal
  include: #link-ref
  include: #plain-uri
  include: #tag-kbd
 italic:
  multi:
   name: markup.italic.markdown
   begin: (?x)\
      (?<!\w)(\*|_)(?=\S)        # Open\
      (?=\
       (\
           <[^>]*+>       # HTML tags\
         | (?<raw>`+)([^`]|(?!(?<!`)\k<raw>(?!`))`)*+\k<raw>\
                 # Raw\
         | \\[\\`*_{}\[\]()#.!+\->]?+   # Escapes\
         | \[\
        (\
                (?<square>     # Named group\
           [^\[\]\\]    # Match most chars\
                  | \\.      # Escaped chars\
                  | \[ \k<square>*+ \]  # Nested brackets\
                )*+\
         \]\
         (\
          (       # Reference Link\
           [ ]?     # Optional space\
           \[[^\]]*+\]    # Ref name\
          )\
           | (       # Inline Link\
           \(      # Opening paren\
            [ \t]*+    # Optional whtiespace\
            <?(.*?)>?   # URL\
            [ \t]*+    # Optional whtiespace\
            (     # Optional Title\
             (?<title>['"])\
             (.*?)\
             \k<title>\
            )?\
           \)\
          )\
         )\
        )\
         | \1\1        # Must be bold closer\
         | (?!(?<=\S)\1).      # Everything besides\
                 # style closer\
       )++\
       (?<=\S)\1        # Close\
      )\
     
   bcaps: 1=punctuation.definition.italic.markdown
   end: (?<=\S)(\1)((?!\1)|(?=\1\1))
   ecaps: 1=punctuation.definition.italic.markdown
   patterns:
    multi:
     begin: (?=<[^>]*?>)
     end: (?<=>)
     patterns:
      include: #tag-kbd
      include: text.html.basic
    include: #escape
    include: #ampersand
    include: #bracket
    include: #raw
    include: #bold
    include: #strikethrough
    include: #image-inline
    include: #link-inline
    include: #link-inet
    include: #link-email
    include: #image-ref
    include: #link-ref-literal
    include: #link-ref
 line-break:
  single:
   name: meta.dummy.line-break
   pattern:  {2,}$
 link-email:
  single:
   name: meta.link.email.lt-gt.markdown
   pattern: (<)((?:mailto:)?[-.\w]+@[-a-z0-9]+(\.[-a-z0-9]+)*\.[a-z]+)(>)
   caps: 1=punctuation.definition.link.markdown 2=markup.underline.link.markdown 4=punctuation.definition.link.markdown
 link-inet:
  single:
   name: meta.link.inet.markdown
   pattern: (<)((?:https?|ftp)://.*?)(>)
   caps: 1=punctuation.definition.link.markdown 2=markup.underline.link.markdown 3=punctuation.definition.link.markdown
 link-inline:
  single:
   name: meta.link.inline.markdown
   pattern: (?x:\
    (\[)((?<square>[^\[\]\\]|\\.|\[\k<square>*+\])*+)(\])\
           # Match the link text.\
    ([ ])?      # Space not allowed\
    (\()      # Opening paren for url\
     (<?)(.*?)(>?)   # The url\
     [ \t]*     # Optional whitespace\
     (?:\
        ((\().+?(\)))  # Match title in parens…\
      | ((").+?("))  # or in quotes.\
     )?      # Title is optional\
     \s*      # Optional whitespace\
    (\))\
    )
   caps: 1=punctuation.definition.string.begin.markdown 10=string.other.link.description.title.markdown 11=punctuation.definition.string.begin.markdown 12=punctuation.definition.string.end.markdown 13=string.other.link.description.title.markdown 14=punctuation.definition.string.begin.markdown 15=punctuation.definition.string.end.markdown 16=punctuation.definition.metadata.markdown 2=string.other.link.title.markdown 4=punctuation.definition.string.end.markdown 5=invalid.illegal.whitespace.markdown 6=punctuation.definition.metadata.markdown 7=punctuation.definition.link.markdown 8=markup.underline.link.markdown 9=punctuation.definition.link.markdown
 link-ref:
  single:
   name: meta.link.reference.markdown
   pattern: (\[)((?<square>[^\[\]\\]|\\.|\[\k<square>*+\])*+)(\])[ ]?(\[)([^\]]*+)(\])
   caps: 1=punctuation.definition.string.begin.markdown 2=string.other.link.title.markdown 4=punctuation.definition.string.end.markdown 5=punctuation.definition.constant.begin.markdown 6=constant.other.reference.link.markdown 7=punctuation.definition.constant.end.markdown
 link-ref-literal:
  single:
   name: meta.link.reference.literal.markdown
   pattern: (\[)((?<square>[^\[\]\\]|\\.|\[\k<square>*+\])*+)(\])[ ]?(\[)(\])
   caps: 1=punctuation.definition.string.begin.markdown 2=string.other.link.title.markdown 4=punctuation.definition.string.end.markdown 5=punctuation.definition.constant.begin.markdown 6=punctuation.definition.constant.end.markdown
 list-paragraph:
  multi:
   name: meta.paragraph.list.markdown
   begin: \G\s+(?=\S)
   end: ^\s*$
   patterns:
    include: #inline
    include: text.html.basic
    include: #fenced-code-blocks
 plain-uri:
  single:
   name: meta.link.inet.markdown markup.underline.link.naked.markdown
   pattern: (?xi)\
 \b\
  (                           # Capture 1: entire matched URL\
  (?:\
    [a-z][\w-]+:                # URL protocol and colon\
    (?:\
      /{1,3}                        # 1-3 slashes\
      |                             #   or\
      [a-z0-9%]                     # Single letter or digit or '%'\
                                    # (Trying not to match e.g. "URI::Escape")\
    )\
    |                           #   or\
    www\d{0,3}[.]               # "www.", "www1.", "www2." … "www999."\
    |                           #   or\
    [a-z0-9.\-]+[.][a-z]{2,4}/  # looks like domain name followed by a slash\
  )\
  (?:                           # One or more:\
    [^\s()<>]+                      # Run of non-space, non-()<>\
    |                               #   or\
    \(([^\s()<>]+|(\([^\s()<>]+\)))*\)  # balanced parens, up to 2 levels\
  )+\
  (?:                           # End with:\
    \(([^\s()<>]+|(\([^\s()<>]+\)))*\)  # balanced parens, up to 2 levels\
    |                                   #   or\
    [^\s`!()\[\]{};:'".,<>?«»“”‘’]        # not a space or one of these punct chars\
  )\
)
 raw:
  single:
   name: markup.raw.inline.markdown
   pattern: (`+)((?:[^`]|(?!(?<!`)\1(?!`))`)*+)(\1)
   caps: 1=punctuation.definition.raw.markdown 2=markup.raw.inline.content.markdown 3=punctuation.definition.raw.markdown
 separator:
  single:
   name: meta.separator.markdown
   pattern: \G[ ]{1,3}([-*_])([ ]{1,2}\1){2,}[ \t]*$\n?
 strikethrough:
  multi:
   name: markup.strikethrough.markdown
   begin: (?x)\
      (?<!\w)(~~)(?=[^\s~])        # Open\
      (?=\
       (\
           <[^>]*+>       # HTML tags\
         | (?<raw>`+)([^`]|(?!(?<!`)\k<raw>(?!`))`)*+\k<raw>\
                 # Raw\
         | \\[\\`*_{}\[\]()#.!+\->]?+   # Escapes\
         | \[\
        (\
                (?<square>     # Named group\
           [^\[\]\\]    # Match most chars\
                  | \\.      # Escaped chars\
                  | \[ \k<square>*+ \]  # Nested brackets\
                )*+\
         \]\
         (\
          (       # Reference Link\
           [ ]?     # Optional space\
           \[[^\]]*+\]    # Ref name\
          )\
           | (       # Inline Link\
           \(      # Opening paren\
            [ \t]*+    # Optional whtiespace\
            <?(.*?)>?   # URL\
            [ \t]*+    # Optional whtiespace\
            (     # Optional Title\
             (?<title>['"])\
             (.*?)\
             \k<title>\
            )?\
           \)\
          )\
         )\
        )\
         | (?!(?<=\S)\1).      # Everything besides\
                 # style closer\
       )++\
      )\
     
   bcaps: 1=punctuation.definition.strikethrough.markdown
   end: (?<=\S)(\1)
   ecaps: 1=punctuation.definition.strikethrough.markdown
   patterns:
    multi:
     begin: (?=<[^>]*?>)
     end: (?<=>)
     patterns:
      include: #tag-kbd
      include: text.html.basic
    include: #escape
    include: #ampersand
    include: #bracket
    include: #raw
    include: #bold
    include: #italic
    include: #image-inline
    include: #link-inline
    include: #link-inet
    include: #link-email
    include: #image-ref
    include: #link-ref-literal
    include: #link-ref
 tag-kbd:
  single:
   name: markup.kbd.markdown
   pattern: ((<)(kbd)(>))([^<]+)((</)(kbd)(>))
   caps: 1=meta.tag.other.html 2=punctuation.definition.tag.begin.html 3=entity.name.tag.other.html 4=punctuation.definition.tag.end.html 5=markup.kbd.content.markdown 6=meta.tag.other.html 7=punctuation.definition.tag.begin.html 8=entity.name.tag.other.html 9=punctuation.definition.tag.end.html
patterns:
 multi:
  name: meta.block-level.markdown
  begin: (?x)^\
    (?= [ ]{,3}>\
    | ([ ]{4}|\t)(?!$)\
    | [#]{1,6}\s*+\
    | [ ]{,3}(?<marker>[-*_])([ ]{,2}\k<marker>){2,}[ \t]*+$\
    )
  end: (?x)^\
    (?! [ ]{,3}>\
    | ([ ]{4}|\t)\
    | [#]{1,6}\s*+\
    | [ ]{,3}(?<marker>[-*_])([ ]{,2}\k<marker>){2,}[ \t]*+$\
    )
  patterns:
   include: #block_quote
   include: #block_raw
   include: #heading
   include: #separator
 multi:
  name: markup.list.unnumbered.markdown
  begin: ^[ ]{0,3}([*+-])(?=\s)
  bcaps: 1=punctuation.definition.list_item.markdown
  end: ^(?=\S)
  ecaps: 1=punctuation.definition.list_item.markdown
  patterns:
   include: #fenced-code-blocks
   include: #list-paragraph
 multi:
  name: markup.list.numbered.markdown
  begin: ^[ ]{0,3}([0-9]+)(\.)(?=\s)
  bcaps: 1=punctuation.definition.list_item.number.markdown 2=punctuation.definition.list_item.markdown
  end: ^(?=\S)
  ecaps: 1=punctuation.definition.list_item.number.markdown 2=punctuation.definition.list_item.markdown
  patterns:
   include: #fenced-code-blocks
   include: #list-paragraph
 include: #fenced-code-blocks
 multi:
  name: meta.disable-markdown
  begin: ^(?=<(p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math|ins|del)\b[^>]*>)(?!.*?</\1>)
  end: (?<=^</\1>$\n)
  patterns:
   include: #tag-kbd
   include: text.html.basic
 multi:
  name: meta.disable-markdown
  begin: ^(?=<(p|div|h[1-6]|blockquote|pre|table|dl|ol|ul|script|noscript|form|fieldset|iframe|math|ins|del)\b[^>]*>)
  end: $\n?
  patterns:
   include: #tag-kbd
   include: text.html.basic
 single:
  name: meta.link.reference.def.markdown
  pattern: (?x:\
    \s*      # Leading whitespace\
    (\[)(.+?)(\])(:)  # Reference name\
    [ \t]*     # Optional whitespace\
    (<?)(\S+?)(>?)   # The url\
    [ \t]*     # Optional whitespace\
    (?:\
       ((\().+?(\)))  # Match title in quotes…\
     | ((").+?("))  # or in parens.\
    )?      # Title is optional\
    [ \t]*      # Optional whitespace\
    $\
   )
  caps: 1=punctuation.definition.constant.markdown 10=punctuation.definition.string.end.markdown 11=string.other.link.description.title.markdown 12=punctuation.definition.string.begin.markdown 13=punctuation.definition.string.end.markdown 2=constant.other.reference.link.markdown 3=punctuation.definition.constant.markdown 4=punctuation.separator.key-value.markdown 5=punctuation.definition.link.markdown 6=markup.underline.link.markdown 7=punctuation.definition.link.markdown 8=string.other.link.description.title.markdown 9=punctuation.definition.string.begin.markdown
 multi:
  name: meta.paragraph.markdown
  begin: ^(?=\S)(?![=-]{3,}(?=$))
  end: ^(?:\s*$|(?=[ ]{,3}>)|(?=```|~~~))|(?=[ \t]*\n)(?<=^===|^====|=====|^---|^----|-----)[ \t]*\n|(?=^#)
  patterns:
   include: #inline
   include: text.html.basic
   single:
    name: markup.heading.1.markdown
    pattern: ^(={3,})(?=[ \t]*$)
    caps: 1=punctuation.definition.heading.markdown
   single:
    name: markup.heading.2.markdown
    pattern: ^(-{3,})(?=[ \t]*$)
    caps: 1=punctuation.definition.heading.markdown
"""

  @Test def testRead () :Unit = {
    val vals = NDF.read(List.from(input.split("\n")))
    assertEquals(List("name", "scopeName", "foldStart", "foldStop", "repository", "patterns"),
                 vals.map(_.key))

    val nameVal = vals.find(_.key == "name").map({
      case NDF.StrEnt(key,value) => value
      case other => other.toString
    }).get
    assertEquals("Markdown GFM", nameVal)

    val repoValKeys = vals.find(_.key == "repository").map({
      case NDF.DicEnt(key,values) => values.map(_.key)
      case other => other.toString
    }).get
    assertEquals(List("ampersand", "block_quote", "block_raw", "bold", "bracket", "escape",
                      "fenced-c", "fenced-c++", "fenced-code-blocks", "fenced-coffee",
                      "fenced-css", "fenced-diff", "fenced-html", "fenced-java", "fenced-js",
                      "fenced-less", "fenced-obj-c", "fenced-perl", "fenced-php", "fenced-python",
                      "fenced-ruby", "fenced-sass", "fenced-scala", "fenced-shell", "fenced-sql",
                      "fenced-undefined", "fenced-xml", "fenced-yaml", "heading", "image-inline",
                      "image-ref", "inline", "italic", "line-break", "link-email", "link-inet",
                      "link-inline", "link-ref", "link-ref-literal", "list-paragraph", "plain-uri",
                      "raw", "separator", "strikethrough", "tag-kbd"), repoValKeys)
  }

  @Test def testParseGrammar () :Unit = {
    val grammar = NDFGrammar.toGrammar(NDF.read(List.from(input.split("\n"))))
    grammar.print(System.out)
  }
}
