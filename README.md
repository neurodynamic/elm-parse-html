An HTML parser in Elm using the [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1) library. This library takes some ideas from [jinjor/elm-html-parser](http://package.elm-lang.org/packages/jinjor/elm-html-parser), and implements something similar, but with a focus on getting more specific information when parsing fails, and without the querying capabilities that library provides.

TODO
- Slashless self-closing tags like \<br\>, \<hr\>, etc.
- Escape characters?