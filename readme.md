# Description:

Simple Markdown to HTML converter

[![Build Status](https://travis-ci.org/geo2a/markdown-monparsing.svg?branch=master)](https://travis-ci.org/geo2a/markdown_monparsing)

## Features

* Headers
* Paragraphs
* Lists (non-nested)
* Block quotes  
* Latex blocks

## Development

To build this project you need to have haskell [stack](http://docs.haskellstack.org/en/stable/README.html) installed.

To build just run: 

```
stack build
```

To test:

```
stack test
```

To convert example markdown file to html: 

```
stack exec markdown-monparsing -- examples/example.md -o examples/example.html
```