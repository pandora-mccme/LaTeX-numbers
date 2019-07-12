### LaTeX-numbers
Edit numbers formatting in LaTeX files

#### Rules:

There is a dictionary of commands to be excluded from consideration.

All numbers outside of commands must be written in math mode.

Decimals are always written as `d+{,}d+`. All numbers are splitted by chunks of length three with `\,` space.

Time is interpreted as single number.

#### Usage:

Works with all .tex files in a directory.

`LaTeX-numbers -d <path to dictionary> <path to directory to work in>`.

For complete reference call `LaTeX-numbers -h`. **Important notice:** By default tool works in place of files. Enable writing to copies with `.test` extension by `-D` option.

Output of embedded help message:
```
Fix number formatting through directory.

Usage: LaTeX-numbers (-d|--dict DICT) PATH [-D|--debug]

Available options:
  -h,--help                Show this help text
  -d,--dict DICT           File with list of expressions not to change numbers
                           in.
  PATH                     Directory with LaTeX to fix.
  -D,--debug               Write changes to another file (debug mode)
```

Dictionary contains regular expressions (usually simple) for LaTeX commands with arguments. It support annotation line comments in Haskell syntax.

**Disclaimer**

This script is not an excuse to forget habitual techniques of page making. It should be used at the end of the process to ensure everything is ok. And it is an excuse not to make edits if they're in numbers only.
