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


_Disclaimer_

This script is not an excuse to forget habitual techniques of page making. It should be used at the end of the process to ensure everything is ok. And it is an excuse not to make edits if they're in numbers only.
