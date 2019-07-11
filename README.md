### LaTeX-numbers
Edit numbers formatting in LaTeX files

#### Rules:

There is a dictionary of commands to be excluded from consideration.

All numbers outside of commands must be written in math mode.

Decimals are written as d+{,}d+. All numbers are splitted by chunks of length three with `\,` space.

#### Usage:

Works with all .tex files in a directory.

`LaTeX-numbers <path to dictionary> <path to directory to work in>`.
