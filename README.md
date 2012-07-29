Stackoverflow for CLI
======
Lightweight StackOverflow search / browsing CLI written in Haskell.

Usage
=====
```
Usage: so [-t tag1;tag2] [-adtvVh] [--version] [--help] ["<search query>"].
Commands: j,k       get next (j) or previous (k) page, question or answer
          <Number>  jump to question with given order no.
          a         jump to answers of the displayed question
          q         quit app
Don't forget to use quote marks on queries.
Arguments:
           --date       Sort results by date.
           --score      Sort results by score.
  -r       --relevance  Sort results by relevance. Default.
  -a       --asc        Ascending order.
  -d       --desc       Descending order. Default
  -t tags  --tags=tags  Comma-separated tags to boil down your search results with. 
                         E.g. "-t=haskell;parsec"
  -v, -V   --version    Print version and exit...
  -h, -?   --help       Prints this help message.
```
