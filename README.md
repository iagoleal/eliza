# Eliza Chatbot

| **Build Status** |
|:----------------:|
| [![Build Status][build-img]][build-url] |

This is a Haskell implementation of Weizembaum's classic chatbot ELIZA,
based upon the description found on his 1966 ACM paper [[1]](#1).

# Installation

The easiest and recommended way is to install using [stack](https://docs.haskellstack.org/en/stable/README/).
To directly install the executable to your path (Stack by default uses `~/.local/bin/`), run

```
stack setup && stack install
```

If you just want to run the program to try it out without any installation or copy,
you can use the one-liner

```
stack setup && stack run
```

## Writing a script

The bot bases its answers on decomposition / reassembly rules
defined in a JSON file who acts as a "script"
(in the screenwriter sense, not the computer program sense).
The program comes with the doctor script from Weizenbaum's paper already bundled
and loaded by default but you can write any set of rules you want.
See the folder `scripts/` for examples of ready to use scripts.

Note: on what follows, `<word>` stands for a sequence of alphanumeric characters
and `<number>` for a sequence of digits representing a positive number.

### Decomposition rules

A decomposition rule consists of a string of space separated expressions.
Each expression matches a certain kind of word or phrase.
See the description below.

| Expression            | Description                          |
| :-------------------- | :----------------------------------- |
| `<word>`              | match exactly `<word>`               |
| `*`                   | match zero or more words             |
| `#<number>`           | match exactly `<number>` words       |
| `[<word> ... <word>]` | match any of word in the list        |
| `@<word>`             | match a keyword from group `<word>`  |

So, for example, the rule `"* I [love hate] #4 @family *"`
can only match a phrase consisting of anything followed by the word `you`,
followed by one of `love` or `hate`, followed by exactly two words,
followed by any word of the group `family` followed by anything.
This means that the phrase "I really love to ski with my children on cold weekends"
matches into the list `["", "love", "to ski with my", "children", "on cold weekends"]`.
If a phrase does not match the rule structure, the program skips the rule.

### Reassembly rules
| Expression          | Description                      |
| :------------------ | :------------------------------- |
| `<word> ... <word>` | return the phrase exactly        |
| `$<number>`         | return `<number>`th word on list |

## To do

### Bug tracker
- parsers are space-insensitive but not output

### Original features not yet implemented
- [ ] Memory
- [ ] Redirections such as `(=what)`
- [ ] `newkey` reassembly
- [ ] Match on groups

## References
<a id="1">[1]</a>
- Joseph Weizenbaum. 1966. ELIZA — A Computer Program for the Study of Natural Language Communication Between Man and Machine. Commun. ACM 9, 1 (Jan. 1966), 36–45. DOI: https://doi.org/10.1145/365153.365168


[build-img]: https://github.com/iagoleal/eliza/actions/workflows/ci.yml/badge.svg?branch=master
[build-url]: https://github.com/iagoleal/eliza/actions/workflows/ci.yml
