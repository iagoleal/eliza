# Eliza Chatbot

| **Build Status** |
|:----------------:|
| [![Build Status][build-img]][build-url] |

This is a Haskell implementation of Joseph Weizenbaum's classic chatbot ELIZA,
based upon the description found on his 1966 ACM paper [[1]](#1).

The idea for ELIZA comes from the 60s, the golden era of symbolic artificial intelligence.
As such, its answers are based on textual pattern matching,
differently from the more modern chatbots based on machine learning.
Nevertheless, it was still an interesting project for putting Haskell's parsing capabilities into practice
and the result can be pretty fun.
If you are stressed while debuging some code on the middle of the night,
try starting an `eliza` REPL and have some chat. 

Since people say an asciinema is worth more than a thousand words...
[![asciicast](https://asciinema.org/a/400665.svg)](https://asciinema.org/a/400665)

# Installation

The easiest and recommended way to install s using [stack](https://docs.haskellstack.org/en/stable/README/).
To directly install the executable to your path (Stack by default uses `~/.local/bin/`), run

```
stack setup && stack install
```

If you just want to run the program to try it out without any installation or copy,
you can use the one-liner

```
stack setup && stack run
```

# Usage

This program consists of a simple REPL-like conversation between you and a program following a certain script.
The script to the bot is a JSON file and should be passed as the first command line argument:

```
eliza path/to/script.json
```

If no argument is passed, the program defaults to (kind of) emulate a Rogerian psychiatrist.

## Writing a script

The bot bases its answers on decomposition / reassembly rules
defined in a JSON file who acts as a "script"
(in the screenwriter sense, not the computer program sense).
The program comes with the doctor script from Weizenbaum's paper already bundled
and loaded by default but you can write any set of rules you want.
See the folder `scripts/` for examples of ready to use scripts.

Note: on what follows, `<word>` stands for a sequence of alphanumeric characters
and `<number>` for a sequence of digits representing a positive number.

A script consists of a single JSON object that must have the following fields:

| Field           | Type             | Optional                |
|:----------------|:-----------------|:------------------------|
| `"greetings"`   | Array of strings | No                      |
| `"goodbyes"`    | Array of strings | No                      |
| `"default"`     | Array of strings | No                      |
| `"groups"`      | Object           | Yes (defaults to empty) |
| `"reflections"` | Object           | Yes (defaults to empty) |
| `"keywords"`    | Array of objects | No                      |

The fields `"greetings"`, `"goodbyes"`, and `"default"`
store, respectively, the program's possible initial messages,
possible final messages, and the default replies to choose from
when the input doesn't match the decomposition rules for any keyword.

The optional field `"groups"` is a dictionary storing in the field `"<word>"`
an array of strings. Any decomposition rule referring to this group
matches any element of the array.

The optional field `"reflections"` is a dictionary storing strings.
These are the system's simplest rewriting rules,
If there is a field of the form `"<word1>": "<word2>"`,
the program will substitute any occurrence of `<word1>` in the input for `<word2>`.
Notice that this exchange happens _after_ the keyword scanning
but _before_ testing the decomposition rules.

The field `"keywords"` store an array of objects following a specific schema.

| Field          | Type             | Optional                |
|:---------------|:-----------------|:------------------------|
| `"keyword"`    | String           | No                      |
| `"precedence"` | Integer          | Yes (defaults to 0)     |
| `"rules"`      | Array of objects | No                      |
| `"memory"`     | Array of objects | Yes (defaults to empty) |

Both `"rules`" and `"memory"` store the same kind of object, herein called a rule.
Each rule is itself an object with two fields:
`"decomposition"` storing a string and
`"reassembly"` storing an array of strings.
These represent the decomposition rule and choices of reassembly rules
in the case of a match.
How to write them is explained below.

### Decomposition rules

There are two options for a decomposition rule.
Either it is of the form `"=<word>"`,
in which case it tells the program to try the rules associated with the keyword `<word>`,
or it consists of a string of space separated expressions as described below.

| Expression            | Description                         |
|:----------------------|:------------------------------------|
| `<word>`              | match exactly `<word>`              |
| `*`                   | match zero or more words            |
| `#<number>`           | match exactly `<number>` words      |
| `[<word> ... <word>]` | match any of word in the list       |
| `@<word>`             | match a keyword from group `<word>` |

Each expression matches a certain kind of word or phrase.
So, for example, the rule `"* I [love hate] #4 @family *"`
can only match a phrase consisting of anything followed by the word `you`,
followed by one of `love` or `hate`, followed by exactly two words,
followed by any word of the group `family` followed by anything.
This means that the phrase `"I love to ski with my children on cold weekends"`
matches into the list `["", "I", "love", "to ski with my", "children", "on cold weekends"]`.
If a phrase does not match the rule structure, the program skips the rule.

### Reassembly rules

There are three kinds of reassembly rule.
It can be a newkey directive, `":newkey"`,
telling the program to continue looking for keywords on the stack,
it can point to another keyword, `"<word>"`,
or it can be a proper reassembly rule,
that is, a string where the special term `$<number>` may appear.
The reassembly process substitutes `$<number>` by the `<number>`th element
of the decomposed input. No length checks are made,
so you must guarantee that the decomposition has enough elements.

| Expression                        | Description                                         |
|:----------------------------------|:----------------------------------------------------|
| `:newkey`                         | try rules for next keyword on the stack             |
| `=<word>`                         | try rules for keyword `<word>`                      |
| `<word> ... $<number> ... <word>` | interpolate `<number>`th word on the list on string |

## References
<a id="1">[1]</a>
Joseph Weizenbaum. 1966. ELIZA — A Computer Program for the Study of Natural Language Communication Between Man and Machine. Commun. ACM 9, 1 (Jan. 1966), 36–45. DOI: https://doi.org/10.1145/365153.365168


[build-img]: https://github.com/iagoleal/eliza/actions/workflows/ci.yml/badge.svg?branch=master
[build-url]: https://github.com/iagoleal/eliza/actions/workflows/ci.yml
