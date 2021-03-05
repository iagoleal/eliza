# Eliza Chatbot

## To do
- [x] Keyword finding parser
- [x] Keyword reflections
- [x] Disassembly -> Reassembly parser
- [x] Adapt script from paper to JSON
- [x] JSON reader for script
- [ ] Rewrite the main script with state
- [ ] CLI interface

### Bugs:
- parsers are space-insensitive but not output

### Original features not yet implemented
- [ ] Memory
- [ ] Redirections such as `(=what)`
- [ ] `newkey` reassembly
- [ ] Match on groups

## Rules

Notice: Decomposition rules are word based but reassembly rules return spaces exactly.

Words in decomposition rules only accept alphanumeric characters

### Decomposition
| `bla`              | matches exactly the word "bla"       |
| `*`                | matches all                          |
| `#<number>`        | matches exactly <number> words       |
| `[bla1 bla2 bla3]` | matches any of the words             |
| `@group`           | matches a keyword from group "group" |

### Reassembly
| `bla bla bla` | return exactly               |
| `$<number>`   | return nth word on read list |

## Bibliography
- Joseph Weizenbaum. 1966. ELIZA — A Computer Program for the Study of Natural Language Communication Between Man and Machine. Commun. ACM 9, 1 (Jan. 1966), 36–45. DOI:https://doi.org/10.1145/365153.365168
