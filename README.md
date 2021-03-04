# Eliza Chatbot

## To do
- [x] Keyword finding parser
- [x] Keyword reflections
- [ ] Disassembly -> Reassembly parser
- [ ] Add state to the main script, so greetings are random
- [x] Adapt script from paper to JSON
- [ ] JSON reader for script
- [ ] CLI interface


### Modifications to matching rules:
- Different types to decomp and recomp
- Decomp: MatchAll | MatchNWords Int | MatchText Text | MatchAny [Text]
- Recomp: ReturnNthWord Int | ReturnText Text
- Should use a lexer and divide everything into words

### Optimizations (not priority):
- Store keywords in a Map

### Bugs:
- parsers are case sensitive atm
- parsers are space-insensitive but not output

### Original features not yet implemented
- [ ] Memory
- [ ] Redirections such as `(=what)`
- [ ] `newkey` reassembly
- [ ] Match on groups

## Rules

### Decomposition
`*` -> matches all
`bla` -> matches exactly the word "bla"
`[bla1 bla2 bla3]` -> matches any of the words
`:group` -> matches a keyword from group "group

### Reassembly
`bla bla bla` -> return exactly
`$<number>` -> return nth word on read list

## Bibliography
- Joseph Weizenbaum. 1966. ELIZA — A Computer Program for the Study of Natural Language Communication Between Man and Machine. Commun. ACM 9, 1 (Jan. 1966), 36–45. DOI:https://doi.org/10.1145/365153.365168
