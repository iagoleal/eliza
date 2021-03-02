# Eliza Chatbot

## To do
[x] Keyword finding parser
[x] Keyword reflections
[x] Disassembly -> Reassembly parser
[ ] Add state to the main script, so greetings are random
[ ] Adapt script from paper to JSON
[x] JSON reader for script
[ ] CLI interface
[ ] Memory

Modifications to matching rules:
  - Different types to decomp and recomp
  - Decomp: MatchAll | MatchNWords Int | MatchText Text | MatchAny [Text]
  - Recomp: ReturnNthWord Int | ReturnText Text
  - Should use a lexer and divide everything into words

Optimizations (not priority):
  - Store keywords in a Map

## Bibliography
- Joseph Weizenbaum. 1966. ELIZA — A Computer Program for the Study of Natural Language Communication Between Man and Machine. Commun. ACM 9, 1 (Jan. 1966), 36–45. DOI:https://doi.org/10.1145/365153.365168
