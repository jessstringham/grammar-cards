
"word" is word in the target language
"translation" is English translation of the word

File structure
==============
Data/
  Swedish/
    grammarbook.yaml
    words.yaml
  German/
    grammarbook.yaml
    words.yaml

Grammar book yaml
=================

Section
-------

| section | String | name of the section |
| concepts | [Concept] | a list of concepts belonging to this section |


Concept
-------

| concept | String | name of the concept |
| wordlist | [String] | list of strings with the names being used in template strings |
| conceptTrait | [SituationTemplate] | Expands to things added to Situation. |
| situations | [Situation] | |

Situation
---------

| situation | String | |
| front | Template | A template of the front of the card. Usually a translation. |
| rules | [Rule] | The first onen will be used by default |

Rule
----
| rule | String | |
| back | Template | A template of the back of the card. Usually the word. |


SituationTemplate
-----------------

| TranslateEachWord | Each word in the WordSet becomes its own situation, with the front being the translation, and back being the word. |


Template
--------

S -> <W>S
S -> (W)S
S -> [a-Z ]S   # We might have a bunch of words and spaces. Consume!
S -> |X|X|S      # We've reached a modification rule!
X -> [a-z?]*     # Rules are case insensitive
S -> <W>S        # We've reached a translated word!
W -> [a-Z]+      # Translated words can only contain a-Z with no spaces
S -> ε           # We are done.


<present_tense>|r?|de|

| <word> | Replaced by that "word" from the wordset |
| \|x\|y\| | remove x letters. add y letters |
| x? | Remove these letters if they exist |




