
Grammar Cards
=============

Oh dear, I need a better name for this.

Motivation
----------

The point of this project is to fill the gap between learning grammar rules and committing them to memory.

It's also because I really like grammar rules, even though I'm terrible at languages. I want to be a bit better at languages.

People say you should throw away your grammar books and just practice the language.
That's nice, and they're probably right.

But when I form mnemonics, I really, really like when they are actually based on the actual
rules that underly the language. I have my neverending set of flashcards, but it would be nicer if I
could just remember that darn rule of how to make plurals in Swedish. 

The Real Motiviation
--------------------
The real point is for me to learn Haskell and other not-work things.

So I write super, erm, interesting Haskell; learn of new ways to do things in Haskell; completely
rewrite half of the codebase in this new way that might not actually be better; and repeat.

Contributing to the interestingness is that this is a personal project that I work on after my full-time job coding. So for most of its history, erm, I didn't really worrying about making neat and tidy commits, or removing dead code, or naming variables well, or organizing the folder structure in a sane way, or writing code sober, etc. Gonna try to clean this up now that it's in Github though.

Overview
--------

This is how it will work for the user.

1. Make a set of rules using the templating system.
2. Add a few examples. Assign rules and exceptions to the examples.
3. This program will try to apply the rules. It will give you a list of flashcards with special tags.
4. You load them into a flashcard program of your choice (but probably Anki or something else I can get stats for)
5. Periodically, you download the flashcard stats, and upload them. My program will break down what you need to study by the rules, examples, and exceptions.

All but the last step is done!

Usage
-----
program words.yaml grammarbook.yaml
Where words.yaml is Example Yaml File, and grammarbook.yaml is a Grammar Book Yaml File


File structure
==============
Data/
  Swedish/
    grammarbook.yaml
    words.yaml
  German/
    grammarbook.yaml
    words.yaml



Grammar Book Yaml File
----------------------
```yaml
- section: Nouns  # For organizational purposes
  concepts:
  - concept: Nouns  # Each concept needs to share a wordlist, situation list, 
    wordlist:  # This is the list of words we can use in templates
    - noun
    situations:
    - situation: Translation  # This is created by default if you use Translation concept trait, but you can also override it.
      front: <_noun>  # A template of the front of the card.
      rules:
      - rule: DEFAULT # If we look up a rule and fail, we fall back on DEFAULT cards
        back: <noun>
    - situation: Indefinite Article
      front: (a <_noun>)
      rules:
      - rule: en
        back: en <noun>
      - rule: ett
        back: ett <noun>
```

Section
-------

| section | String | Name of the section (for organization). |
| concepts | [Concept] | |


Concept
-------

| concept | String | Name |
| wordlist | [String] | List of word slot names. See Template's <word> |
| situations | [Situation] | |
| situation_templates | [SituationTemplate] | |

Situation
---------

| situation | String | |
| front | Template | A template of the front of the card. Usually a translation. |
| rules | [Rule] | A list of rules that address this situation. |

Rule
----
| rule | String | |
| back | Template | A template of the back of the card. Usually the word. |


SituationTemplate
-----------------

| name | String | Name |
| cards | [RuleApplication] | Examples with this situation template will have all of these rule applications. |


Words Yaml
==========

List of examples

Group
-----

| section | String | Name of section. Maps to grammarbook. |
| concept | String | Name of concept. Maps to grammarbook. |
| words | [Example] | |

Example
--------
| translations | [Word] | |
| rule | Name | Maps to a name of grammarbook's Rule |
| ruleTemplates | [String] | Maps to a name of grammarbook's SituationTemplates |
| exceptions | [Exception] | |

Exception
----------
| situation | String | |
| newFront | String | Use this for the front of the card. |
| newBack | String | Use this for the back of the card. |

Word
----
| label | String | Maps to entries in grammarbook's wordlist. |
| word | String | The target-language string. |
| translations | String | English string. |



Templating System
-----------------

| Template  | Description  | Example                     |
| --------- | ------------ | --------------------------- |
| <word>    | word         | <heart> -> hjärta           |
| <_word>   | translation  | <_heart> -> heart           |
| /ae/a/    | replace      | <_heart>e/ae/a/n -> hjärtan |


<_heart>e/ae/a/n -> hjärtae/ae/a/n -> hjärtan

