# Grammar Cards

Oh dear, I need a better name for this.


## Motivation

The point of this project is to fill the gap between learning grammar rules and committing them to memory.

It's also because I really like grammar rules, even though I'm terrible at languages. I want to be a bit better at languages.

People say you should throw away your grammar books and just practice the language. Which is fine, and they're probably right, but when I form mnemonics, I really, really like when they are actually based on the actual
rules that underly the language. I have my neverending set of flashcards, but it would be nicer if I
could just remember that darn rule of how to make plurals in Swedish.

You transcribe a set of simple rules for your target language into this template language. Then you define a list of words, and this program will produce a list of flashcards you can upload into the SRS of your choice. Eventually, you'll be able to use a second program to view what grammar rules you should focus on.

## uh, the secret motiviation

The real point is for me to learn Haskell and other technologies.

So I write super, erm, interesting Haskell; learn of new ways to do things in Haskell; completely
rewrite part of the codebase in this new way that might not actually be better; and repeat.

And a disclaimer: Contributing to the interestingness is that this is a personal project that I work on after my full-time job coding. So for most of its history, erm, I didn't really worrying about making neat and tidy commits, or removing dead code, or naming variables well, or organizing the folder structure in a sane way, or writing code sober, etc. So not the best example of clean code, but whatev's. Gonna try to clean this up now that it's in Github though.


# Program Overview

This is how it will work for the user.

1. Make a set of rules using the templating system.
2. Add a few examples. Assign rules and exceptions to the examples.
3. This program will try to apply the rules. It will give you a list of flashcards with special tags.
4. You load them into a flashcard program of your choice (but probably Anki or something else I can get stats from).
5. Periodically, you download the flashcard stats, and upload them. My program will break down what you need to study by the rules, examples, and exceptions.

All but the last step is done!


## Script usage

```
./GrammarCardsGenerator grammarbook.yaml words.yaml
```
And the output looks something like

```
(a chair)	en stol	Indefinite_Article en second_declension
(a book)	en bok	Indefinite_Article en second_declension
(the chair)	stolen	Definite_Article en second_declension
(the book)	boken	Definite_Article en second_declension
(several chair)	stolar	Indefinite_Plural ar second_declension
(several book)	bocker	Indefinite_Plural ar second_declension (exception)
```


## grammarbook.yaml file

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
      - rule: DEFAULT  # If there isn't a rule defined for this situation, use the DEFAULT one
        back: <noun>
```


### Section

field | type | description
--- | --- | ---
section | String | Name of the section (for organization).
concepts | [Concept] |


### Concept

field | type | description
--- | --- | ---
concept | String | Name
wordlist | [String] | List of word slot names. See Template Language's &lt;word&rt;
situations | [Situation] |
situation_templates | [SituationTemplate] |


### Situation

 field | type | description
 --- | --- | ---
 situation | String |
 front | Template | A template of the front of the card. Usually a translation.
 rules | [Rule] | A list of rules that address this situation.


### Rule

field | type | description
--- | --- | ---
rule | String |
back | Template | A template of the back of the card. Usually the word.


### SituationTemplate

 field | type | description
 --- | --- | ---
 name | String | Name
 cards | [RuleApplication] | Examples with this situation template will have all of these rule applications.


## words.yaml

List of examples.

```
  - wordset:
    - name: noun
      word: bok
      translation: book
    ruleTemplates:
    - second declension  # This will fill in a bunch of rules for us. Cool!
    exceptions:
    - situation: Indefinite Plural
      new_front: DEFAULT  # Leave this field the way it is
      new_back: bocker
```



### Group

 field | type | description
 --- | --- | ---
 section | String | Name of section. Maps to grammarbook.
 concept | String | Name of concept. Maps to grammarbook.
 words | [Example] |

### Example

field | type | description
--- | --- | ---
translations | [Word] |
rule | Name | Maps to a name of grammarbook's Rule
ruleTemplates | [String] | Maps to a name of grammarbook's SituationTemplates
exceptions | [Exception] |

### Exception

field | type | description
--- | --- | ---
situation | String |
newFront | String | Use this for the front of the card.
newBack | String | Use this for the back of the card.

### Word

field | type | description
--- | --- | ---
label | String | Maps to entries in grammarbook's wordlist.
word | String | The target-language string.
translations | String | English string.


## Template Language

The front and backs of cards are defined using this (evolving) templating language:

Template     | Description  | Example
------------ | ------------ | ---------------------------
&lt;word> | word         | &lt;heart> → hjärta
<_word>      | translation  | <_heart> → heart
/ae/a/       | replace      | <_heart>e/ae/a/n → hjärtan


<_heart>e/ae/a/n → hjärtae/ae/a/n → hjärtan
