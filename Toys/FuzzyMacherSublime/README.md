# Sublime Style Fuzzy Matcher

Reimplement the fuzzy matcher used in these programs:

* Sublime Text
* Emacs orderless https://github.com/oantolin/orderless
* ?

Overall idea:

* User input multiple prefixes or whole words
* Each of the prefixes matches against every word in the target sentences
* The target sentence with at least one match will be considered as a result
* The target sentence with the highest matches count will be ranked higher

For example, both `hell` and `wor hel` matching `hello world` will return true and `wor hel` will rank higher.

TODO:

* split based on Cap letters
* sorting within results that have same ranks

Ref:

https://www.reddit.com/r/programming/comments/4cfz8r/comment/d1i7unr/
