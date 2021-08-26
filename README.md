# flagmashr

This is experimental, not guaranteed to work and entirely pointless. It will only work in RStudio.

This is a package to mash flags together. The function `flagmash(country1, country2)` takes the colours of the flag for `country1` and applies them to the flag of `country2`. The arguments `country1` and `country2` must be two character ISO 3166-1 codes (e.g. IE for Ireland). Flags are also given names using the adjective of the first country and the name of the second country.

## Inspiration
`flagmashr` is inspired by the wonderful [Flags Mashup Bot on Twitter](https://twitter.com/FlagsMashupBot) and is a very poor imitation of the bot's fantastic mashups that attempts to do the same but in R.

## Sources
The flags are sourced from Lipis' [`flag-icons-css`](https://github.com/lipis/flag-icon-css/) repo which is a git submodule at `inst/extdata/flag-icons-css`.

Adjectives are sourced from [Wikipedia's list of adjectives](https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations).

Country names are sourced via Vincent Arel-Bundock's excellent [countrycode package](https://vincentarelbundock.github.io/countrycode)
