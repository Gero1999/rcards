# Console languages

Supported locales: en (default), es, de, it. Each console controller selects its
own language. Internal actions, ranks, suits, observations, and scores remain
canonical. Localized action aliases and English action identifiers are accepted;
menu numbers remain available. User names and custom card displays are not
translated as interface phrases. Missing catalog entries fall back to English.

Translate prompts, action explanations, game phases, outcomes, and expected
input/engine validation errors. Programmer diagnostics and arbitrary third-party
controller errors may fall back to English. This is not automatic machine
translation. Adding a locale requires catalog, alias, and card-label coverage.
