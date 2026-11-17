# Display languages

Portuguese (`pt`) uses Brazilian terminology. Language codes select display
text and console aliases; canonical actions, card values and suits stay fixed.
English action names and numbered menus remain accepted in every language.

The language selector includes translated card descriptions, session controls,
and in-app Help. New catalogs are separate files under R/zz_language_*.R.
Tests check template coverage, interpolation and canonical card identities.

Mandarin Chinese (`zh`) uses Simplified Chinese. Standard French ranks retain
A/J/Q/K labels; suit descriptions and gameplay instructions are Chinese. Card
descriptions put suit before rank. Numbered menus and English commands remain
available when a terminal cannot enter Chinese characters.

Traditional Mandarin (`zh-Hant`) is independently selectable, including Help
and commands. Translations use Traditional characters without changing game
identities or numeric ranks. Chinese output needs a UTF-8 terminal and a font
with CJK coverage; English aliases and menu numbers remain available.
