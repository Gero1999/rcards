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
## Quechua preview (`qu`)

This is a **draft bilingual interface**, not a complete reviewed Quechua
translation. A small core vocabulary is Quechua; card vocabulary, commands,
errors and game rules deliberately use Spanish pending fluent-speaker review.
The app selector and Help identify the draft. English commands also work.

The intended starting profile is Southern Quechua, a widely used family of
varieties rather than a universal Quechua standard. Before releasing a complete
translation, have a fluent Southern Quechua reviewer translate
all templates and Help paragraphs, and validate terminology in actual play.
Do not equate catalog coverage or passing software tests with linguistic accuracy.
