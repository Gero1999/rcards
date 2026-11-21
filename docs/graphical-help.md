# Help at the table

The graphical header uses the package's original SVG logo. The installed copy
under `inst/app/logo.svg` is kept identical to `man/figures/logo.svg`; it is
embedded locally and does not load fonts, artwork, or services from the network.
See [artwork provenance and licensing](branding.md).

The Help button opens an accessible dialog with three sections:

- An explanation of local human-versus-bot play, legal controls, and session resets.
- Instructions for the game currently at the table, including its continuation controls.
- Links to report bugs or propose a game in the `Gero1999/rcards` GitHub repository.

Instructions follow the app's selected English, Spanish, German, or Italian
language. Game selection for a future table does not change the help until that
table is started. The dialog uses the current view's canonical game type and
does not read hidden cards or controller state. GitHub links open only when
the user chooses them.

The short instructions describe the supported software profile; the rule link
opens the more detailed repository documentation. Custom game settings still
govern the engine. Keep translated instructions and the rule profiles aligned
when adding games or changing controls.
