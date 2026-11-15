# Artwork and reuse

## Logo

[`man/figures/logo.svg`](../man/figures/logo.svg) was created for rcards from
simple geometric paths, rectangles, and circles. An AI-assisted visual concept
informed the design; the committed asset is editable SVG source. The navy,
green, gold, and ivory palette follows the package's graphical table.

The mark combines three cards: `r` on the left, a spade with a gold diamond in
the middle, and `cards` on the right. The lettering is drawn directly as
original vector paths. It contains no
third-party card illustrations, stock icons, casino branding, official R logo,
embedded fonts, scripts, or remote resources. No font file is redistributed
with the logo.

The app bundles the same SVG at `inst/app/logo.svg` for installed-package use.
Keep that copy byte-for-byte identical when updating the logo.

## Table preview

[`man/figures/table-scopa.png`](../man/figures/table-scopa.png) is a screenshot
of the rcards local Shiny interface with a seeded three-player Scopa game.
The player names and cards are generated demonstration data. The screenshot
contains the app's own HTML/CSS card faces and backs.

## License

The logo and table preview are distributed under the repository's
[Apache License 2.0](../LICENSE). No third-party artwork or font files are
bundled with them. This provenance record does not claim a trademark search or
legal clearance.
