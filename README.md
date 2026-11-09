# tagframe

A small knitr engine for embedding rendered htmltools content (bslib components, htmlwidgets, Shiny UI elements) in Quarto slides and HTML documents.

Bootstrap and reveal.js do not share a page gracefully, so each chunk's result is shown through an iframe with its own Bootstrap page. The engine renders the tags with htmltools, inlines every dependency once per document, stores each frame's body as JSON, and a small script assembles the iframe `srcdoc` when the page loads. The output stays a single self-contained HTML file: no per-chunk renders, no template files, and no sidecar folders.

## Installation

```r
# install.packages("pak")
pak::pak("rundel/tagframe")
```

## Usage

Load the package in a setup chunk:

```r
library(tagframe)
```

This registers the `tagframe` knitr engine (with `bslib` as an alias). A chunk such as

````
```{bslib}
#| height: 250
#| output-location: column
card(
  card_header("A header"),
  card_body("Some text")
)
```
````

evaluates the code, wraps the result in `bslib::page()` (unless it already is a bslib page such as `page_fillable()`), and emits an iframe showing it. Objects and attached packages from earlier R chunks are visible inside the chunk. The last expression must return an htmltools tag, tag list, or htmlwidget.

### Chunk options

| Option   | Description                                                      | Default                                       |
|----------|------------------------------------------------------------------|-----------------------------------------------|
| `height` | Iframe height, a CSS unit or number of pixels                    | `"100%"`                                      |
| `width`  | Iframe width, a CSS unit or number of pixels                     | `"100%"`                                      |
| `theme`  | R code (as a string) evaluating to a `bslib::bs_theme()`         | `getOption("tagframe.theme")` or `bs_theme()` |
| `title`  | Page title                                                       | none                                          |

The usual knitr and Quarto options (`echo`, `label`, `output-location`, ...) work as normal.

### Animating sizes

`anim_width()` and `anim_height()` wrap a tag so its size cycles between two values, which is handy for demonstrating responsive layouts:

````
```{bslib}
#| height: 300
layout_column_wrap(
  width = "200px",
  card("One"), card("Two"), card("Three")
) |>
  anim_width("100%", "40%")
```
````

## Limitations

Font files referenced from stylesheets are not inlined, so frames use system fonts rather than the web fonts of a Bootstrap preset.
