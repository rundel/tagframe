#' Animate the width or height of a tag
#'
#' Wraps a tag so that its width (or height) cycles between two values with a
#' CSS animation. This is useful for demonstrating responsive layouts, such as
#' [bslib::layout_column_wrap()], inside a fixed-size iframe. The required CSS
#' is attached as an HTML dependency so nothing else needs to be included in
#' the page.
#'
#' @param x An htmltools tag.
#' @param from,to The two sizes to animate between, as CSS units or numbers of
#'   pixels.
#' @param duration Length of one animation cycle, as a CSS time (e.g. `"6s"`).
#'
#' @return The tag `x` with animation classes, CSS variables, and the
#'   stylesheet dependency attached. `anim_height()` returns `x` inside a
#'   `<div>` fixed at `to` so content after it does not move.
#'
#' @examples
#' anim_width(htmltools::div("Hello"), "100%", "33%")
#'
#' @export
anim_width = function(x, from, to, duration = "6s") {
  x |>
    htmltools::tagAppendAttributes(
      class = "tagframe-anim-width",
      style = htmltools::css(
        `--tagframe-from` = htmltools::validateCssUnit(from),
        `--tagframe-to` = htmltools::validateCssUnit(to),
        `--tagframe-duration` = duration
      )
    ) |>
    htmltools::attachDependencies(anim_dependency(), append = TRUE)
}

#' @rdname anim_width
#' @export
anim_height = function(x, from, to, duration = "6s") {
  inner = x |>
    htmltools::tagAppendAttributes(
      class = "tagframe-anim-height",
      style = htmltools::css(
        `--tagframe-from` = htmltools::validateCssUnit(from),
        `--tagframe-to` = htmltools::validateCssUnit(to),
        `--tagframe-duration` = duration
      )
    )

  htmltools::div(
    style = htmltools::css(height = htmltools::validateCssUnit(to)),
    inner
  ) |>
    htmltools::attachDependencies(anim_dependency(), append = TRUE)
}

anim_dependency = function() {
  htmltools::htmlDependency(
    name = "tagframe-anim",
    version = as.character(utils::packageVersion("tagframe")),
    src = "css",
    package = "tagframe",
    stylesheet = "anim.css"
  )
}
