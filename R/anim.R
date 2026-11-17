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
#'   `<div>` fixed at `to` so content after it does not move. When `x` is a
#'   bslib page such as [bslib::page_fillable()], the page body itself is
#'   animated, which shows how fill items respond to the available height.
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
  if (inherits(x, "bslib_page")) {
    return(anim_page_height(x, from, to, duration))
  }

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

# A bslib page fills the frame, so its body is animated in place rather than
# being wrapped, which would stop it from being treated as a page
anim_page_height = function(x, from, to, duration) {
  rule = paste0(
    "body { ",
    htmltools::css(
      `--tagframe-from` = htmltools::validateCssUnit(from),
      `--tagframe-to` = htmltools::validateCssUnit(to),
      `--tagframe-duration` = duration
    ),
    " }"
  )
  x[[length(x) + 1]] = htmltools::tags$head(
    htmltools::tags$style(htmltools::HTML(rule))
  )
  x[[length(x) + 1]] = htmltools::tags$script(htmltools::HTML(
    'document.body.classList.add("tagframe-anim-page-height");'
  ))
  htmltools::attachDependencies(x, anim_dependency(), append = TRUE)
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
