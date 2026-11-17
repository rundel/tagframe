#' knitr engine for embedding rendered tags as iframes
#'
#' This engine is registered as `tagframe` (and `bslib` as an alias) when the
#' package is loaded, so a chunk written as ```` ```{tagframe} ```` is handled
#' by it. The chunk code is evaluated in a child of the knitting environment,
#' so objects and attached packages from earlier R chunks are visible. The
#' value of the last expression must be an htmltools tag, tag list, or
#' htmlwidget. It is wrapped in a [bslib::page()] (unless it already is a
#' bslib page) and shown in the knitted document inside an `<iframe>`, which
#' keeps its Bootstrap CSS and JavaScript isolated from the surrounding
#' reveal.js or HTML document.
#'
#' The output is fully self-contained. The frame's body is stored in the
#' document as JSON, each HTML dependency (Bootstrap, jQuery, bslib
#' components, htmlwidgets, ...) is inlined once per document, and a small
#' script assembles the iframe `srcdoc` when the page loads. Nothing is
#' written next to the document. Font files referenced by stylesheets are not
#' inlined, so frames fall back to system fonts.
#'
#' Supported chunk options (in addition to the usual knitr ones such as
#' `echo`, `label`, and Quarto's `output-location`):
#'
#' * `height`, `width`: iframe size, a CSS unit or a number of pixels.
#'   Defaults are `"100%"`.
#' * `theme`: R code (as a string) evaluating to a [bslib::bs_theme()], used
#'   when wrapping a non-page result. Defaults to `getOption("tagframe.theme")`
#'   or `bslib::bs_theme()`.
#' * `title`: the page title.
#' * `scale`: a number such as `0.8` that renders the frame at `1 / scale`
#'   times its size and shrinks it to fit. Use it when a narrow frame would
#'   trigger a mobile layout, for example `page_fillable()` not filling below
#'   576px. Requires `height` in absolute units.
#' * `padding`: padding around a non-page result, a CSS unit or a number of
#'   pixels, so borders and shadows are not clipped by the iframe edge.
#'   Defaults to `getOption("tagframe.padding", "0.5rem")`. Results that are
#'   already a bslib page keep their own padding.
#'
#' @param options A list of chunk options, as passed by knitr.
#'
#' @return A character string of knitted output, as returned by
#'   [knitr::engine_output()].
#'
#' @examples
#' \dontrun{
#' # In a Quarto document, after library(tagframe) in a setup chunk:
#' # ```{tagframe}
#' # #| height: 200
#' # bslib::card(bslib::card_header("A header"), "Some text")
#' # ```
#' }
#'
#' @export
eng_tagframe = function(options) {
  code = paste(options$code, collapse = "\n")
  env = new.env(parent = knitr::knit_global())

  result = withCallingHandlers(
    eval(parse(text = code, keep.source = FALSE), envir = env),
    message = function(m) invokeRestart("muffleMessage")
  )

  if (!is_renderable(result)) {
    stop(
      "The last expression of a tagframe chunk must return an htmltools tag, ",
      "tag list, or htmlwidget, not an object of class <",
      paste(class(result), collapse = "/"), ">.",
      call. = FALSE
    )
  }

  theme = options$theme
  if (is.character(theme)) {
    theme = eval(parse(text = theme, keep.source = FALSE), envir = env)
  }
  theme = theme %||% getOption("tagframe.theme") %||% bslib::bs_theme()

  page = if (inherits(result, "bslib_page")) {
    result
  } else {
    padding = options$padding %||% getOption("tagframe.padding", "0.5rem")
    bslib::page(
      result,
      title = options$title,
      theme = theme,
      style = htmltools::css(padding = htmltools::validateCssUnit(padding))
    )
  }

  out = frame_html(
    page,
    id = options$label,
    width = options$width %||% "100%",
    height = options$height %||% "100%",
    scale = options$scale %||% 1
  )

  options$results = "asis"
  options$engine = "r"
  knitr::engine_output(options, options$code, out)
}

is_renderable = function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list", "htmlwidget", "html"))
}

# Dependencies already written into the current document, keyed by input file
emitted = new.env(parent = emptyenv())

frame_html = function(page, id, width, height, scale = 1) {
  rendered = htmltools::renderTags(page)
  deps = htmltools::resolveDependencies(rendered$dependencies)
  dep_ids = vapply(deps, function(d) paste0(d$name, "-", d$version), "")

  doc = knitr::current_input() %||% "<console>"
  seen = emitted[[doc]] %||% character()
  first = length(seen) == 0

  new = !dep_ids %in% seen
  emitted[[doc]] = c(seen, dep_ids[new])

  dep_scripts = mapply(
    function(dep, dep_id) {
      json_script(inline_dependency(dep), "data-tagframe-dep", dep_id)
    },
    deps[new], dep_ids[new],
    USE.NAMES = FALSE
  )

  body = list(
    head = as.character(rendered$head),
    body = as.character(rendered$html)
  )

  width = htmltools::validateCssUnit(width)
  height = htmltools::validateCssUnit(height)
  scaled = !isTRUE(all.equal(scale, 1))

  # A scaled frame is laid out larger by 1 / scale and shrunk back with a
  # transform, so its page sees a wider viewport than the space it occupies
  frame = htmltools::tags$iframe(
    `data-tagframe` = id,
    `data-tagframe-deps` = paste(dep_ids, collapse = " "),
    width = if (!scaled) width,
    height = if (!scaled) height,
    style = htmltools::css(
      border = "none",
      width = if (scaled) paste0("calc(", width, " / ", scale, ")"),
      height = if (scaled) paste0("calc(", height, " / ", scale, ")"),
      transform = if (scaled) paste0("scale(", scale, ")"),
      `transform-origin` = if (scaled) "top left",
      `max-width` = if (scaled) "none",
      `max-height` = if (scaled) "none"
    ),
    scrolling = "no"
  )

  if (scaled) {
    frame = htmltools::div(
      style = htmltools::css(width = width, height = height, overflow = "hidden"),
      frame
    )
  }

  html = c(
    if (first) runtime_script(),
    dep_scripts,
    json_script(body, "data-tagframe-body", id),
    as.character(frame)
  )

  paste0("```{=html}\n", paste(html, collapse = "\n"), "\n```\n")
}

json_script = function(x, attr, id) {
  json = jsonlite::toJSON(x, auto_unbox = TRUE)
  json = gsub("</", "<\\/", json, fixed = TRUE)
  paste0('<script type="application/json" ', attr, '="', id, '">', json, "</script>")
}

inline_dependency = function(dep) {
  dir = dep$src$file
  if (is.null(dir)) {
    return(htmltools::renderDependencies(list(dep), srcType = "href"))
  }
  if (!is.null(dep$package)) {
    dir = system.file(dir, package = dep$package)
  }

  read = function(f) paste(readLines(file.path(dir, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  meta = if (length(dep$meta)) {
    paste0('<meta name="', names(dep$meta), '" content="', htmltools::htmlEscape(unlist(dep$meta), attribute = TRUE), '">', collapse = "\n")
  }

  css = vapply(dep$stylesheet, function(f) {
    paste0("<style>", strip_relative_imports(read(f)), "</style>")
  }, "")

  scripts = dep$script
  if (!is.list(scripts)) {
    scripts = lapply(scripts, function(f) list(src = f))
  }
  # Attributes such as type = "module" change when a script runs, which
  # web components rely on, so they are kept on the inlined tag
  js = vapply(scripts, function(script) {
    attrs = script[setdiff(names(script), c("src", "defer", "async", "integrity", "crossorigin"))]
    attrs = if (length(attrs)) {
      paste0(" ", names(attrs), '="', htmltools::htmlEscape(unlist(attrs), attribute = TRUE), '"', collapse = "")
    } else {
      ""
    }
    code = gsub("</script", "<\\/script", read(script$src), fixed = TRUE)
    paste0("<script", attrs, ">", code, "</script>")
  }, "")

  paste(c(meta, css, js, dep$head), collapse = "\n")
}

# Relative @import rules (e.g. Bootstrap's font.css) cannot resolve from an
# inlined stylesheet, so drop them rather than request files that do not exist
strip_relative_imports = function(css) {
  gsub('@import\\s+url\\(["\']?(?![a-z]+:|//)[^"\')]*["\']?\\)[^;]*;', "", css, perl = TRUE)
}

runtime_script = function() {
  js = '
(function() {
  function parse(sel) {
    var el = document.querySelector(sel);
    return el ? JSON.parse(el.textContent) : null;
  }
  function mount(frame) {
    var ids = (frame.getAttribute("data-tagframe-deps") || "").split(" ").filter(Boolean);
    var deps = ids.map(function(id) { return parse("script[data-tagframe-dep=\\"" + id + "\\"]") || ""; });
    var page = parse("script[data-tagframe-body=\\"" + frame.getAttribute("data-tagframe") + "\\"]") || {head: "", body: ""};
    var body = /^\\s*<body[\\s>]/.test(page.body) ? page.body : "<body>" + page.body + "</body>";
    frame.srcdoc = "<!DOCTYPE html><html><head><meta charset=\\"utf-8\\">" +
      deps.join("\\n") + page.head + "</head>" + body + "</html>";
  }
  function run() {
    document.querySelectorAll("iframe[data-tagframe]").forEach(mount);
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", run);
  } else {
    run();
  }
})();
'
  paste0("<script>", js, "</script>")
}
