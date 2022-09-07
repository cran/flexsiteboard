#' @importFrom stats setNames
post_knit <- function(metadata, input_file, runtime, ...) {
  preprocessed_file <- file.path(getwd(),
                                 sub("\\.Rmd$", ".knit.md", input_file))
  target_path <- #gsub("_files$", "_subpages",
                      dirname(knitr::opts_chunk$get("fig.path"))#)
  redirect_path <- #gsub("_files$", "_subpages",
                      basename(dirname(knitr::opts_chunk$get("fig.path")))#)
  dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
  # file.copy(list.files(target_path), )
  oldwd <- setwd(target_path)
  on.exit(setwd(oldwd))
  target_path <- "."
  # on.exit({
  #   unlink(temp_path, recursive = TRUE, force = TRUE)
  # }, add = TRUE)
  htmlwidgets::JS("noop"
   ) # to suppress false positive NOTE about htmlwidgets not being used.
  preprocessed <- readLines(preprocessed_file)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", preprocessed)
  if (length(delimiters) > 1) {
    front_matter <- preprocessed[(delimiters[[1]]+1):(delimiters[[2]]-1)]
    preprocessed <-
      preprocessed[(delimiters[[2]] + 1) : length(preprocessed)]
  } else {
    front_matter <- ""
  }
  front_matter <-
    gsub("flexdashboard::flex_dashboard",
         "flexsiteboard::no_output",
         front_matter,
         fixed = TRUE)
  front_matter <-
    gsub("flexsiteboard::flex_site_board",
         "flexdashboard::flex_dashboard",
         front_matter,
         fixed = TRUE)
  block_lines0 <-
    grep("^^#+ .*\\{(.+ )?data-navmenu=\"[^\"]+\"\\}", trimws(preprocessed))
  # Hint: pre block from after the front matter up to first headline
  #           -> copy to each split-file
  if (length(block_lines0)) {
    block0 <- preprocessed[seq_len(block_lines0[[1]] - 1)]
    preprocessed <- preprocessed[block_lines0[[1]]:length(preprocessed)]
    block_lines <-
      grep("^^#+ .*\\{(.+ )?data-navmenu=\"[^\"]+\"\\}", trimws(preprocessed))
    if (length(block_lines) > 1) {
      stopifnot(block_lines[[1]] == 1)
      block_lines_resolved <- mapply(`:`, 1 + (c(block_lines)),
                                     c(tail(block_lines - 1, -1),
                                       length(preprocessed)),
                                     SIMPLIFY = FALSE)

       html_headers <-
         pandoc_convert(text = preprocessed[block_lines],
                        from = "markdown",
                        to = "html5")

      ids <- tail(strsplit(
        html_headers,
        'id="',
        fixed = TRUE
      )[[1]], -1)
      ids <- gsub(
        "\".*$",
        "",
        ids
      )
      names(block_lines_resolved) <- ids
    }
    files <- lapply(block_lines_resolved,
                    function(l) preprocessed[l])

    files <- lapply(files, function(f) {
      p <- knitr::opts_chunk$get("fig.path")
      if (!startsWith(p, .Platform$file.sep)) {
        p2 <- # remove the first directory from the path
          intToUtf8(rev(utf8ToInt(dirname(intToUtf8(rev(utf8ToInt(p)))))))
        f <- gsub(p, p2, f, fixed = TRUE)
      }
      f
    })

    headlines <- setNames(nm = names(block_lines_resolved),
                          preprocessed[block_lines])

    html_dependencies <-
      knitr::knit_meta(clean = FALSE, class = "html_dependency")
    hash_index <- as.list(setNames(paste0(names(files),
                                          ".html#",
                                          ids), ids))
    for (f_i in seq_along(names(files))) {
      ### Function to plot histograms added by empirical cumulative distributions for subgroups\_0 {#res-s0_testx-functiontoplothistogramsaddedbyempiricalcumulativedistributionsforsubgroups_0}
      f <- names(files)[[f_i]]
      for (id in grep("\\{#.*?[}\\s]",
                      perl = TRUE,
                      value = TRUE,
                      files[[f_i]])) {
        id <- gsub("^.*\\{#(.*?)[}\\s].*$", # TODO: lines with two ids
                   "\\1",
                   perl = TRUE,
                   id)
        hash_index[[id]] <- paste0(f, '.html#', f, '#', id)
      }
    }

    hash_index <- lapply(hash_index,
                         jsonlite::unbox)
    for (f_i in seq_along(names(files))) {
      f <- names(files)[[f_i]]
      cat(
        c('---', front_matter, '---',
          '```{js echo=FALSE}','
              function goto_sub_anchor(flx, sub) {
                window.location.href = flx + ".html#" + flx;
                $(function() {
                  window.setTimeout(function() {
                    $([document.documentElement, document.body]).animate({
                      scrollTop: $("#" + sub).offset().top
                    }, 100);
                  }, 500)
                })
              }

              var hash_index = ',
          jsonlite::toJSON(hash_index)
          , ';
              $(document).ready(function() {
                $(\'#dashboard-container\').on(\'flexdashboard:layoutcomplete\', function() {
                  $(\'a[href!="#"][href^="#"]\').click(function() {
                    // TODO: Maybe, we should replace the hrefs of all these tags instead of doing this dynamically?
                    var alternative_target = hash_index[decodeURI(this.hash.replace("#", ""))]
                    if (alternative_target != undefined) {
                      location.replace(alternative_target);
                    }
                  });
                })
              })
              var verify_target = function() {
                var flx = undefined;
                var sub = undefined;
                if ((location.hash.match(/#/g) || []).length == 2) { // https://stackoverflow.com/a/4009768
                  flx = location.hash.match(/#(.*?)#(.*)/)[1]
                  sub = location.hash.match(/#(.*?)#(.*)/)[2]
                  goto_sub_anchor(flx, sub);
                  return;
                } else {
                  flx = location.hash.replace("#", "")
                  if (flx == "") {
                    flx = location.href.split("/").reverse()[0].replace(/\\.html#?$/i, "");
                    location.replace(flx + ".html#" + flx);
                    return;
                  }
                  var my_target = hash_index[decodeURI(flx)]
                  if (my_target != undefined) {
                    if (!location.href.endsWith("/" + my_target)) {

                      var m = my_target.match(/#(.*?)#(.*)/);

                      if (m != null && m.length > 2) {
                        flx = m[1];
                        sub = m[2];
                      } else if (my_target.match("#") != null) {
                        var m2 = my_target.match(/^(.*?)(?:\\.html)?#(.*?)$/);
                        flx = m2[1];
                        sub = m2[2];
                      } else {
                        flx = my_target;
                        sub = "";
                      }

                      if (!location.href.endsWith(flx + ".html#" + sub)) {
                        location.replace(flx + ".html#" + sub);
                        return;
                      }

                    }
                  }
                }
                window.setTimeout(verify_target, 100); // just to be on the safe side */
              }
              $(function() {
                window.setTimeout(verify_target, 100);
              });
              ',
          '```',
          '```{r include=TRUE}',
          'library(htmlwidgets)',
          # 'html_dependencies <- ',
          # deparse(html_dependencies),
          'knitr::knit_print(htmlwidgets::createWidget(name = "flex_site_board_sub_dashinit", x = "TEST!!", dependencies = html_dependencies, height = "0px", width = "0px"))',
          '```',
          block0,
          paste0(
            headlines[seq_len(f_i)]
          ),
          "\n",
          files[[f]],
          "\n",
          headlines[seq_len(length(files))[seq_len(length(files)) > f_i]]
        ),
        file = paste0(f, ".Rmd"),
        sep = "\n"
      )
      # dir.create(file.path(temp_path, target_path, "libs"), recursive = TRUE,
      #            showWarnings = FALSE)
      e <- new.env(parent = baseenv())
      e$html_dependencies <- html_dependencies
      message(sprintf("\nWrote file %s\n", rmarkdown::render(
        quiet = TRUE,
        input = paste0(f, ".Rmd"),
        envir = e,
        # output_options = list(
        #   lib_dir = file.path(target_path, "libs")
        # ),
        output_dir = target_path,
        output_file = paste0(f, ".html")
      )))
      try(unlink(paste0(f, ".Rmd")))
    }
    # cat(preprocessed, file = preprocessed_file, sep = "\n")
    front_matter_parsed_idx <- yaml::read_yaml(text = front_matter)
    # this does not work, seems to be already part of knit_meta here:
    # front_matter_parsed_idx$output$`flexdashboard::flex_dashboard`$includes =
    #   list(in_header = "redirect_header.html")
    knitr::knit_meta_add(list( # see https://rmarkdown.rstudio.com/docs/reference/output_format.html
      htmltools::htmlDependency(
        name = "flex-site-board-index-redirector",
        version = "0.0.1",
        src = c(file = system.file("",
                                   package = "flexsiteboard")),
        script =  "flexsiteboard.js",
        head =
          sprintf(
            '<meta http-equiv="refresh" content="0; url=%s.html"/>',
            file.path(redirect_path, names(files)[[1]])
          )
      )
    ))
    cat(
      '---',
      yaml::as.yaml(front_matter_parsed_idx),
      '---',
      sprintf('Go to [%s](%s.html)',
              front_matter_parsed_idx$title,
              file.path(redirect_path, names(files)[[1]])),
      htmltools::renderTags(htmltools::tags$script(htmltools::HTML(sprintf(
        '\n//<![CDATA[\nlocation.replace("%s.html");\n//]]>\n',
        file.path(redirect_path, names(files)[[1]])
      )), type = "text/javascript"))$html,
      file = preprocessed_file,
      sep = "\n"
    )
  }
  if (exists("orig", .props, mode = "function")) {
    get("orig", .props, mode = "function")(metadata, input_file, runtime, ...)
  } else {
    NULL
  }
}
