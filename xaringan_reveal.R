# reveal lines up to `upto` and highlight lines `highlight`
reveal <- function(name, upto, highlight = upto) {
  content <- knitr:::knit_code$get(name)
  content[upto] <- gsub("%>%\\s*(#.+)?$", "\\1", content[upto])
  content[upto] <- gsub("\\+\\s*(#.+)?$", "\\1", content[upto])
  content[upto] <- gsub("->\\s*(#.+)?$", "\\1", content[upto])	
  content[highlight] <- paste(content[highlight], "#<<")
  content[1:upto]
}

partial_knit_chunks <- function(chunk_name) {
  # Create slide for lines 1:N for each line N in the given chunk
  idx_lines <- seq_along(knitr:::knit_code$get(chunk_name))
  partial_knit_steps <- glue::glue(
    "class: split-40",
    "count: false",
    "",
    ".column[.content[",
    "```{r plot_{{chunk_name}}_{{idx_lines}}, eval=FALSE, code=reveal('{{chunk_name}}', {{idx_lines}})}",
    "```",
    "]]",
    ".column[.content.center[",
    "```{r output_{{chunk_name}}_{{idx_lines}}, echo=FALSE, code=reveal('{{chunk_name}}', {{idx_lines}})}",
    "```",
    "]]",
    .open = "{{", .close = "}}", .sep = "\n"
  )
  glue::glue_collapse(partial_knit_steps, "\n---\n")
}

apply_reveal <- function(chunk_name){
  paste(knitr::knit(text = partial_knit_chunks(chunk_name)), collapse = "\n")
}
