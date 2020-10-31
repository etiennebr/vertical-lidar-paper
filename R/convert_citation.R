# Find potential citations

# test:
content <- c(
  "blah (Wulder et al. 2012; White et al. 2013; Beland et al. 2019) blah",
  "blah blah (Ellsworth and Reich 1993; White et al. 2013) blah",
  "blah blah (Beland et al. 2019; Crespo-Peremarch et al. 2020) blah",
  "blah blah (White et al. 2013) blah",
  "blah blah (Ellsworth and Reich 1993) blah",
  "blah blah (Ellsworth and Reich 1993, 2000) blah",  # If date after comma (check it's not a page), repeat authors and add semicolon
  "(MacArthur and Horn 1969; Aber 1979; Bassow and Bazzaz 1997; Tackenberg 2007)",
  "nope (nope not this one) here"
  )

content_wrap <- c("do not wrap\n(MacArthur and Horn 1969; Aber 1979; Bassow and\nBazzaz 1997; Tackenberg 2007) and (not \n this)")
#' expect_equal(convert_selection(content), gsub("\n", " ", c("blah [@wulder_etal12;
#' @white_etal13; @beland_etal19] blah", "blah blah [@ellsworth_reich93;
#' @white_etal13] blah", "blah blah [@beland_etal19; @crespo-peremarch_etal20]
#' blah", "blah blah [@white_etal13] blah", "blah blah [@ellsworth_reich93]
#' blah", "nope (nope not this one) here")))

wrap_lines <- function(x) {
  # TODO: wrap only if references inside
  # Wrap lines with parenthesis
  x <- gsub("\\[\\]\\{#anchor-\\d+\\}", "", x, perl = TRUE)
  gsub("\\n(?=[^()*])", " ", x, perl = TRUE)
}

convert_reference <- function(x, tokens = c("\\(?[-\\w]+ et al. \\d{4}\\)?;?", "\\(?[-\\w]+ and [-\\w]+ \\d{4}\\)?;?", "\\(?[-\\w]+ \\d{4}\\)?;?"), wrap = FALSE) {
  if (wrap) {
    x <- wrap_lines(x)
  }
  for (token in tokens) {
    x <- convert_token(x, token)
  }
  if (wrap) {
    x <- stringr::str_wrap(x, width = 80, indent = 0, exdent = 0)
  }
  return(x)
}

ref_parser <- function(x) {
  x %>% 
    # lowercase
    tolower() %>% 
    # remove first year digits
    gsub("\\b\\d\\d(\\d\\d)\\b", "\\1", .) %>% 
    # underscore before etal
    gsub("\\bet al.", "_etal", .) %>% 
    gsub("\\band\\b", "_", .) %>% 
    # remove remaining whitespace
    gsub("\\s", "", .) %>% 
    # add brackets
    gsub("\\(", "[", .) %>% 
    gsub("\\)", "]", .) %>% 
    # prefix tag with @
    sub("(\\w)", "\\@\\1", .)
}

convert_token <- function(content, token, parser = ref_parser) {
  hint <- gregexpr(token, content, perl = TRUE)
  refs <- regmatches(content, hint)
  replace(content, hint, parser)
}

replace <- function(x, y, .f) {
  regmatches(x, y) <- lapply(regmatches(x, y), .f)
  return(x)
}

addin_convert_reference_selection <- function() {
  # Gets The active Document
  ctx <- rstudioapi::getActiveDocumentContext()
  
  # Checks that a document is active
  if (!is.null(ctx)) {
    
    # Extracts selection as a string
    selected_text <- ctx$selection[[1]]$text
    
    # modify string
    selected_text <- convert_reference(selected_text)
    
    # replaces selection with string
    rstudioapi::modifyRange(ctx$selection[[1]]$range, selected_text)
  }
}

addin_convert_reference_selection_wrap <- function() {
  # Gets The active Document
  ctx <- rstudioapi::getActiveDocumentContext()
  
  # Checks that a document is active
  if (!is.null(ctx)) {
    
    # Extracts selection as a string
    selected_text <- ctx$selection[[1]]$text
    
    # modify string
    selected_text <- convert_reference(selected_text, wrap = TRUE)
    
    # replaces selection with string
    rstudioapi::modifyRange(ctx$selection[[1]]$range, selected_text)
  }
}
