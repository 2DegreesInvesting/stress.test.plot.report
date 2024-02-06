get_path_to_root_from_test <- function() {
  wd_parts <- unlist(strsplit(getwd(), "/"))
  is_test_folder <-
    length(wd_parts) >= 3 &&
      all(tail(wd_parts, 2) == c("tests", "testthat"))
  if (is_test_folder) {
    return(file.path(getwd(), "..", ".."))
  } else {
    return(getwd())
  }
}