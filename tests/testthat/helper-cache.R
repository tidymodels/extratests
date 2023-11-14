# tools for caching results within testthat.

is_object_available <- function(object, fail = FALSE, save_path = "saved_objects") {
  cl <- match.call()
  file_name <- paste0(cl$object, ".RData")
  file_path <- file.path(save_path, file_name)
  has_file <- file.exists(file_path)
  if (fail && !has_file) {
    msg <- paste0("File '", file_name, "' is not in '", save_path, "'.")
    cli::cli_abort(msg)
  }
  has_file
}

save_object <- function(object, save_path = "saved_objects") {
  cl <- match.call()
  file_name <- paste0(cl$object, ".RData")
  file_path <- file.path(save_path, file_name)
  res <- try(save(object, file = file_path), silent = TRUE)
  # returned NULL if it worked
  if (is.null(res)) {
    # verify
    res <- file.exists(file_path)
  } else {
    # save failed
    print(as.character(res))
    res <- FALSE
  }
  res
}

return_object <- function(object, save_path = "saved_objects") {
  cl <- match.call()
  file_name <- paste0(cl$object, ".RData")
  file_path <- file.path(save_path, file_name)
  load(file_path)
  object
}

purge_objects <- function(save_path = "saved_objects") {
  all_files <- list.files(save_path, pattern = "RData$", full.names = TRUE)
  res <- vapply(all_files, unlink, integer(1))
  df_res <- tibble::tibble(file = names(res))
  df_res$deleted <- ifelse(res == 0, TRUE, FALSE)
  invisible(df_res)
}

# Example usage
if (FALSE) {
  pkg <- "tune"
  is_object_available(pkg)

  save_object(pkg)
  is_object_available(pkg)

  rm(pkg)
  pkg <- return_object(pkg)
  pkg

  file_86 <- purge_objects()
  file_86
  is_object_available(pkg)

  is_object_available(some_other_pkg, fail = TRUE)
}
