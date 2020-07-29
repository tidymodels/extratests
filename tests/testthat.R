library(testthat)
library(extratests)

## -----------------------------------------------------------------------------

spark_install_winutils <- function(version) {
  hadoop_version <- if (version < "2.0.0") "2.6" else "2.7"
  spark_dir <- paste("spark-", version, "-bin-hadoop", hadoop_version, sep = "")
  winutils_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "spark", spark_dir, "tmp", "hadoop", "bin", fsep = "\\")

  if (!dir.exists(winutils_dir)) {
    message("Installing winutils...")

    dir.create(winutils_dir, recursive = TRUE)
    winutils_path <- file.path(winutils_dir, "winutils.exe", fsep = "\\")

    download.file(
      "https://github.com/steveloughran/winutils/raw/master/hadoop-2.6.0/bin/winutils.exe",
      winutils_path,
      mode = "wb"
    )

    message("Installed winutils in ", winutils_path)
  }
}

## -----------------------------------------------------------------------------

library(sparklyr)

if (.Platform$OS.type == "windows") {
  # Right now, this does not seem to be working on windows; working on fixing it.
  # Leaving it as-is as a place to start.
  spark_install_winutils("2.4")
  sparklyr::spark_install(verbose = TRUE, version = "2.4", hadoop_version = "2.7")
} else {
  sparklyr::spark_install(verbose = TRUE, version = "2.4")
}

sc <- try(sparklyr::spark_connect(master = "local"), silent = TRUE)

if(inherits(sc, "try-error")) {
  print(sc)
}

## -----------------------------------------------------------------------------

test_check("extratests", reporter = "summary")
