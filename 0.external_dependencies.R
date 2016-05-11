check.cran.pkg <- function(name, force = FALSE) {
  if (force || !(name %in% rownames(installed.packages())))
    install.packages(name)
}

install.from.github <- function(user, pkg, subdir = NULL) {
  check.cran.pkg("devtools")
  library(devtools)
  check.cran.pkg("httr")
  library(httr)
  
  #set_config(config(ssl.verifypeer = 0L))
  set_config(config(ssl_verifypeer = 0L))
  install_github(paste(user, pkg, sep = "/"), subdir = subdir, dependencies = TRUE, args = "--no-multiarch")
}

install.from.folder <- function(path) {
  check.cran.pkg("devtools")
  library(devtools)
  
  install_deps(path)
  install.packages(path, repos = NULL, type = "source", INSTALL_opts = "--no-multiarch")
}

check.tsdist.pkg <- function(force = FALSE) {
  # install the TSdist package from source
  if (force || !("TSdist" %in% rownames(installed.packages()))) {
    check.cran.pkg("proxy")
    library(proxy)
    
    dynam.index <- which(lapply(library.dynam(), function(x) x[["name"]]) %in% "TSdist")
    if (length(dynam.index) != 0)
      try(library.dynam.unload("TSdist", dirname(dirname(dirname(library.dynam()[[dynam.index]][["path"]])))), silent = TRUE)
    if (is.element("TSdist", .packages()))
      try(detach("package:TSdist", unload=TRUE), silent = TRUE)
    if (pr_DB$entry_exists("tsDistances"))
      pr_DB$delete_entry("tsDistances")
    install.from.github("Kevin-Jin", "TSdist")
    
    #install.from.folder("S:\\K_Jin\\Git\\TSdist")
  }
}
