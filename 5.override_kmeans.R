assign.in.namespace <- function(x, value, ns, pos = -1, envir = as.environment(pos)) {
  if (missing(ns)) {
    nm <- attr(envir, "name", exact = TRUE)
    if (is.null(nm) || substring(nm, 1L, 8L) != "package:") 
      stop("environment specified is not a package")
    ns <- asNamespace(substring(nm, 9L))
  }
  else ns <- asNamespace(ns)
  ns_name <- getNamespaceName(ns)
  if (bindingIsLocked(x, ns)) {
    in_load <- Sys.getenv("_R_NS_LOAD_")
    if (nzchar(in_load)) {
      if (in_load != ns_name) {
        msg <- gettextf("changing locked binding for %s in %s whilst loading %s", 
                        sQuote(x), sQuote(ns_name), sQuote(in_load))
        if (!in_load %in% c("Matrix", "SparseM")) 
          warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
      }
    }
    else if (nzchar(Sys.getenv("_R_WARN_ON_LOCKED_BINDINGS_"))) {
      warning(gettextf("changing locked binding for %s in %s", 
                       sQuote(x), sQuote(ns_name)), call. = FALSE, domain = NA, 
              immediate. = TRUE)
    }
    unlockBinding(x, ns)
    assign(x, value, envir = ns, inherits = FALSE)
    w <- options("warn")
    on.exit(options(w))
    options(warn = -1)
    lockBinding(x, ns)
  }
  else {
    assign(x, value, envir = ns, inherits = FALSE)
  }
  if (!isBaseNamespace(ns)) {
    S3 <- .getNamespaceInfo(ns, "S3methods")
    if (!length(S3)) 
      return(invisible(NULL))
    S3names <- S3[, 3L]
    if (x %in% S3names) {
      i <- match(x, S3names)
      genfun <- get(S3[i, 1L], mode = "function", envir = parent.frame())
      if (.isMethodsDispatchOn() && methods::is(genfun, 
                                                "genericFunction")) 
        genfun <- methods::slot(genfun, "default")@methods$ANY
      defenv <- if (typeof(genfun) == "closure") 
        environment(genfun)
      else .BaseNamespaceEnv
      S3Table <- get(".__S3MethodsTable__.", envir = defenv)
      remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
      if (exists(remappedName, envir = S3Table, inherits = FALSE)) 
        assign(remappedName, value, S3Table)
    }
  }
  invisible(NULL)
}

topological.sort <- function(adj.list, indegrees) {
  output <- NULL
  
  # Kahn's algorithm.
  indegree.zero <- names(indegrees)[indegrees == 0]
  while (length(indegree.zero)) {
    # Pop.
    node <- tail(indegree.zero, 1)
    indegree.zero <- head(indegree.zero, -1)
    # Visit.
    output <- c(output, node)
    # Push.
    for (neighbor in adj.list[[node]]) {
      indegrees[neighbor] <- indegrees[neighbor] - 1
      if (indegrees[neighbor] == 0)
        indegree.zero <- c(indegree.zero, neighbor)
    }
  }
  
  output
}

errors.to.warnings <- function(to.wrap)
  function(...)
    tryCatch(to.wrap(...), error = function(e) warning(e))

reload.package <- function(pkg, reload.self) {
  if (isNamespaceLoaded(pkg)) {
    to.visit <- pkg
    reload.deps <- list()
    indegrees <- NULL
    while (length(to.visit)) {
      # Pop.
      dependency <- tail(to.visit, 1)
      to.visit <- head(to.visit, -1)
      # Visit.
      users <- getNamespaceUsers(dependency)
      reload.deps[users] <- lapply(reload.deps[users], function(x) c(x, dependency))
      indegrees[dependency] <- length(users)
      # Push
      to.visit <- c(to.visit, users)
    }
    
    reload.deps <- topological.sort(reload.deps, indegrees)
    if (!reload.self)
      # Don't reload ourself or else we lose the overwritten implementation.
      reload.deps <- reload.deps[reload.deps != pkg]
    lapply(reload.deps, errors.to.warnings(unloadNamespace))
    lapply(rev(reload.deps), errors.to.warnings(attachNamespace))
  }
  invisible(NULL)
}

reassign.in.package <- function(name, value, pkg) {
  propagate.change <- function(name, value, pkg) {
    # Overwrites implementation when dependent packages
    # load the function e.g. referencing "stats::kmeans".
    env <- asNamespace(pkg)
    unlockBinding(name, env)
    assign.in.namespace(name, value, pkg, env)
    # Overwrites implementation available in global scope
    # e.g. just referencing "kmeans'.
    env <- as.environment(paste("package", pkg, sep = ":"))
    unlockBinding(name, env)
    assign(name, value, env)
    
    # Make sure dependencies see the updated implementation.
    reload.package(pkg, FALSE)
  }
  
  super <- getFromNamespace(name, pkg)
  attr(value, "super") <- super
  attr(value, "revert") <- function() propagate.change(name, super, pkg)
  
  propagate.change(name, value, pkg)
  
  super
}

reassign.in.package("kmeans", function(x, centers, ...) {
  # Check if we specify the initial cluster centers by averaging
  # an a priori guess of the cluster memberships.
  if (!is.null(attr(centers, "kmeans.pass.through")))
    centers <- attr(centers, "kmeans.pass.through")
  if (is.list(centers) && !is.data.frame(centers))
    # Unlike the rest of R, kmeans() is row-major for some reason.
    centers <- do.call(rbind, lapply(centers, function(cluster.def) colMeans(x[cluster.def, ])))
  
  attr(kmeans, "super")(x, centers, ...)
}, "stats")
