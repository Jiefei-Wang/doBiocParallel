registerDoBiocParallel <- function(BPPARAM, cores = NULL){
    env <- new.env(parent = emptyenv())
    env$BPPARAM <- BPPARAM
    setDoPar(doBiocParallel, env, bpinfo)

    ## The previous registration may be the same BPPARAM
    if (bpisup(BPPARAM))
        gc()
    ## start the cluster if not
    if (!bpisup(BPPARAM)){
        tryCatch({
            bpstart(BPPARAM)
        },
        error = function(e) {
            registerDoSEQ()
            stop(e)
        })
        reg.finalizer(env, function(e){
            message("Hi")
            if (bpisup(BPPARAM))
                bpstop(BPPARAM)
        })
    }
    invisible()
}

# passed to setDoPar via registerDoBiocParallel, and called by getDoParWorkers, etc
bpinfo <- function(data, item) {
    switch(item,
           workers=bpworkers(data),
           name=paste0('doBiocParallel-', class(data)[1]),
           version=packageDescription('doBiocParallel', fields='Version'),
           NULL)
}

makeExportEnv <- function(obj, expr, envir) {
    # setup the parent environment by first attempting to create an environment
    # that has '...' defined in it with the appropriate values
    exportenv <- tryCatch({
        qargs <- quote(list(...))
        args <- eval(qargs, envir)
        environment(do.call(makeDotsEnv, args))
    },
    error=function(e) {
        new.env(parent=emptyenv())
    })
    noexport <- union(obj$noexport, obj$argnames)
    getexports(expr, exportenv, envir, bad=noexport)

    vars <- ls(exportenv)
    if (obj$verbose) {
        if (length(vars) > 0) {
            cat('automatically exporting the following variables',
                'from the local environment:\n')
            cat(' ', paste(vars, collapse=', '), '\n')
        } else {
            cat('no variables are automatically exported\n')
        }
    }

    # compute list of variables to export
    export <- unique(obj$export)
    ignore <- intersect(export, vars)
    if (length(ignore) > 0) {
        warning(sprintf('already exporting variable(s): %s',
                        paste(ignore, collapse=', ')))
        export <- setdiff(export, ignore)
    }

    # add explicitly exported variables to exportenv
    if (length(export) > 0) {
        if (obj$verbose)
            cat(sprintf('explicitly exporting variables(s): %s\n',
                        paste(export, collapse=', ')))

        for (sym in export) {
            if (!exists(sym, envir, inherits=TRUE))
                stop(sprintf('unable to find variable "%s"', sym))
            val <- get(sym, envir, inherits=TRUE)
            if (is.function(val) &&
                (identical(environment(val), .GlobalEnv) ||
                 identical(environment(val), envir))) {
                # Changing this function's environment to exportenv allows it to
                # access/execute any other functions defined in exportenv.  This
                # has always been done for auto-exported functions, and not
                # doing so for explicitly exported functions results in
                # functions defined in exportenv that can't call each other.
                environment(val) <- exportenv
            }
            assign(sym, val, pos=exportenv, inherits=FALSE)
        }
    }
    parent.env(exportenv) <- getNamespace("base")
    exportenv
}



doBiocParallel <- function(obj, expr, envir, data) {
    ## TODO: disable auto export
    BPPARAM <- data$BPPARAM

    if (!inherits(obj, 'foreach'))
        stop('obj must be a foreach object')

    exportenv <- makeExportEnv(obj, expr, envir)
    packages <- unique(obj$packages)

    argNames <- names(obj$args)
    args <- lapply(
        argNames,
        function(i) eval(obj$args[[i]])
    )
    names(args) <- argNames

    allArgs <- c(
        list(FUN = evalWrapper),
        args,
        list(
            MoreArgs = list(
                expr = as.expression(expr),
                exportenv = exportenv,
                packages = packages
            ),
            SIMPLIFY = FALSE,
            BPPARAM = BPPARAM
        )
    )
    do.call(what = bpmapply, args = allArgs, quote = FALSE)
}

evalWrapper <- function(..., expr, exportenv, packages) {
    for (i in packages)
        library(i, character.only = TRUE)
    args <- list(...)
    for (i in names(args))
        assign(i, args[[i]], pos=exportenv, inherits=FALSE)
    eval(expr, envir = exportenv)
}
