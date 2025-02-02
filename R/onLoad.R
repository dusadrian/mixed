`.onLoad` <- function(...) {
    load_library <- function(pkg) {
        if (pkg %in% loadedNamespaces()) {
            loc <- dirname(getNamespaceInfo(pkg, "path"))
            do.call(
                "library",
                list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
            )
        }
    }

    suppressPackageStartupMessages(
        load_library("stats")
    )

    if (admisc::unlockEnvironment(asNamespace("base"))) {

        env <- as.environment("package:base")
        do.call("unlockBinding", list(sym = "print.data.frame", env = env))
        
        # this function is unchanged, but it needs to be re-written to access
        # the custom version of format.data.frame()
        env$`print.data.frame` <- function (x, ..., digits = NULL, quote = FALSE, 
            right = TRUE, row.names = TRUE, max = NULL) {
            n <- length(row.names(x))
            if (length(x) == 0L) {
                do.call("cat", list(
                    sprintf(ngettext(n, "data frame with 0 columns and %d row", 
                    "data frame with 0 columns and %d rows"), n),
                    "\n", 
                    sep = "")
                )
            }
            else if (n == 0L) {
                print.default(names(x), quote = FALSE)
                do.call("cat", list(
                    gettext("<0 rows> (or 0-length row.names)\n")
                    )
                )
            }
            else {
                if (is.null(max)) 
                    max <- getOption("max.print", 99999L)
                if (!is.finite(max)) 
                    stop("invalid 'max' / getOption(\"max.print\"): ", 
                        max)
                omit <- (n0 <- max%/%length(x)) < n
                m <- as.matrix(format.data.frame(if (omit) 
                    x[seq_len(n0), , drop = FALSE]
                else x, digits = digits, na.encode = FALSE))
                if (!isTRUE(row.names)) 
                    dimnames(m)[[1L]] <- if (isFALSE(row.names)) 
                        rep.int("", if (omit) 
                        n0
                        else n)
                    else row.names
                do.call("print", list(m, ..., quote = quote, right = right, max = max))
                if (omit) 
                    do.call("cat", list(
                        " [ reached 'max' / getOption(\"max.print\") -- omitted", 
                        n - n0, "rows ]\n"
                        )
                    )
            }
            invisible(x)
        }

        do.call("unlockBinding", list(sym = "format.data.frame", env = env))

        env$`format.data.frame` <- function (x, ..., justify = "none")
        {
            nc <- length(x)
            if (!nc) 
                return(x)
            nr <- .row_names_info(x, 2L)
            rval <- vector("list", nc)
            
            # -----------------------------------------------------
            # this function is also unchanged, except for this part:
            for (i in seq_len(nc)) {
                if (is_mixed(x[[i]]) && any(is.na(x[[i]]))) {
                    # any(is.na()) is necessary to guard against na.omit(), for instance
                    rval[[i]] <- format_mixed(x[[i]])
                }
                else {
                    rval[[i]] <- format(x[[i]], ..., justify = justify)
                }
            }
            # -----------------------------------------------------

            lens <- vapply(rval, NROW, 1)
            
            if (any(lens != nr)) {
                warning("corrupt data frame: columns will be truncated or padded with NAs")
                for (i in seq_len(nc)) {
                    len <- NROW(rval[[i]])
                    if (len == nr) 
                        next
                    if (length(dim(rval[[i]])) == 2L) {
                        rval[[i]] <- if (len < nr) 
                        rbind(rval[[i]], matrix(NA, nr - len, ncol(rval[[i]])))
                        else rval[[i]][seq_len(nr), ]
                    }
                    else {
                        rval[[i]] <- if (len < nr) 
                        c(rval[[i]], rep.int(NA, nr - len))
                        else rval[[i]][seq_len(nr)]
                    }
                }
            }

            for (i in seq_len(nc)) {
                if (is.character(rval[[i]]) && inherits(rval[[i]], "character")) 
                    oldClass(rval[[i]]) <- "AsIs"
            }

            y <- as.data.frame.list(rval, row.names = seq_len(nr), col.names = names(x), 
                optional = TRUE, fix.empty.names = FALSE, cut.names = TRUE)
            
            attr(y, "row.names") <- row.names(x)

            return(y)
        }

        do.call("unlockBinding", list(sym = "c", env = env))
        
        env$c <- function(..., recursive = FALSE, use.names = TRUE) {
            dots <- list(...)
            any_mixed <- FALSE
            
            if (all(vapply(dots, is.atomic, logical(1L)))) {
                any_mixed <- any(vapply(dots, is_mixed, logical(1L)))
            }

            if (any_mixed) {
                return(c_mixed(dots))
            }
            else {
                do.call(.Primitive("c"), c(dots, list(recursive = recursive, use.names = use.names)))
            }
        }

        do.call("unlockBinding", list(sym = "order", env = env))
        
        env$order <- function (..., na.last = TRUE, decreasing = FALSE,
            method = c("auto", "shell", "radix")) {
            
            z <- list(...)
            decreasing <- as.logical(decreasing)
            
            if (length(z) == 1L && is.numeric(x <- z[[1L]]) && !is.object(x) && length(x) > 0) {
                if (eval(parse(text = ".Internal(sorted_fpass(x, decreasing, na.last))"))) {
                    return(seq_along(x))
                }
            }
            
            method <- match.arg(method)
            
            if (any(vapply(z, function(x) {
                    is.object(x) && !is_mixed(x)
                }, logical(1L)))) {
                z <- lapply(z, function(x) if (is.object(x)) 
                    as.vector(xtfrm(x))
                else x)
                return(do.call("order", c(z, list(na.last = na.last, 
                    decreasing = decreasing, method = method))))
            }
            
            if (method == "auto") {
                useRadix <- all(vapply(z, function(x) {
                    (is.numeric(x) || is.factor(x) || is.logical(x)) && is.integer(length(x))
                }, logical(1L)))
                method <- ifelse (useRadix, "radix", "shell")
            }

            if (length(z) == 1L && is_mixed(x)) {
                return(order_mixed(x, na.last = na.last, decreasing = decreasing, method = method, na_values.last = na.last))
            }

            if (method != "radix" && !is.na(na.last)) {
                return(eval(parse(text = ".Internal(order(na.last, decreasing, ...))")))
            }

            if (method == "radix") {
                decreasing <- rep_len(as.logical(decreasing), length(z))
                return(eval(parse(text = ".Internal(radixsort(na.last, decreasing, FALSE, TRUE, ...))")))
            }

            if (any(diff((l.z <- lengths(z)) != 0L))) {
                stop("argument lengths differ")
            }

            na <- vapply(z, is.na, rep.int(NA, l.z[1L]))

            ok <- if (is.matrix(na)) {
                rowSums(na) == 0L
            }
            else {
                !any(na)
            }

            if (all(!ok)) {
                return(integer())
            }

            z[[1L]][!ok] <- NA

            ans <- do.call("order", c(z, list(decreasing = decreasing)))
        
            ans[ok[ans]]
        }

    }

    if (admisc::unlockEnvironment(asNamespace("stats"))) {

        env <- as.environment("package:stats")

        do.call("unlockBinding", list(sym = "sd", env = env))
        
        env$sd <- function(x, na.rm = FALSE) {
            if (is_mixed(x)) {
                na_index <- attr(x, "na_index")
                if (!is.null(na_index)) {
                    x <- x[-na_index]
                }
                class(x) <- setdiff(class(x), "mixed_labelled")
            }

            sqrt(var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm = na.rm))
        }

        do.call("unlockBinding", list(sym = "var", env = env))
        
        env$var <- function(x, y = NULL, na.rm = FALSE, use) {
            if (is_mixed(x)) {
                na_index <- attr(x, "na_index")
                if (!is.null(na_index)) {
                    x <- x[-na_index]
                }
                class(x) <- setdiff(class(x), "mixed_labelled")
            }

            if (missing(use)) {
                use <- ifelse(na.rm, "na.or.complete", "everything")
            }

            na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", 
                "everything", "na.or.complete"))

            if (is.na(na.method)) {
                stop("invalid 'use' argument")
            }

            if (is.data.frame(x)) {
                x <- as.matrix(x)
            }
            else {
                stopifnot(is.atomic(x))
            }

            if (is.data.frame(y)) {
                y <- as.matrix(y)
            }
            else {
                stopifnot(is.atomic(y))
            }
            
            eval(parse(text = '.Call(stats:::C_cov, x, y, na.method, FALSE)'))
            # .Call(C_cov, x, y, na.method, FALSE)
        }

        do.call("unlockBinding", list(sym = "fivenum", env = env))
        
        env$fivenum <- function(x, na.rm = FALSE) {
            if (is_mixed(x)) {
                na_index <- attr(x, "na_index")
                if (!is.null(na_index)) {
                    x <- x[-na_index]
                }
                class(x) <- setdiff(class(x), "mixed_labelled")
            }

            xna <- is.na(x)
            if (any(xna)) {
                if (na.rm) 
                    x <- x[!xna]
                else return(rep.int(NA, 5))
            }
            x <- sort(x)
            n <- length(x)
            if (n == 0) 
                rep.int(NA, 5)
            else {
                n4 <- floor((n + 3)/2)/2
                d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
                0.5 * (x[floor(d)] + x[ceiling(d)])
            }
        }
    }

}
