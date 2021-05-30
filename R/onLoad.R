`.onLoad` <- function(...) {

    if (unlockEnvironment(asNamespace("base"))) {

        env <- as.environment("package:base")
        do.call("unlockBinding", list(sym = "print.data.frame", env = env))
        
        env$`print.data.frame` <- function (x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {
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

        env$`format.data.frame` <- function (x, ..., justify = "none") {
            nc <- length(x)
            if (!nc) 
                return(x)
            nr <- .row_names_info(x, 2L)
            rval <- vector("list", nc)
            
            for (i in seq_len(nc)) {
                if (is_mixed(x[[i]])) {
                    rval[[i]] <- format_mixed(x[[i]])
                }
                else {
                    rval[[i]] <- format(x[[i]], ..., justify = justify)
                }
            }

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
            
            if (all(unlist(lapply(dots, is.atomic)))) {
                any_mixed <- any(unlist(lapply(dots, is_mixed)))
            }

            if (any_mixed) {
                return(c_mixed_labelled(dots))
            }
            else {
                do.call(.Primitive("c"), c(dots, list(recursive = recursive, use.names = use.names)))
            }
        }
        
    }
    


}
