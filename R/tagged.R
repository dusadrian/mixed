`tag` <- function(...) {
    x <- as.character(c(...))
    ln <- logical(length(x))
    numbers <- unlist(lapply(x, possibleNumeric))
    if (any(numbers)) {
        ln[numbers] <- abs(asNumeric(x[numbers])) > 32767
    }
    
    
    if (any(ln)) {
        large_numbers <- as.numeric(sort(unique(x[ln])))
        
        if (length(large_numbers) < length(letters)) {
            names(large_numbers) <- paste0("_", letters[seq_along(large_numbers)])
            x[ln] <- names(large_numbers)[match(x[ln], large_numbers)]
        }
        else {
            cat("\n")
            stop("Too many large numbers.\n\n", call. = FALSE)
        }
    }
    
    x <- .Call("_tag", x, PACKAGE = "mixed")
    class(x) <- c("tagged", "vctrs_vctr", "double")

    if (any(ln)) {
        attr(x, "large_numbers") <- large_numbers
    }
    return(x)
}

`has_tag` <- function(x, tag = NULL) {
    if (!is.atomic(x)) {
        return(FALSE)
    }

    if (is.character(x)) {
        return(startsWith(x, "NA(") & endsWith(x, ")"))
    }
    else if (!is.double(x)) {
        return(logical(length(x)))
    }
    
    if (!is.null(tag)) {
        if (!is.atomic(tag) && (length(tag) > 1 || is.na(tag))) {
            cat("\n")
            stop("`tag` should be a vector of length 1.\n\n", call. = FALSE)
        }

        large_numbers <- attr(x, "large_numbers")
        if (!is.null(large_numbers)) {
            if (is.element(tag, large_numbers)) {
                tag <- names(large_numbers[large_numbers == tag])
            }
        }

        tag <- as.character(tag)
    }
    
    if (possibleNumeric(tag) && abs(asNumeric(tag)) > 32767) {
        return(logical(length(x)))
    }
    
    return(.Call("_has_tag", x, tag, PACKAGE = "mixed"))
}

`get_tag` <- function(x) {
    result <- rep(NA, length(x))

    if (is.character(x)) {
        ts <- is_tagged_string(x)
        if (any(ts)) {
            result[ts] <- gsub("^.*?\\(|)$", "",  x[ts])
        }
    }
    else if (is.double(x)) {
        result <- .Call("_get_tag", x, PACKAGE = "mixed")

        tv <- attr(x, "large_numbers")
        if (!is.null(tv)) {
            nms <- names(tv)
            isel <- is.element(result, nms)
            if (any(isel)) {
                result[isel] <- unname(tv[match(result[isel], nms)])
            }
        }

        forcenumeric <- attr(x, "numeric")
        if (is.null(forcenumeric)) {
            forcenumeric <- FALSE
        }

        if (forcenumeric || !any(is.na(suppressWarnings(as.numeric(na.omit(result)))))) {
            result <- suppressWarnings(as.numeric(result))
        }
    }
    
    return(result)
}

# is a string representing a tagged NA such as "NA(a)"?
`is_tagged_string` <- function(x) {
    if (!is.character(x)) {
        return(logical(length(x)))
    }

    return(startsWith(x, "NA(") & endsWith(x, ")"))
}

`format.tagged` <- function(x, ..., digits = getOption("digits")) {
    format_tagged(x, digits = digits)
}

`format_tagged` <- function(x, digits = getOption("digits")) {
    if (!is.atomic(x)) {
        cat("\n")
        stop("`x` has to be a vector.\n\n", call. = FALSE)
    }
    
    out <- format(unclass(x), digits = digits)
    tagged <- has_tag(x)

    if (any(tagged)) {
        out[tagged] <- paste0("NA(", get_tag(x[tagged]), ")")
    }

    # format again to make sure all elements have same width
    return(format(out, justify = "right"))
}

`print.tagged` <- function(x, ...) {
    print(noquote(format_tagged(x)), ...)
}


`untag` <- function(x, numeric = FALSE) {
    tagged <- has_tag(x)
    mixed <- inherits(x, "mixed_labelled")
    attrx <- attributes(x)

    if (any(tagged)) {
        attr(x, "numeric") <- numeric
        
        tags <- get_tag(x[tagged])
        
        # this is necessary to replace those values
        # (because of the "[<-.mixed_labelled" method)
        attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
        
        x[tagged] <- tags
    }

    if (mixed) {
        attrx$class <- setdiff(attrx$class, "mixed_labelled")
        if (!is.null(attrx$labels)) {
            attrx$labels <- unclass(Recall(attrx$labels, numeric = numeric))
        }
        attrx$na_values <- NULL
        attrx$na_range <- NULL
    }

    attributes(x) <- attrx
    return(x)
}

`order_tagged` <- function(x, na.last = NA, decreasing = FALSE, method = c("auto", "shell", "radix"),
    na_values.last = NA) {
        
    method <- match.arg(method)
    
    ix <- seq_along(x)

    tagged <- has_tag(x)
    truena <- ix[is.na(x) & !tagged]
    itagged <- c()

    if (any(tagged)) {
        x <- unclass(untag(x))
        itagged <- ix[tagged][order(x[tagged], decreasing = decreasing, method = method)]
    }

    attributes(x) <- NULL # just in case
    ix <- ix[!(is.na(x) | tagged)]
    x <- x[!(is.na(x) | tagged)]

    res <- c()
    if (isFALSE(na.last)) {
        res <- truena
    }

    if (isFALSE(na_values.last)) {
        res <- c(res, itagged)
    }
    
    res <- c(res, ix[order(x, decreasing = decreasing, method = method)])
    
    if (isTRUE(na_values.last)) {
        res <- c(res, itagged)
    }
    
    if (isTRUE(na.last)) {
        res <- c(res, truena)
    }

    return(res)
}

`sort_tagged` <- function(x, decreasing = FALSE, na.last = NA,
                           na_values.last = NA, ...) {
    x[order_tagged(
        x,
        decreasing = decreasing,
        na.last = na.last,
        na_values.last = na_values.last
    )]
}
