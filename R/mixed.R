# internal
`all_missing_values` <- function(x, na_values = NULL, na_range = NULL, labels = NULL) {

    ##########
    # Arguments na_values, na_range and labels can either be provided by the user or,
    # if the input is a haven_labelled(_spss) objects they might already be in the attributes
    if (is.null(na_values)) {
        na_values <- attr(x, "na_values")
    }

    if (is.null(na_range)) {
        na_range <- attr(x, "na_range")
    }

    if (is.null(labels)) {
        labels <- attr(x, "labels", exact = TRUE)
    }
    ##########


    misvals <- c()

    if (is.null(na_values) & is.null(na_range)) {
        return(misvals)
    }

    if (!is.null(na_values)) {
        misvals <- sort(na_values)
    }

    if (is.numeric(x)) {
        if (!is.null(labels)) {
            x <- c(x, unname(unclass(labels)))
        }

        if (!is.null(na_range)) {
            uniques <- sort(unique(x[x >= na_range[1] & x <= na_range[2]]))
            if (length(uniques) == 0) {
                uniques <- na_range
            }
            else {
                uniques <- sort(unique(c(uniques, na_range)))
            }

            misvals <- sort(unique(c(misvals, uniques)))
        }
    }

    return(misvals)
}

# to be added in the namespace
`as_mixed` <- function(x, ...) {
    UseMethod("as_mixed")
}

# to be added in the namespace
`is_mixed` <- function(x) {
    inherits(x, "mixed_labelled")
}

# to be added in the namespace
`as_mixed.default` <- function(x, ...) {
    return(x)
}

# to be added in the namespace
`as_mixed.haven_labelled` <- function(x, ...) {
    if (is_mixed(x) || !is.atomic(x)) {
        return(x)
    }

    misvals <- all_missing_values(x)

    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    labels <- attr(x, "labels", exact = TRUE)
    label <- attr(x, "label", exact = TRUE)

    attributes(x) <- NULL
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    return(x)
}

# to be added in the namespace
`as_mixed.data.frame` <- function(x, ..., only_labelled = TRUE) {
    if (only_labelled) {
        labelled <- unlist(lapply(x, function(x) {
            inherits(x, "haven_labelled")
        }))
        x[labelled] <- lapply(x[labelled], as_mixed, ...)
    } else {
        x[] <- lapply(x, as_mixed, ...)
    }

    return(x)
}

# to be added in the namespace
`unmix` <- function(x) {
    UseMethod("unmix")
}

# to be added in the namespace
`unmix.default` <- function(x) {
    return(x)
}

# to be added in the namespace
`unmix.mixed_labelled` <- function(x) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    
    # this is necessary to replace those values
    # (because of the "[<-.mixed_labelled" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
    if (!is.null(na_index)) {
        # x <- ifelse(!is.na(missingValues), missingValues, x)
        x[na_index] <- likely_mode(names(na_index))
    }
    
    attrx$na_values <- NULL
    attrx$na_range <- NULL
    attrx$na_index <- NULL

    attributes(x) <- attrx
    return(x)
}

# to be added in the namespace
`unmix.data.frame` <- function(x) {
    mixed <- vapply(x, is_mixed, logical(1))
    x[mixed] <- lapply(x[mixed], unmix)
    
    return(x)
}

# no export
`validate_labelled` <- function(x = double(), labels = NULL, label = NULL,
                                na_values = NULL, na_range = NULL, ...) {
    
    if (!is.numeric(x) && !is.character(x)) {
        cat("\n")
        stop(simpleError("`x` must be a numeric or a character vector.\n\n"))
    }
    
    if (!is.null(labels)) {
        if (is.null(names(labels))) {
            stop(simpleError("`labels` must have names."))
        }

        if (any(duplicated(stats::na.omit(labels)))) {
            stop(simpleError("`labels` must be unique."))
        }
    }

    if (!is.null(label) && (!is.atomic(label) || !is.character(label) || length(label) != 1)) {
        cat("\n")
        stop(simpleError("`label` must be a character vector of length one.\n\n"))
    }
    
    if (!is.null(na_values)) {
        if (any(is.na(na_values))) {
            cat("\n")
            stop(simpleError("`na_values` should not contain NA values.\n\n"))
        }
    }

    if (!is.null(na_range)) {
        type_ok <- (is.character(x) && is.character(na_range)) || (is.numeric(x) && is.numeric(na_range))
        
        if (!type_ok || length(na_range) != 2) {
            cat("\n")
            stop(simpleError("`na_range` must be a vector of length two of the same type as `x`.\n\n"))
        }
        
        if (any(is.na(na_range))) {
            cat("\n")
            stop(simpleError("`na_range` can not contain missing values.\n\n"))
        }
        
        if (na_range[1] >= na_range[2]) {
            cat("\n")
            stop(simpleError("`na_range` must be in ascending order.\n\n"))
        }
    }
}


# to be added in the namespace
`mixed_labelled` <- function(x = double(), labels = NULL, na_values = NULL,
                          na_range = NULL, label = NULL, ...) {
    if (is_mixed(x)) {
        return(x)
    }

    if (inherits(x, "haven_labelled")) {
        return(as_mixed(x))
    }

    attributes(x) <- NULL
    validate_labelled(x, labels, label, na_values, na_range)

    misvals <- all_missing_values(x, na_values, na_range, labels)
    
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels

    return(x)

}


`missingValues` <- function(x) {
    
    mv <- rep(NA, length(x))
    
    if (is_mixed(x)) {
        misvals <- attr(x, "na_index")
        mv[as.numeric(names(misvals))] <- misvals
    }

    return(mv)
}


`missingValues<-` <- function(x, value) {
    
    class(x) <- setdiff(class(x), "mixed_labelled")
    other_classes <- setdiff(class(x), c("integer", "double", "character", "numeric", "complex", "haven_labelled", "haven_labelled_spss", "vctrs_vctr"))
    pn <- admisc::possibleNumeric(x)
    x[!is.na(value)] <- NA
    
    
    na_index <- which(!is.na(value))
    value <- value[!is.na(value)]

    if (pn || all(is.na(x))) {
        x <- admisc::asNumeric(x)
        class(x) <- ifelse(admisc::wholeNumeric(x), "integer", "double")
    }
    else {
        class(x) <- "character"
    }

    names(na_index) <- value
    
    attr(x, "na_index") <- na_index
    # print(attributes(x))
    structure(x, class = c("mixed_labelled", other_classes, class(x)))
}

`likely_mode` <- function(x) {
    if (admisc::possibleNumeric(x)) {
        x <- admisc::asNumeric(x)
        if (admisc::wholeNumeric(x)) {
            x <- as.integer(x)
        }
    }
    return(x)
}


`[.mixed_labelled` <- function(x, i, ...) {
    
    missings <- rep(NA, length(x))
    na_index <- attr(x, "na_index")

    missings[na_index] <- likely_mode(names(na_index))
    
    x <- NextMethod()
    missings <- missings[i]
    missingValues(x) <- missings
    # print(attributes(x))
    x
}



`[<-.mixed_labelled` <- function(x, i, value) {
    missings <- rep(NA, length(x))
    class(x) <- setdiff(class(x), "mixed_labelled")
    x[i] <- value
    missings[i] <- missingValues(value)

    missingValues(x) <- missings
    # print(attributes(x))
    x
}



`format_mixed` <- function(x, digits = getOption("digits")) {
    if (!is.atomic(x)) {
        cat("\n")
        stop("`x` has to be a vector.\n\n", call. = FALSE)
    }
    
    out <- format(unclass(x), digits = digits)
    na_index <- attr(x, "na_index")
    # return(na_index)
    out[na_index] <- paste0("NA(", names(na_index), ")")

    # format again to make sure all elements have same width
    return(format(out, justify = "right"))
}


`print.mixed_labelled` <- function(x, ...) {
    print(noquote(format_mixed(x)), ...)
}


`order_mixed` <- function(x, na.last = NA, decreasing = FALSE, method = c("auto", "shell", "radix"),
    na_values.last = NA) {
        
    method <- match.arg(method)
    
    ix <- seq_along(x)

    na_index <- attr(x, "na_index")
    declared <- logical(length(x))
    declared[na_index] <- TRUE
    truena <- ix[is.na(x) & !declared]
    # return(truena)    
    ideclared <- c()

    if (any(declared)) {
        x <- unclass(unmix(x))
        # return(na_index)
        # return(ix[na_index])
        ideclared <- unname(na_index[order(names(na_index), decreasing = decreasing, method = method)])
        # return(ideclared)
    }

    attributes(x) <- NULL # just in case
    ix <- ix[!(is.na(x) | declared)]
    x <- x[!(is.na(x) | declared)]

    res <- c()
    if (isFALSE(na.last)) {
        res <- truena
    }

    if (isFALSE(na_values.last)) {
        res <- c(res, ideclared)
    }

    res <- c(res, ix[order(unclass(x), decreasing = decreasing, method = method)])
    # return(ideclared)
    
    if (isTRUE(na_values.last)) {
        res <- c(res, ideclared)
    }
    
    if (isTRUE(na.last)) {
        res <- c(res, truena)
    }

    return(res)
}



`==.mixed_labelled` <- function(e1, e2) {
    return(unclass(unmix(e1)) == unclass(unmix(e2)))
}

`!=.mixed_labelled` <- function(e1, e2) {
    return(unclass(unmix(e1)) != unclass(unmix(e2)))
}

`<=.mixed_labelled` <- function(e1, e2) {
    return(unclass(unmix(e1)) <= unclass(unmix(e2)))
}

`<.mixed_labelled` <- function(e1, e2) {
    return(unclass(unmix(e1)) < unclass(unmix(e2)))
}

`>=.mixed_labelled` <- function(e1, e2) {
    return(unclass(unmix(e1)) >= unclass(unmix(e2)))
}

`>.mixed_labelled` <- function(e1, e2) {
    return(unclass(unmix(e1)) > unclass(unmix(e2)))
}

`names<-.mixed_labelled` <- function(x, value) {
    attr(x, "names") <- value
    x
}



`c_mixed_labelled` <- function(dots, recursive = FALSE, use.names = TRUE) {
    # dots <- list(...)
    mixed <- unlist(lapply(dots, is_mixed))
    na_values <- sort(unique(unlist(lapply(dots, function(x) attr(x, "na_values")))))
    
    labels <- unlist(lapply(dots, function(x) {
        attr(x, "labels", exact = TRUE)
    }))
    
    duplicates <- duplicated(labels)

    if (length(wduplicates <- which(duplicates)) > 0) {
        for (i in seq(length(wduplicates))) {
            if (length(unique(names(labels[labels == labels[wduplicates[i]]]))) > 1) {
                cat("\n")
                stop(simpleError("Labels must be unique.\n\n"))
            }
        }
    }

    labels <- sort(labels[!duplicates])

    na_range <- lapply(dots, function(x) attr(x, "na_range", exact = TRUE))
    nulls <- unlist(lapply(na_range, is.null))
    
    if (all(nulls)) {
        na_range <- NULL
    }
    else {
        if (sum(nulls) == length(na_range) - 1) {
            na_range <- unlist(na_range)
        }
        else {
            compatible <- logical(length(na_range))
            if (!is.null(na_range)) {
                for (i in seq(1, length(na_range) - 1)) {
                    nai <- na_range[[i]]
                    if (is.null(nai)) {
                        compatible[i] <- TRUE
                    }
                    else {
                        for (j in seq(2, length(na_range))) {
                            naj <- na_range[[j]]
                            if (is.null(naj)) {
                                compatible[j] <- TRUE
                            }
                            else {
                                if (any(is.element(seq(nai[1], nai[2]), seq(naj[1], naj[2]))) > 0) {
                                    compatible[i] <- TRUE
                                    compatible[j] <- TRUE
                                }
                            }
                        }
                    }
                }
            }

            if (any(!compatible)) {
                cat("\n")
                stop(simpleError("Incompatible NA ranges.\n\n"))
            }

            na_range <- range(unlist(na_range))
        }
    }

    dots <- unlist(lapply(dots, function(x) {
        if (is_mixed(x)) x <- unmix(x)
        attributes(x) <- NULL
        return(x)
    }))

    mixed_labelled(
        dots,
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = attr(dots[[which(mixed)[1]]], "label", exact = TRUE)
    )
}
