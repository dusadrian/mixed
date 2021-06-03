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
`as_mixed.haven_labelled_spss` <- function(x, ...) {
    # TO DO: add functionality for tagged NAs in class haven_labelled
    
    misvals <- all_missing_values(unclass(x))

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
        labelled_spss <- unlist(lapply(x, function(x) {
            inherits(x, "haven_labelled_spss")
        }))
        x[labelled_spss] <- lapply(x[labelled_spss], as_mixed, ...)
    } else {
        x[] <- lapply(x, as_mixed, ...)
    }

    return(x)
}

# to be added in the namespace
`unmix` <- function(x, haven = FALSE) {
    UseMethod("unmix")
}

# to be added in the namespace
`unmix.default` <- function(x, haven = FALSE) {
    return(x)
}

# to be added in the namespace
`unmix.mixed_labelled` <- function(x, haven = FALSE) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    
    # this is necessary to replace those values
    # (because of the "[<-.mixed_labelled" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
    if (!is.null(na_index)) {
        # x <- ifelse(!is.na(missingValues), missingValues, x)
        x[na_index] <- likely_mode(names(na_index))
    }
    
    attrx$na_index <- NULL

    if (haven) {
        attrx$class <- c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", setdiff(attrx$class, "mixed_labelled"))
    }
    else {
        attrx$na_values <- NULL
        attrx$na_range <- NULL
    }

    attributes(x) <- attrx
    return(x)
}

# to be added in the namespace
`unmix.data.frame` <- function(x, haven = FALSE) {
    mixed <- vapply(x, is_mixed, logical(1))
    x[mixed] <- lapply(x[mixed], unmix, haven = haven)
    
    return(x)
}

# no export
`validate_mixed` <- function(x = double(), labels = NULL, label = NULL,
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
    }
}


# to be added in the namespace
`mixed_labelled` <- function(x = double(), labels = NULL, na_values = NULL,
                          na_range = NULL, label = NULL, ...) {
    if (inherits(x, "haven_labelled")) {
        return(as_mixed(x))
    }

    attributes(x) <- NULL
    validate_mixed(x, labels, label, na_values, na_range)

    misvals <- all_missing_values(x, na_values, na_range, labels)

    if (!is.null(na_range)) {
        na_range <- sort(na_range)
    }
    
    missingValues(x)[is.element(x, misvals)] <- x[is.element(x, misvals)]

    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels

    return(x)
}

`likely_mode` <- function(x) {
    if (admisc::possibleNumeric(x) || all(is.na(x))) {
        x <- admisc::asNumeric(x)
        if (admisc::wholeNumeric(x)) {
            x <- as.integer(x)
        }
    }

    return(x)
}

`likely_type` <- function(x) {
    type <- NULL
    if (is.numeric(x)) {
        type <- "numeric"
        if (is.integer(x)) {
            type <- "integer"
        }
    }
    else if (is.character(x)) {
        type <- "character"
    }

    if (!is.null(type)) {
        return(paste0("<", type, ">"))
    }
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
    
    x <- likely_mode(x)

    if (any(!is.na(value))) {
        x[!is.na(value)] <- NA
        na_index <- which(!is.na(value))
        value <- value[!is.na(value)]
        names(na_index) <- value
        attr(x, "na_index") <- na_index
    }
    
    structure(x, class = c("mixed_labelled", other_classes, class(x)))
}


`[.mixed_labelled` <- function(x, i, ...) {
    attrx <- attributes(x)
    x <- unmix(x)
    x <- NextMethod()
    # attrx$label, if not existing, takes from attrx$labels
    # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
    mixed_labelled(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}


`[<-.mixed_labelled` <- function(x, i, value) {
    attrx <- attributes(x)
    value <- unmix(value)
    x <- unmix(x)
    x <- NextMethod()
    mixed_labelled(x, attrx[["labels"]], attrx$na_values, attrx$na_range, attrx[["label"]])
}


`format_mixed` <- function(x, digits = getOption("digits")) {
    if (!is.atomic(x)) {
        cat("\n")
        stop("`x` has to be a vector.\n\n", call. = FALSE)
    }
    
    out <- format(unclass(x), digits = digits)
    na_index <- attr(x, "na_index")
    
    out[na_index] <- paste0("NA(", names(na_index), ")")

    # format again to make sure all elements have same width
    return(format(out, justify = "right"))
}


`print.mixed_labelled` <- function(x, ...) {
    label <- variable_label(x)
    if (!is.null(label)) {
        label <- paste0(":", label)
    }

    cat(paste0("<mixed_labelled", likely_type(x), "[", length(x), "]>", label, "\n"))
    print(noquote(format_mixed(x)), ...)

    na_values <- attr(x, "na_values")
    if (!is.null(na_values)) {
        cat(paste0("Missing values: ", paste(na_values, collapse = ", "), "\n"))
    }

    na_range <- attr(x, "na_range")
    if (!is.null(na_range)) {
        cat(paste0("Missing range:  [", paste(na_range, collapse = ", "), "]\n"))
    }

    labels <- attr(x, "labels", exact = TRUE)

    if (length(labels) == 0) {
        return(invisible(x))
    }

    cat("\nLabels:", "\n", sep = "")

    print(data.frame(value = unname(labels), label = names(labels), row.names = NULL), row.names = FALSE)
    return(invisible(x))
}



`==.mixed_labelled` <- function(e1, e2) {
    e1 <- unclass(unmix(e1))
    e2 <- unclass(unmix(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(abs(unclass(unmix(e1)) - unclass(unmix(e2))) < .Machine$double.eps^0.5)
        # return(admisc::aeqb(unclass(unmix(e1)), unclass(unmix(e2))))
    }
    else {
        return(e1 == e2)
    }
}

`!=.mixed_labelled` <- function(e1, e2) {e1 <- unclass(unmix(e1))
    e2 <- unclass(unmix(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(abs(unclass(unmix(e1)) - unclass(unmix(e2))) < .Machine$double.eps^0.5)
        # return(admisc::aneqb(unclass(unmix(e1)), unclass(unmix(e2))))
    }
    else {
        return(e1 != e2)
    }
}

`<=.mixed_labelled` <- function(e1, e2) {
    e2 <- unclass(unmix(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(unclass(unmix(e1)) < (unclass(unmix(e2)) + .Machine$double.eps^0.5))
        # return(admisc::alteb(unclass(unmix(e1)), unclass(unmix(e2))))
    }
    else {
        return(e1 <= e2)
    }
}

`<.mixed_labelled` <- function(e1, e2) {
    e2 <- unclass(unmix(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return(unclass(unmix(e1)) < (unclass(unmix(e2)) - .Machine$double.eps^0.5))
        # return(admisc::altb(unclass(unmix(e1)), unclass(unmix(e2))))
    }
    else {
        return(e1 < e2)
    }
}

`>=.mixed_labelled` <- function(e1, e2) {
    e2 <- unclass(unmix(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return((unclass(unmix(e1)) + .Machine$double.eps^0.5) > unclass(unmix(e2)))
        # return(admisc::agteb(unclass(unmix(e1)), unclass(unmix(e2))))
    }
    else {
        return(e1 >= e2)
    }
}

`>.mixed_labelled` <- function(e1, e2) {
    e2 <- unclass(unmix(e2))
    if (admisc::possibleNumeric(e1) && admisc::possibleNumeric(e2)) {
        e1 <- admisc::asNumeric(e1)
        e2 <- admisc::asNumeric(e2)
        return((unclass(unmix(e1)) - .Machine$double.eps^0.5) > unclass(unmix(e2)))
        # return(admisc::agtb(unclass(unmix(e1)), unclass(unmix(e2))))
    }
    else {
        return(e1 > e2)
    }
}

`names<-.mixed_labelled` <- function(x, value) {
    attr(x, "names") <- value
    x
}

`duplicated.mixed_labelled` <- function(x, incomparables = FALSE, ...) {
    x <- unclass(unmix(x))
    NextMethod()
}

`unique.mixed_labelled` <- function(x, incomparables = FALSE, ...) {
    x[!duplicated(x)]
}

`head.mixed_labelled` <- function(x, n = 6L, ...) {
    x[seq(n)]
}

`tail.mixed_labelled` <- function(x, n = 6L, ...) {
    lx <- length(x)
    x[seq(lx - n + 1, lx)]
}

`na.omit.mixed_labelled` <- function (object, ...)  {
    attrx <- attributes(object)
    attrx$na_index <- NULL
    object <- unclass(object)
    object <- NextMethod()
    attrx$na.action <- attr(object, "na.action")
    nms <- attrx$names
    if (!is.null(nms) && !is.null(attrx$na.action)) {
        nms <- nms[-attr(object, "na.action")]
        attrx$names <- nms
    }
    attributes(object) <- attrx
    return(object)
}

`na.fail.mixed_labelled` <- function (object, ...)  {
    object <- unclass(object)
    NextMethod()
}

`na.exclude.mixed_labelled` <- function (object, ...)  {
    attrx <- attributes(object)
    attrx$na_index <- NULL
    object <- unclass(object)
    object <- NextMethod()
    attrx$na.action <- attr(object, "na.action")
    nms <- attrx$names
    if (!is.null(nms) && !is.null(attrx$na.action)) {
        nms <- nms[-attr(object, "na.action")]
        attrx$names <- nms
    }
    attributes(object) <- attrx
    return(object)
}

`mean.mixed_labelled` <- function(x, ...) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
        x <- x[-na_index]
    }
    x <- unclass(x)
    NextMethod()
}

`median.mixed_labelled` <- function(x, na.rm = FALSE, ...) {
    na_index <- attr(x, "na_index")
    if (!is.null(na_index)) {
        x <- x[-na_index]
    }
    x <- unclass(x)
    NextMethod()
}

`summary.mixed_labelled` <- function(object, ...) {
    na_index <- attr(object, "na_index")
    if (!is.null(na_index)) {
        object[na_index] <- NA
    }
    object <- unclass(object)
    NextMethod()
}
