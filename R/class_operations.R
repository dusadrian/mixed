`==.mixed_labelled` <- function(e1, e2) {
    e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_equal(e1, e2)
}

`!=.mixed_labelled` <- function(e1, e2) {
    e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    !vec_equal(e1, e2)
}

`anyDuplicated.mixed_labelled` <- function(x, incomparables = FALSE, ...) {
    vec_duplicate_any(unmix(x))
}

`duplicated.mixed_labelled` <- function(x, incomparables = FALSE, ...) {
    #--------
    # either
    x <- unmix(x)
    NextMethod()

    #--------
    # or
    # vec_duplicate_id(unmix(x)) != seq_along(x)
}

`duplicated.haven_labelled` <- function(x, incomparables = FALSE, ...) {
    duplicates <- logical(length(x))
    tagged <- admisc::has_tag(x)

    ix <- seq_along(x)
    if (any(tagged)) {
        duplicates[ix[tagged][duplicated(admisc::get_tag(x[tagged]))]] <- TRUE
    }

    duplicates[ix[!tagged][duplicated(unclass(x[!tagged]))]] <- TRUE

    return(duplicates)
}

`unique.haven_labelled` <- function(x, incomparables = FALSE, ...) {
    
    x <- x[!duplicated(x)]
    oa <- list(...)
    if (is.element("sort", names(oa)) && oa$sort) {
        return(sort_labelled(x, ... = ...))
    }

    return(x)
}

`<=.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_compare(e1, e2) <= 0
}

`<.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_compare(e1, e2) < 0
}

`>=.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_compare(e1, e2) >= 0
}

`>.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_compare(e1, e2) > 0
}

`+.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    if (missing(e2)) {
        vec_arith("+", e1, MISSING())
    } else {
        vec_arith("+", e1, e2)
    }
}

`-.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    if (missing(e2)) {
        vec_arith("-", e1, MISSING())
    } else {
        vec_arith("-", e1, e2)
    }
}

`*.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("*", e1, e2)
}

`/.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("/", e1, e2)
}

`^.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("^", e1, e2)
}

`%%.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("%%", e1, e2)
}

`%/%.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("%/%", e1, e2)
}

`!.mixed_labelled` <- function(x) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("!", x, MISSING())
}

`&.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("&", e1, e2)
}

`|.mixed_labelled` <- function(e1, e2) {
    if (is_mixed(e1)) e1 <- unmix(e1)
    if (is_mixed(e2)) e2 <- unmix(e2)
    vec_arith("|", e1, e2)
}

`[<-.mixed_labelled` <- function(x, i, value) {
    na_values <- attr(x, "na_values", exact = TRUE)
    
    if (any(is.element(value, na_values))) {
        valmatch <- match(value, na_values)
        tagged_values <- attr(x, "tagged_values", exact = TRUE)
        if (!is.null(tagged_values)) {
            el <- is.element(na_values, tagged_values)
            na_values[el] <- names(tagged_values)[match(na_values[el], tagged_values)]
        }

        value[!is.na(valmatch)] <- admisc::tag_na(na_values[valmatch[!is.na(valmatch)]])
    }

    NextMethod()
}

`mean.mixed_labelled` <- function(x, ...) {
    x <- unmix(x)
    
    na_values <- attr(x, "na_values", exact = TRUE)
    x <- x[!admisc::isElement(x, na_values)]
    
    na_range <- attr(x, "na_range", exact = TRUE)
    if (!is.null(na_range)) {
        x <- x[x < na_range[1] | x > na_range[2]]
    }

    mean(unclass(x), ...)
}

`median.mixed_labelled` <- function(x, na.rm = TRUE, ...) {
    x <- unmix(x)

    NextMethod()
}

`c.mixed_labelled` <- function(..., recursive = FALSE, use.names = TRUE) {
    cargs <- list(...)

    checks <- lapply(cargs, function(x) {
        if (!is_mixed(x) && is.double(x)) {
            if (any(admisc::has_tag(x))) {
                cat("\n")
                stop(simpleError("Declared and tagged missing values should not be mixed.\n\n"))
            }
        }
    })

    na_values <- sort(unique(unlist(lapply(cargs, function(x) attr(x, "na_values", exact = TRUE)))))
    
    labels <- unlist(lapply(cargs, function(x) attr(x, "labels", exact = TRUE)))
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

    na_range <- lapply(cargs, function(x) attr(x, "na_range", exact = TRUE))
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

    cargs <- unlist(lapply(cargs, function(x) {
        if (is_mixed(x)) x <- unmix(x)
        attributes(x) <- NULL
        return(x)
    }))

    label <- attr(cargs[[1]], "label", exact = TRUE)

    new_mixed_labelled(
        vec_data(cargs),
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = label
    )
}

`cbind.mixed_labelled` <- function(..., deparse.level = 1) {
    cargs <- lapply(list(...), unmix)
    cargs$deparse.level <- deparse.level
    do.call("cbind", cargs)
}

`as_factor.mixed_labelled` <- function(x, ..., only_labelled = TRUE) {
    oa <- list(...)
    if (is.element("unmix", names(oa)) && is.logical(oa$unmix)) {
        if (oa$unmix[1]) { # to prevent an accidental logical vector
            x <- unmix(x)
        }
    }
    else {
        # unmix by default
        x <- unmix(x)
    }
    
    NextMethod()
}


#----------------------------------------------


`vec_ptype_full.mixed_labelled` <- function(x, ...) {
    paste0("mixed_labelled<", vec_ptype_full(vec_data(x)), ">")
}

`obj_print_header.mixed_labelled` <- function(x, ...) {
    if (!inherits(x, "noprint")) {
        cat(paste0("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x), "\n"))
    }
    invisible(x)
}

`obj_print_footer.mixed_labelled` <- function(x, ...) {
    if (!inherits(x, "noprint")) {
        na_values <- attr(x, "na_values")
        if (!is.null(na_values)) {
            cat(paste0("Missing values: ", paste(na_values, collapse = ", "), "\n"))
        }

        na_range <- attr(x, "na_range")
        if (!is.null(na_range)) {
            cat(paste0("Missing range: [", paste(na_range, collapse = ", "), "]\n"))
        }
        
        haven::print_labels(unmix(x))
    }
}

`obj_print_data.mixed_labelled` <- function(x, ...) {
    if (length(x) == 0) {
        return(invisible(x))
    }
    
    x <- unmix(x)
    
    out <- stats::setNames(format(x), names(x))
    print(out, quote = FALSE)

    invisible(x)
}

`format.mixed_labelled` <- function(x, ..., digits = getOption("digits")) {
    format(vec_data(unmix(x)), ...)
}


#----------------------------------------------
# to propose adding to package labelled


`drop_unused_value_labels.mixed_labelled` <- function(x) {
    labels <- unmix(attr(x, "labels", exact = TRUE))
    attr(x, "labels") <- as_mixed(labels[is.element(labels, unique(unmix(x)))])
    return(x)
}


`sort_val_labels.mixed_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    return(x[order_labelled(x, according_to = according_to, decreasing = decreasing)])    
}
