`get_labeltext` <- function(x, prefix=": ") {
    label = attr(x, "label", exact = TRUE)
    if(!is.null(label)) {
        paste0(prefix, label)
    }
}

`anyDuplicated.haven_labelled` <- function(x, incomparables = FALSE, ...) {
    vec_duplicate_any(untag(x))
}

`duplicated.haven_labelled` <- function(x, incomparables = FALSE, ...) {
    #--------
    # either
    x <- unclass(untag(x))
    NextMethod()

    #--------
    # or
    # vec_duplicate_id(untag(x)) != seq_along(x)
}

`duplicated.tagged` <- function(x, incomparables = FALSE, ...) {
    x <- unclass(untag(x))
    NextMethod()
}

`unique.haven_labelled` <- function(x, incomparables = FALSE, ...) {
    
    x <- x[!duplicated(x)]
    dots <- list(...)
    if (is.element("sort", names(dots)) && dots$sort) {
        return(sort_labelled(x, ... = ...))
    }

    return(x)
}

`unique.tagged` <- function(x, incomparables = FALSE, ...) {
    
    x <- x[!duplicated(x)]
    dots <- list(...)
    if (is.element("sort", names(dots)) && dots$sort) {
        return(sort_tagged(x, ... = ...))
    }

    return(x)
}

`sort.tagged` <- function(x, decreasing = FALSE, na.last = NA, ...) {
    return(sort_tagged(x, decreasing, na.last, ...))
}

`==.haven_labelled` <- function(e1, e2) {
    if (is.character(e2) && !all(has_tag(e2))) {
        return(logical(length(e1)))
    }

    return(unclass(untag(e1)) == unclass(untag(e2)))
}

`!=.haven_labelled` <- function(e1, e2) {
    if (is.character(e2) && !all(has_tag(e2))) {
        return(logical(length(e1)))
    }

    return(unclass(untag(e1)) != unclass(untag(e2)))
}

`<=.haven_labelled` <- function(e1, e2) {
    return(unclass(untag(e1)) <= unclass(untag(e2)))
}

`<.haven_labelled` <- function(e1, e2) {
    return(unclass(untag(e1)) < unclass(untag(e2)))
}

`>=.haven_labelled` <- function(e1, e2) {
    return(unclass(untag(e1)) >= unclass(untag(e2)))
}

`>.haven_labelled` <- function(e1, e2) {
    return(unclass(untag(e1)) > unclass(untag(e2)))
}

`==.tagged` <- function(e1, e2) {
    return(unclass(untag(e1)) == unclass(untag(e2)))
}

`!=.tagged` <- function(e1, e2) {
    return(unclass(untag(e1)) != unclass(untag(e2)))
}

`<=.tagged` <- function(e1, e2) {
    return(unclass(untag(e1)) <= unclass(untag(e2)))
}

`<.tagged` <- function(e1, e2) {
    return(unclass(untag(e1)) < unclass(untag(e2)))
}

`>=.tagged` <- function(e1, e2) {
    return(unclass(untag(e1)) >= unclass(untag(e2)))
}

`>.tagged` <- function(e1, e2) {
    return(unclass(untag(e1)) > unclass(untag(e2)))
}

`[<-.haven_labelled` <- function(x, i, value) {
    tagged <- has_tag(x)
    if (any(tagged)) {
        isel <- is.element(value, get_tag(x[tagged]))
        if (any(isel)) {
            value[isel] <- tag(value[isel])
        }
    }

    NextMethod()
}

`mean.haven_labelled` <- function(x, ...) {
    x <- x[!has_tag(x)]

    mean(unclass(x), ...)
}

`mean.tagged` <- function(x, ...) {
    x <- x[!has_tag(x)]

    mean(unclass(x), ...)
}

`median.tagged` <- function(x, na.rm = TRUE, ...) {
    median(vec_data(x), na.rm = TRUE, ...)
}

`c_mixed_labelled` <- function(dots, recursive = FALSE, use.names = TRUE) {
    # dots <- list(...)
    mixed <- unlist(lapply(dots, is_mixed))
    na_values <- sort(unique(unlist(lapply(dots, function(x) attr(x, "na_values", exact = TRUE)))))
    labels <- unlist(lapply(dots, function(x) {
        untag(attr(x, "labels", exact = TRUE))
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
        if (is_mixed(x) | inherits(x, "tagged")) x <- untag(x)
        attributes(x) <- NULL
        return(x)
    }))

    mixed_labelled(
        vec_data(dots),
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = attr(dots[[which(mixed)[1]]], "label", exact = TRUE)
    )
}

`cbind.mixed_labelled` <- function(..., deparse.level = 1) {
    cargs <- lapply(list(...), untag)
    cargs$deparse.level <- deparse.level
    do.call("cbind", cargs)
}

`levels.tagged` <- function(x) {
    NULL
}

`names<-.tagged` <- function(x, value) {
    attr(x, "names") <- value
    x
}

`as_factor.mixed_labelled` <- function(x, ..., only_labelled = TRUE) {
    dots <- list(...)
    if (is.element("untag", names(dots)) && is.logical(dots$untag)) {
        if (dots$untag[1]) { # to prevent an accidental logical vector
            x <- untag(x)
        }
    }
    else {
        # untag by default
        x <- untag(x)
    }
    
    NextMethod()
}


#----------------------------------------------


`vec_ptype_full.mixed_labelled` <- function(x, ...) {
    paste0("mixed_labelled<", vec_ptype_full(vec_data(x)), ">")
}


`drop_unused_value_labels.mixed_labelled` <- function(x) {
    labels <- untag(attr(x, "labels", exact = TRUE))
    attr(x, "labels") <- as_mixed(labels[is.element(labels, unique(untag(x)))])
    return(x)
}


`sort_val_labels.mixed_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    return(x[order_labelled(x, according_to = according_to, decreasing = decreasing)])    
}


`val_labels<-.mixed_labelled` <- function(x, value) {
    if (is.null(value)) {
        tagged <- has_tag(x)
        x <- unclass(x)
        attr(x, "labels") <- NULL
        if (any(tagged)) {
            class(x) <- c("tagged", "vctrs_vctr", class(x))
        }
    }
    else {
        x <- mixed_labelled(unmix(x),
                labels = value,
                na_values = attr(x, "na_values", exact = TRUE),
                na_range = attr(x, "na_range", exact = TRUE),
                label = attr(x, "label", exact = TRUE))
    }
    return(x)
}


`vec_ptype2.double.tagged` <- function(x, y, ...) vec_ptype2(x, vec_data(y), ...)

`vec_ptype2.integer.tagged` <- vec_ptype2.double.tagged

`vec_ptype2.character.tagged` <- vec_ptype2.double.tagged

`vec_ptype2.tagged.double` <- function(x, y, ...) vec_ptype2(y, x, ...)

`vec_ptype2.tagged.integer` <- vec_ptype2.tagged.double

`vec_ptype2.tagged.character` <- vec_ptype2.tagged.double

`vec_ptype2.tagged.tagged` <- function(x, y, ..., x_arg = "", y_arg = "") {
    vec_ptype2(vec_data(x), vec_data(y), ..., x_arg = x_arg, y_arg = y_arg)
}



`vec_cast.double.tagged` <- function(x, to, ...) vec_cast(vec_data(untag(x)), to)

`vec_cast.integer.tagged` <- function(x, to, ...) vec_cast(vec_data(untag(x)), to)

`vec_cast.character.tagged` <- function(x, to, ...) {
    vec_data(untag(x))
}

`vec_cast.tagged.tagged` <- function(x, to, ..., x_arg = "", to_arg = "") {
    out <- vec_cast(vec_data(x), vec_data(to), ..., x_arg = x_arg, to_arg = to_arg)

    # do we lose tagged na values?
    if (is.double(x) && !is.double(out)) {
        lossy <- has_tag(x)
        maybe_lossy_cast(out, x, to, lossy,
            x_arg = x_arg,
            to_arg = to_arg,
            details = "Only doubles can hold tagged na values."
        )
    }
  
    out
}

`vec_cast.tagged.double` <- function(x, to, ...) {
    vec_cast.tagged.tagged(x, to, ...)
}

`vec_cast.tagged.integer` <- function(x, to, ...) {
    vec_cast.tagged.tagged(x, to, ...)
}

`vec_cast.tagged.character` <- function(x, to, ...) {
    vec_cast.tagged.tagged(x, to, ...)
}

`as.character.tagged` <- function(x, ...) {
    as.character(vec_data(x))
}


`vec_arith.tagged` <- function(op, x, y, ...) {
    UseMethod("vec_arith.tagged", y)
}

`vec_arith.tagged.default` <- function(op, x, y, ...) {
    stop_incompatible_op(op, x, y)
}

`vec_arith.tagged.tagged` <- function(op, x, y, ...) {
    vec_arith_base(op, x, y)
}

`vec_arith.tagged.numeric` <- function(op, x, y, ...) {
    vec_arith_base(op, x, y)
}

`vec_arith.numeric.tagged` <- function(op, x, y, ...) {
    vec_arith_base(op, x, y)
}


`vec_math.tagged` <- function(.fn, .x, ...) {
    vec_math_base(.fn, .x, ...)
}
