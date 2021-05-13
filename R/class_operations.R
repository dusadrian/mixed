`get_labeltext` <- function(x, prefix=": ") {
    label = attr(x, "label", exact = TRUE)
    if(!is.null(label)) {
        paste0(prefix, label)
    }
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
    #--------
    # either
    x <- unclass(untag(x))
    NextMethod()
}

`unique.haven_labelled` <- function(x, incomparables = FALSE, ...) {
    
    x <- x[!duplicated(x)]
    oa <- list(...)
    if (is.element("sort", names(oa)) && oa$sort) {
        return(sort_labelled(x, ... = ...))
    }

    return(x)
}

`unique.tagged` <- function(x, incomparables = FALSE, ...) {
    
    x <- x[!duplicated(x)]
    oa <- list(...)
    if (is.element("sort", names(oa)) && oa$sort) {
        return(sort_tagged(x, ... = ...))
    }

    return(x)
}

`sort.tagged` <- function(x, decreasing = FALSE, na.last = NA, ...) {
    return(sort_tagged(x, decreasing, na.last, ...))
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

`c_mixed_labelled` <- function(cargs, recursive = FALSE, use.names = TRUE) {
    # cargs <- list(...)

    na_values <- sort(unique(unlist(lapply(cargs, function(x) attr(x, "na_values", exact = TRUE)))))
    labels <- unlist(lapply(cargs, function(x) {
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
        if (is_mixed(x) | inherits(x, "tagged")) x <- untag(x)
        attributes(x) <- NULL
        return(x)
    }))

    mixed_labelled(
        vec_data(cargs),
        labels = labels,
        na_values = na_values,
        na_range = na_range,
        label = attr(cargs[[1]], "label", exact = TRUE)
    )
}

`cbind.mixed_labelled` <- function(..., deparse.level = 1) {
    cargs <- lapply(list(...), untag)
    cargs$deparse.level <- deparse.level
    do.call("cbind", cargs)
}

`as_factor.mixed_labelled` <- function(x, ..., only_labelled = TRUE) {
    oa <- list(...)
    if (is.element("untag", names(oa)) && is.logical(oa$untag)) {
        if (oa$untag[1]) { # to prevent an accidental logical vector
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

# `obj_print_header.mixed_labelled` <- function(x, ...) {
#     if (!inherits(x, "noprint")) {
#         cat(paste0("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x), "\n"))
#     }
#     invisible(x)
# }

# `obj_print_footer.mixed_labelled` <- function(x, ...) {
#     if (!inherits(x, "noprint")) {
#         na_values <- attr(x, "na_values")
#         if (!is.null(na_values)) {
#             cat(paste0("Missing values: ", paste(na_values, collapse = ", "), "\n"))
#         }

#         na_range <- attr(x, "na_range")
#         if (!is.null(na_range)) {
#             cat(paste0("Missing range: [", paste(na_range, collapse = ", "), "]\n"))
#         }
        
#         haven::print_labels(x)
#     }
# }

# `obj_print_data.mixed_labelled` <- function(x, ...) {
#     if (length(x) == 0) {
#         return(invisible(x))
#     }
    
#     out <- stats::setNames(format(x), names(x))
#     print(out, quote = FALSE)

#     invisible(x)
# }


#----------------------------------------------
# to propose adding to package labelled


`drop_unused_value_labels.mixed_labelled` <- function(x) {
    labels <- untag(attr(x, "labels", exact = TRUE))
    attr(x, "labels") <- as_mixed(labels[is.element(labels, unique(untag(x)))])
    return(x)
}


`sort_val_labels.mixed_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    return(x[order_labelled(x, according_to = according_to, decreasing = decreasing)])    
}
