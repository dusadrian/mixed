
`is_mixed` <- function(x) {
    inherits(x, "mixed_labelled")
}

`values_to_tag` <- function(x) {
    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    misvals <- c()

    if (is.null(na_values) & is.null(na_range)) {
        return(misvals)
    }


    if (!is.null(na_values)) {
        misvals <- sort(na_values)
    }

    if (!is.null(na_range)) {
        uniques <- sort(unique(x[x >= na_range[1] & x <= na_range[2]]))
        if (length(uniques) == 0) {
            uniques <- na_range
        }
        else {
            uniques <- sort(unique(c(uniques, na_range)))
        }

        uniques <- setdiff(uniques, na_values)
        misvals <- sort(unique(c(misvals, uniques)))
    }

    if (length(misvals) > length(letters)) {
        cat("\n")
        stop(simpleError("Too large span of missing values.\n\n"))
    }

    names(misvals) <- letters[seq(length(misvals))]
    return(misvals)
}

`as_mixed` <- function(x, ...) {
    UseMethod("as_mixed")
}

`as_mixed.default` <- function(x, ...) {
    cat("\n")
    stop(simpleError("Only labelled vectors can be converted to mixed labelled.\n\n"))
}

`as_mixed.haven_labelled` <- function(x, ...) {
    if (is_mixed(x) | !is.double(x)) {
        return(x)
    }

    class(x) <- setdiff(class(x), "haven_labelled_spss")
    
    tagged <- logical(length(x))
    if (is.double(x)) {
        tagged <- is_tagged_na(x)
    }
    
    if (sum(tagged) > 0) {
        cat("\n")
        stop(simpleError("Declared and tagged missing values should not be mixed.\n\n"))
    }

    tagged_values <- values_to_tag(x)
    
    if (length(tagged_values) > 0) {
        nms <- names(tagged_values)
        x[is.element(x, tagged_values)] <- tagged_na(names(tagged_values)[match(x[is.element(x, tagged_values)], tagged_values)])
        attr(x, "tagged_values") <- tagged_values
        class(x) <- c("mixed_labelled", class(x))
    }

    return(x)
}

`as_mixed.data.frame` <- function(x, ..., only_labelled = TRUE) {
    if (only_labelled) {
        labelled <- vapply(x, is.labelled, logical(1))
        x[labelled] <- lapply(x[labelled], as_mixed, ...)
    } else {
        x[] <- lapply(x, as_mixed)
    }

    return(x)
}

`unmix` <- function(x) {
    UseMethod("unmix")
}

`unmix.default` <- function(x) {
    # Do nothing, or perhaps an error?
    return(x)
}

`unmix.mixed_labelled` <- function(x) {
    attrx <- attributes(x)
    attributes(x) <- NULL

    tagged <- logical(length(x))
    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
    }

    tagged_values <- attrx[["tagged_values"]]
    nms <- names(tagged_values)

    if (!is.null(tagged_values) && any(tagged)) {
        tags <- haven::na_tag(x[tagged])
        x[which(tagged)[is.element(tags, nms)]] <- unname(tagged_values[match(tags[is.element(tags, nms)], nms)])
        tagged[which(tagged)[is.element(tags, nms)]] <- FALSE
    }

    if (sum(tagged) > 0) {
        cat("\n")
        stop("There should not be undeclared missing values into a mixed labelled object.\n\n", call. = FALSE)
    }

    # ------------------
    # the unclass part is VERY important to stay here, BEFORE replacing the values in the labels
    # because of my own choice of automatically transforming any (real) value into a tagged NA
    # when adding (but the same happens when replacing) values into a mixed_labelled object
    labels <- unclass(attrx$labels)
    # ------------------
    
    if (!is.null(labels)) {
        tagged <- logical(length(labels))
        if (is.double(labels)) {
            tagged <- haven::is_tagged_na(labels)
        }

        if (!is.null(tagged_values) && any(tagged)) {
            tags <- haven::na_tag(labels[tagged])
            labels[which(tagged)[is.element(tags, nms)]] <- unname(tagged_values[match(tags[is.element(tags, nms)], nms)])
        }

        attr(labels, "tagged_values") <- NULL # just in case
        attrx$labels <- labels
    }

    attrx$tagged_values <- NULL
    attrx$class <- c("haven_labelled_spss", setdiff(attrx$class, "mixed_labelled"))
    
    attributes(x) <- attrx
    return(x)
}

`unmix.data.frame` <- function(x) {
    mixed <- vapply(x, is_mixed, logical(1))
    x[mixed] <- lapply(x[mixed], unmix)
    
    return(x)
}

`validate_labelled` <- function(x) {
    labels <- attr(x, "labels")
    if (is.null(labels)) {
        return(x)
    }

    if (is.null(names(labels))) {
        stop(simpleError("`labels` must have names."))
    }
    if (any(duplicated(stats::na.omit(labels)))) {
        stop(simpleError("`labels` must be unique."))
    }

    x
}

`new_labelled` <- function(x = double(), labels = NULL, label = NULL,
                         ..., class = character()) {
    if (!is.numeric(x) && !is.character(x)) {
        cat("\n")
        stop(simpleError("`x` must be a numeric or a character vector.\n\n"))
    }

    if (mode(x) == mode(labels)) {
        if (typeof(x) != typeof(labels)) {
            mode(labels) <- typeof(x)
        }
    }
    
    if (!is.null(labels)) {
        lbls <- unclass(labels)
        if (!vec_is(lbls, x)) {
            cat("\n")
            stop(simpleError("`labels` must be same type as `x`.\n\n"))
        }

        oa <- list(...)
        if (is.element("tagged_values", names(oa))) {
            attr(labels, "tagged_values") <- oa$tagged_values
        }
    }

    if (!is.null(label) && (!is.character(label) || length(label) != 1)) {
        cat("\n")
        stop(simpleError("`label` must be a character vector of length one.\n\n"))
    }

    
    
    new_vctr(x,
        labels = labels,
        label = label,
        ...,
        class = c(class, "haven_labelled"),
        inherit_base_type = TRUE
    )
}

`mixed_labelled` <- function(x = double(), labels = NULL, na_values = NULL,
                          na_range = NULL, label = NULL) {

    x <- vec_data(x)
    xlabels <- attr(x, "labels", exact = TRUE)
    if (is.null(labels) & !is.null(xlabels)) {
        labels <- xlabels
    }

    xlabel <- attr(x, "label", exact = TRUE)
    if (is.null(label) & !is.null(xlabel)) {
        label <- xlabel
    }

    # # na_values <- vec_cast_named(na_values, x, x_arg = "na_values", to_arg = "x")
    # na_values <- stats::setNames(vec_cast(na_values, x, x_arg = "na_values", to_arg = "x"), names(na_values))
    labelled <- labelled(x, labels = labels, label = label)
    
    new_mixed_labelled(
        vec_data(labelled),
        labels = attr(labelled, "labels", exact = TRUE),
        na_values = na_values,
        na_range = na_range,
        label = attr(labelled, "label", exact = TRUE)
    )
}

`new_mixed_labelled` <- function(x, labels, na_values, na_range, label) {
    declared <- logical(length(x))
    
    if (!is.null(na_values)) {
        declared <- is.element(x, na_values)
        if (any(is.na(na_values))) {
            cat("\n")
            stop(simpleError("`na_values` can not contain missing values.\n\n"))
        }
    }
    
    if (any(is_tagged_na(x))) {
        cat("\n")
        stop(simpleError("Cannot mix declared and tagged missing values.\n\n"))
    }

    if (!is.null(na_range) && is.numeric(x)) {
        type_ok <- (is.character(x) && is.character(na_range)) || (is.numeric(x) && is.numeric(na_range))
        
        if (!type_ok || length(na_range) != 2) {
            cat("\n")
            stop(simpleError("`na_range` must be a vector of length two the same type as `x`.\n\n"))
        }
        
        if (any(is.na(na_range))) {
            cat("\n")
            stop(simpleError("`na_range` can not contain missing values.\n\n"))
        }
        
        if (na_range[1] >= na_range[2]) {
            cat("\n")
            stop(simpleError("`na_range` must be in ascending order.\n\n"))
        }

        declared <- declared | (x >= na_range[1] & x <= na_range[2])
    }

    if (sum(declared) > 0 && !is.numeric(x)) {
        cat("\n")
        stop(simpleError("Missing values can be declared only for numeric variables.\n\n"))
    }
    
    tagged_values <- sort(unique(x[declared]))
    
    if (length(tagged_values) > length(letters)) {
        cat("\n")
        stop(simpleError("Too many declared missing values.\n\n"))
    }
    
    for (i in seq(length(tagged_values))) {
        x[x == tagged_values[i]] <- tagged_na(letters[i])
    }

    if (!is.null(labels)) {
        for (i in seq(length(tagged_values))) {
            labels[labels == tagged_values[i]] <- tagged_na(letters[i])
        }
        # add these classes to allow printing tagged NA values when asking:
        # attr(x, "labels")
        class(labels) <- c("mixed_labelled", "vctrs_vctr", "noprint", class(labels))
    }
    
    if (length(tagged_values) > 0) {
        names(tagged_values) <- letters[seq(length(tagged_values))]
    }
    
    new_labelled(x,
        labels = labels,
        label = label,
        tagged_values = tagged_values,
        na_values = na_values,
        na_range = na_range,
        class = "mixed_labelled"
    )
}

`get_labeltext` <- function(x, prefix=": ") {
    label = attr(x, "label", exact = TRUE)
    if(!is.null(label)) {
        paste0(prefix, label)
    }
}
