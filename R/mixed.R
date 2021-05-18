
`is_mixed` <- function(x) {
    inherits(x, "mixed_labelled")
}

`values_to_tag` <- function(x) {
    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    labels <- attr(x, "labels", exact = TRUE)
    misvals <- c()

    if (is.null(na_values) & is.null(na_range)) {
        return(list())
    }

    if (!is.null(na_values)) {
        misvals <- sort(na_values)
    }

    if (is.numeric(x)) {
        if (is.numeric(labels)) {
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

    large_numbers <- logical(length(misvals))
    numbers <- unlist(lapply(large_numbers, possibleNumeric))

    if (any(numbers)) {
        large_numbers[numbers] <- abs(asNumeric(misvals[numbers])) > 32767
    }
    
    if (any(!numbers)) {
        if (any(nchar(misvals[!numbers]) > 2)) {
            cat("\n")
            stop(simpleError("Non-numerical missing values should have at most 2 characters.\n\n"))
        }
    }

    result <- list(justright = misvals[!large_numbers], large_numbers = misvals[large_numbers])
    
    if (length(result$large_numbers) > length(letters)) {
        cat("\n")
        stop(simpleError("Too large span of missing values.\n\n"))
    }

    if (length(result$large_numbers) > 0) {
        names(result$large_numbers) <- letters[seq(length(result$large_numbers))]
    }

    return(result)
}

`as_mixed` <- function(x, ...) {
    UseMethod("as_mixed")
}

`as_mixed.default` <- function(x, ...) {
    return(x)
}

`as_mixed.haven_labelled` <- function(x, ...) {
    
    if (is_mixed(x) || !is.atomic(x) || is.character(x)) {
        return(x)
    }

    class(x) <- setdiff(class(x), "haven_labelled_spss")
    attrx <- attributes(x)
    labels <- attr(x, "labels", exact = TRUE)
    tagged_values <- values_to_tag(x)

    x <- untag(x)
    attributes(x) <- NULL

    if (!is.null(unlist(tagged_values))) {
        if (length(tagged_values$justright) > 0) {
            x[is.element(x, tagged_values$justright)] <- tag(x[is.element(x, tagged_values$justright)])
        }
        
        if (length(tagged_values$large_numbers) > 0) {
            nms <- names(tagged_values$large_numbers)
            x[is.element(x, tagged_values$large_numbers)] <- tag(names(tagged_values$large_numbers)[match(x[is.element(x, tagged_values$large_numbers)], tagged_values$large_numbers)])
            attrx$large_numbers <- tagged_values$large_numbers
        }
    }

    if (!is.null(labels) && is.numeric(labels)) {
        
        if (length(tagged_values$justright) > 0) {
            labels[is.element(labels, tagged_values$justright)] <- tag(labels[is.element(labels, tagged_values$justright)])
        }

        if (length(tagged_values$large_numbers) > 0) {
            labels[is.element(labels, tagged_values$large_numbers)] <- tag(names(tagged_values$large_numbers)[match(labels[is.element(labels, tagged_values$large_numbers)], tagged_values$large_numbers)])
            attr(labels, "large_numbers") <- tagged_values$large_numbers
        }

        # add these classes to allow printing tagged NA values when typing:
        # attr(x, "labels")
        
        if (any(has_tag(labels))) {
            class(labels) <- c("tagged", "vctrs_vctr", class(labels))
        }
        
        attrx$labels <- labels
    }

    attrx$class <- c("mixed_labelled", "haven_labelled", "vctrs_vctr", class(x))
    attributes(x) <- attrx
    
    return(x)
}

`as_mixed.data.frame` <- function(x, ..., only_labelled = TRUE) {
    if (only_labelled) {
        labelled <- vapply(x, is.labelled, logical(1))
        x[labelled] <- lapply(x[labelled], as_mixed, ...)
    } else {
        x[] <- lapply(x, as_mixed, ...)
    }

    return(x)
}

`unmix` <- function(x) {
    UseMethod("unmix")
}

`unmix.default` <- function(x) {
    return(x)
}

`unmix.mixed_labelled` <- function(x) {
    # `unmx` <- function(x) {
    attrx <- attributes(x)
    tagged <- has_tag(x)
    attributes(x) <- NULL
    
    if (any(tagged)) {
        x[tagged] <- get_tag(x[tagged])
    }

    labels <- attrx$labels
    
    # ------------------
    if (!is.null(labels)) {
        tagged <- has_tag(labels)

        # ------------------
        # the vec_data() part is VERY important to stay here, BEFORE replacing the values in the
        # labels because of my the choice to automatically transform any (actual) value into a 
        # tagged NA when adding (and the same happens when replacing) values into this object
        labels <- vec_data(labels)
        
        if (any(tagged)) {
            labels[tagged] <- get_tag(labels[tagged])
        }

        attr(labels, "large_numbers") <- NULL # just in case
        attrx$labels <- labels
    }

    attrx$large_numbers <- NULL
    attrx$class <- c("haven_labelled_spss", setdiff(attrx$class, "mixed_labelled"))
    attributes(x) <- attrx
    return(x)
}

`unmix.data.frame` <- function(x) {
    mixed <- vapply(x, is_mixed, logical(1))
    x[mixed] <- lapply(x[mixed], untag)
    
    return(x)
}

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

        if (!vec_is(unclass(labels), x)) {
            cat("\n")
            stop(simpleError("`labels` must be same type as `x`.\n\n"))
        }
    }

    if (!is.null(label) && (!is.atomic(label) || !is.character(label) || length(label) != 1)) {
        cat("\n")
        stop(simpleError("`label` must be a character vector of length one.\n\n"))
    }
    
    if (!is.null(na_values)) {
        if (any(is.na(na_values))) {
            cat("\n")
            stop(simpleError("`na_values` can not contain missing values.\n\n"))
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

`new_labelled` <- function(x = double(), labels = NULL, label = NULL,
                    na_values = NULL, na_range = NULL, ..., class = character()) {
    
    if (mode(x) == mode(labels)) {
        if (typeof(x) != typeof(labels)) {
            mode(labels) <- typeof(x)
        }
    }
    
    dots <- list(...)
    if (!is.null(labels)) {
        if (is.element("large_numbers", names(dots))) {
            if (length(dots$large_numbers) > 0) {
                attr(labels, "large_numbers") <- dots$large_numbers
            }
        }
    }
    
    new_vctr(x,
        labels = labels,
        label = label,
        na_values = na_values,
        na_range = na_range,
        ...,
        class = c(class, "haven_labelled"),
        inherit_base_type = TRUE
    )
}

`mixed_labelled` <- function(x = double(), labels = NULL, na_values = NULL,
                          na_range = NULL, label = NULL, ...) {

    x <- vec_data(x)
    if (inherits(labels, "tagged")) {
        if (is.character(untag(labels))) {
            cat("\n")
            stop(simpleError("Tagged values in `labels` have to be numeric.\n\n"))
        }
    }

    labels <- stats::setNames(vec_cast(vec_data(labels), x, x_arg = "labels", to_arg = "x"), names(labels))
    validate_labelled(x, labels, label, na_values, na_range)

    if (is.null(na_values) && is.null(na_range)) {
        return(new_labelled(x, labels = labels, label = label))
    }

    dots <- list(...)
    declared <- logical(length(x))
    tagged <- has_tag(x)

    if (is.character(na_values) || any(is.na(na_values))) {
        cat("\n")
        stop(simpleError("`na_values` has to be numeric.\n\n"))
    }
    
    if (!is.null(na_values) || !is.null(na_range)) {
        if (any(tagged)) {
            tags <- unique(get_tag(x[tagged]))
            numeric <- unlist(lapply(tags, possibleNumeric))
            tags <- setdiff(tags, na_values)

            if (any(numeric) && !is.null(na_range)) {
                numtags <- as.numeric(tags[numeric])
                tags <- tags[-which(numeric)[numtags >= na_range[1] & numtags <= na_range[2]]]
            }
            
            if (length(tags) > 0 && is.element("large_numbers", names(dots))) {
                tags <- setdiff(tags, names(dots$large_numbers))
            }

            if (length(tags) > 0) {
                cat("\n")
                stop(simpleError("There are undeclared, tagged missing values in `x`.\n\n"))
            }
        }

        # what if there are already tagged values in x, but still
        # untagged values in x corresponding to declared na_values...?

        declared <- isElement(x, na_values)

        if (!is.null(na_range) && is.numeric(x)) {
            declared <- declared | (x >= na_range[1] & x <= na_range[2])
        }
    }
    
    #---------------------------------------
    # when `x` is a labelled object, it might have labels and label attributes

    xlabels <- attr(x, "labels", exact = TRUE)
    if (is.null(labels) & !is.null(xlabels)) {
        labels <- xlabels
    }

    xlabel <- attr(x, "label", exact = TRUE)
    if (is.null(label) & !is.null(xlabel)) {
        label <- xlabel
    }
    
    #---------------------------------------
    

    if (sum(declared) > 0) {

        if (!is.numeric(x)) {
            cat("\n")
            stop(simpleError("Missing values can be declared only for numeric variables.\n\n"))
        }
    
        declared <- sort(unique(x[declared]))
        
        ln <- logical(length(declared))
        pN <- unlist(lapply(declared, possibleNumeric))
        if (any(pN)) {
            ln[pN] <- abs(asNumeric(declared[pN])) > 32767
        }

        large_numbers <- declared[ln]

        if (length(large_numbers) > length(letters)) {
            cat("\n")
            stop(simpleError("Too many large missing values.\n\n"))
        }

        if (length(declared[!ln]) > 0) {
            x[is.element(x, declared[!ln])] <- tag(x[is.element(x, declared[!ln])])
        }

        if (length(large_numbers) > 0) {
            names(large_numbers) <- letters[seq(length(large_numbers))]
            x[is.element(x, large_numbers)] <- tag(letters[match(x[is.element(x, large_numbers)], large_numbers)])
        }
        else {
            large_numbers <- NULL
        }

        if (!is.null(labels)) {
            if (length(declared[!ln]) > 0) {
                labels[is.element(labels, declared[!ln])] <- tag(labels[is.element(labels, declared[!ln])])
            }

            if (length(large_numbers) > 0) {
                labels[is.element(labels, large_numbers)] <- tag(letters[match(labels[is.element(labels, large_numbers)], large_numbers)])
                attr(labels, "large_numbers") <- large_numbers
            }

            # add these classes to allow printing tagged NA values when typing, for instance:
            # attr(x, "labels")

            if (any(has_tag(labels))) {
                class(labels) <- c("tagged", "vctrs_vctr", class(labels))
            }
        }
    }
    
    
    new_labelled(x,
        labels = labels,
        label = label,
        large_numbers = large_numbers,
        na_values = na_values,
        na_range = na_range,
        class = "mixed_labelled"
    )
}
