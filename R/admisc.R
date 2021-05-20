`possibleNumeric` <- function(x) {
    if (all(is.na(x))) {
        return(FALSE)
    }

    if (inherits(x, "haven_labelled")) {
        return(Recall(unclass(x)) && !any(is.na(suppressWarnings(as.numeric(names(attr(x, "labels")))))))
    }

    if (is.numeric(x)) {
        return(TRUE)
    }

    if (is.factor(x)) {
        return(!any(is.na(suppressWarnings(as.numeric(levels(x))))))
    }

    # https://stackoverflow.com/questions/34613761/detect-non-ascii-characters-in-a-string
    if (any(grepl("[^!-~]", x))) {
        return(FALSE)
    }

    # as.character converts everything (especially factors)
    return(!any(is.na(suppressWarnings(as.numeric(na.omit(x))))))
}


`asNumeric` <- function(x) {
    if (is.numeric(x)) {
        return(x)
    }

    if (is.factor(x)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }

    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~]", x)
    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))

    return(result)
}
