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
    
    return(suppressWarnings(as.numeric(as.character(x))))
}
