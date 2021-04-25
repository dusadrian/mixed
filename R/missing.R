
`missing_values` <- function(x) {
    UseMethod("missing_values")
}

`missing_values.default` <- function(x) {
    # the vector does not have any class
    NULL
}

`missing_values.haven_labelled_spss` <- function(x) {
    unname(attr(x, "na_values", exact = TRUE))
}

`missing_values.mixed_labelled` <- function(x) {
    unname(attr(x, "tagged_values", exact = TRUE))
}

`missing_values.data.frame` <- function(x) {
    lapply(x, missing_values)
}


#----------------------------------------------


`missing_range` <- function(x) {
    UseMethod("missing_range")
}

`missing_range.default` <- function(x) {
    # the vector does not have any class
    NULL
}

`missing_range.haven_labelled_spss` <- function(x) {
    unname(attr(x, "na_range", exact = TRUE))
}

`missing_range.mixed_labelled` <- function(x) {
    unname(attr(x, "na_range", exact = TRUE))
}

`missing_range.data.frame` <- function(x) {
    lapply(x, missing_range)
}


#----------------------------------------------


`missing_values<-` <- function(x, value) {
    UseMethod("missing_values<-")
}

`missing_values<-.default` <- function(x, value) {
    if (!is.null(value)) {
        x <- mixed_labelled(x,
                labels = attr(x, "labels", exact = TRUE),
                na_values = value,
                na_range = attr(x, "na_range", exact = TRUE),
                label = attr(x, "label", exact = TRUE)
        )
    }
    return(x)
}

`missing_values<-.haven_labelled` <- function(x, value) {
    if (is.null(value)) {
        attr(x, "na_values") <- NULL
        if (is.null(attr(x, "na_range", exact = TRUE))) {
            x <- labelled(x,
                labels = attr(x, "labels", exact = TRUE),
                label = attr(x, "label", exact = TRUE)
            )
        }
    }
    else {
        x <- mixed_labelled(x,
            labels = attr(x, "labels", exact = TRUE),
            na_values = value,
            na_range = attr(x, "na_range", exact = TRUE),
            label = attr(x, "label", exact = TRUE)
        )
    }
    return(x)
}

`missing_values<-.mixed_labelled` <- function(x, value) {
    x <- unmix(x)

    attr(x, "na_values") <- NULL
    attr(x, "tagged_values") <- NULL

    if (is.null(value)) {
        x <- labelled(x,
            labels = unclass(attr(x, "labels", exact = TRUE)),
            label = attr(x, "label", exact = TRUE)
        )
    }
    else {
        x <- mixed_labelled(x,
            labels = unclass(attr(x, "labels", exact = TRUE)),
            na_values = value,
            na_range = attr(x, "na_range", exact = TRUE),
            label = attr(x, "label", exact = TRUE)
        )
    }
    
    return(x)
}

`missing_values<-.data.frame` <- function(x, value) {
    if (!is.list(value)) {
        value <- lapply(x, function(x) value)
    }

    for (var in names(value)) {
        if (!is.null(value[[var]]) & is.element(var, names(x))) {
            if (mode(x[[var]]) == mode(value[[var]])) {
                if (typeof(x[[var]]) != typeof(value[[var]])) {
                    mode(value[[var]]) <- typeof(x[[var]])
                }

                missing_values(x[[var]]) <- value[[var]]
            }
        }
    }
}


#----------------------------------------------


`missing_range<-` <- function(x, value) {
    UseMethod("missing_range<-")
}

`missing_range<-.default` <- function(x, value) {
    if (!is.null(value)) {
        x <- mixed_labelled(x,
            labels = unclass(attr(x, "labels", exact = TRUE)),
            na_values = attr(x, "na_values", exact = TRUE),
            na_range = value,
            label = attr(x, "label", exact = TRUE)
        )
    }
    return(x)
}

`missing_range<-.haven_labelled` <- function(x, value) {
    if (is.null(value)) {
        attr(x, "na_range") <- NULL
        if (is.null(attr(x, "na_values", exact = TRUE))) {
            x <- unmix(x)
            x <- labelled(x,
                    labels = attr(x, "labels", exact = TRUE),
                    label = attr(x, "label", exact = TRUE)
            )
        }
    }
    else {
        x <- mixed_labelled(x,
                labels = unclass(attr(x, "labels", exact = TRUE)),
                na_values = attr(x, "na_values", exact = TRUE),
                na_range = value,
                label = attr(x, "label", exact = TRUE)
        )
    }
}

`missing_range<-.mixed_labelled` <- function(x, value) {
    x <- unmix(x)

    attr(x, "na_range") <- NULL
    attr(x, "tagged_values") <- NULL

    if (is.null(value)) {
        attr(x, "na_values") <- NULL
    }
    else {
        x <- mixed_labelled(x,
                labels = attr(x, "labels", exact = TRUE),
                na_values = attr(x, "na_values", exact = TRUE),
                na_range = value,
                label = attr(x, "label", exact = TRUE)
        )
    }
    return(x)
}

`missing_range<-.data.frame` <- function(x, value) {
    lapply(x, missing_range)
}


#----------------------------------------------
