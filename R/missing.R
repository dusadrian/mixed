`na_values.mixed_labelled` <- function(x) {
    attr(x, "na_values", exact = TRUE)
}

`na_range.mixed_labelled` <- function(x) {
    attr(x, "na_range", exact = TRUE)
}

#-----------------------------------------

`na_values<-.mixed_labelled` <- function(x, value) {
    x <- untag(x)

    attr(x, "na_values") <- NULL
    attr(x, "large_numbers") <- NULL

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

`na_range<-.mixed_labelled` <- function(x, value) {
    x <- untag(x)

    attr(x, "na_range") <- NULL
    attr(x, "large_numbers") <- NULL

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

# `na_values<-.haven_labelled` <- function(x, value) {
#     if (is.null(value)) {
#         attr(x, "na_values") <- NULL
#         if (is.null(attr(x, "na_range", exact = TRUE))) {
#             x <- labelled(x,
#                 labels = attr(x, "labels", exact = TRUE),
#                 label = attr(x, "label", exact = TRUE)
#             )
#         }
#     }
#     else {
#         x <- mixed_labelled(x,
#             labels = attr(x, "labels", exact = TRUE),
#             na_values = value,
#             na_range = attr(x, "na_range", exact = TRUE),
#             label = attr(x, "label", exact = TRUE)
#         )
#     }
#     return(x)
# }
