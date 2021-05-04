`has_tag` <- function(x, tag = NULL) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }

    if (!is.null(tag) && !is.atomic(tag) && (length(tag) > 1 || is.na(tag))) {
        cat("\n")
        stop("`tag` should be a vector of length 1.\n\n", call. = FALSE)
    }

    return(.Call("C_is_tagged_na", x, as.character(tag)))
}

`get_tag` <- function(x) {
    if (is.character(x)) {
        return(gsub("N|A|\\(|\\)|\\.", "",  x))
    }
    else if (is.double(x)) {
        x <- .Call("C_na_tag", x, PACKAGE = "mixed")
        if (!any(is.na(suppressWarnings(as.numeric(na.omit(x)))))) {
            x <- as.numeric(x)
        }
        return(x)
    }
    else {
        # cat("\n")
        # stop("Unsuitable input to extract a tagged value.\n\n", call. = FALSE)
        return(rep(NA, length(x)))
    }
}

# is a string representing a tagged NA such as ".a" or "NA(a)"?
`is_tagged_string` <- function(x) {
    if (!is.character(x)) {
        return(logical(length(x)))
    }

    return(
        is.element(get_tag(x), letters) && 
        (
            (nchar(x) == 2 & grepl("^\\.", x)) ||
            (nchar(x) == 5 & grepl("^NA\\(", x) & grepl("\\)", x))
        )
    )
}
