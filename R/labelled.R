`order_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE,
    user_na = c("last", "first", "ignore", "na"), na_value = c("last", "first", "na")) {

    according_to <- match.arg(according_to)
    user_na <- match.arg(user_na)
    na_value = match.arg(na_value)

    if (is_mixed(x)) {
        x <- unmix(x)
    }
    
    labels <- attr(x, "labels", exact = TRUE)
    na_values <- attr(x, "na_values", exact = TRUE)
    na_range <- attr(x, "na_range", exact = TRUE)

    indexes <- seq_along(x)

    attrx <- attributes(x)
    attributes(x) <- NULL

    tagged <- logical(length(x))
    if (is.double(x)) {
        tagged <- has_tag(x)
    }

    xmis <- isElement(x, na_values)

    if (!is.null(na_range)) {
        xmis <- xmis | (x >= na_range[1] & x <= na_range[2])
    }

    truena <- is.na(x) & !tagged
    na_indexes <- indexes[truena]
    tagged <- tagged[!truena]
    xmis <- xmis[!truena]
    indexes <- indexes[!truena]

    x <- x[!truena]
    
    result <- c()
    if (na_value == "first") {
        result <- na_indexes
        length(na_indexes) <- 0
    }

    y <- x

    ltagged <- logical(length(labels))
    if (is.double(labels)) {
        ltagged <- has_tag(labels)
    }
    
    if (any(ltagged)) {
        labels <- unclass(labels)
        labels[ltagged] <- na_tag(labels[ltagged])
    }

    if (any(tagged)) {
        y <- unclass(y)
        y[tagged] <- na_tag(x[tagged])
    }

    if (according_to == "labels") {
        haslabels <- is.element(y, labels)
        tagged_no_label <- !haslabels & tagged

        if (any(tagged_no_label)) {
            if (length(result) > 0) {
                result <- c(result, indexes[tagged_no_label])
            }
            else {
                na_indexes <- c(indexes[tagged_no_label], na_indexes)
            }

            indexes <- indexes[!tagged_no_label]
            x <- x[!tagged_no_label]
            y <- y[!tagged_no_label]
            xmis <- xmis[!tagged_no_label]
            tagged <- tagged[!tagged_no_label]
        }
    }

    if (na_value == "na") {
        length(na_indexes) <- 0
    }
    
    z <- y[xmis | tagged]
    if (according_to == "labels") {
        haslabels <- is.element(z, labels)
        z[haslabels] <- names(labels)[match(z[haslabels], labels)]
    }
    if (user_na == "first") {
        result <- c(result, indexes[xmis | tagged][order(z, decreasing = decreasing)])
    }
    else if (user_na == "last") {
        na_indexes <- c(indexes[xmis | tagged][order(z, decreasing = decreasing)], na_indexes)
    }
    
    if (user_na != "ignore") {
        indexes <- indexes[!xmis & !tagged]
        x <- x[!xmis & !tagged]
        y <- y[!xmis & !tagged]
    }
    

    if (according_to == "labels") {
        haslabels <- is.element(y, labels)
        y[haslabels] <- names(labels)[match(y[haslabels], labels)]
        result <- c(result, indexes[order(y, decreasing = decreasing)])
    }
    else {
        result <- c(result, indexes[order(y, decreasing = decreasing)])
    }

    result <- c(result, na_indexes)
    return(result)

    attributes(result) <- attrx
    return(result)
}



`sort_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE,
    user_na = c("last", "first", "ignore", "na"), na_value = c("last", "first", "na"), ...) {

    # vec_sort() in package vctrs is somewhat similar, but still does not
    # differentiate between (hence does not sort) different tagged_na values

    return(x[order_labelled(x, according_to = according_to, decreasing = decreasing, user_na = user_na, na_value = na_value)])
}



`names_values` <- function(x) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    if (inherits(x, "mixed_labelled")) {
        x <- unmix(x)
    }
    
    labels <- attr(x, "labels", exact = TRUE)
    ltagged <- logical(length(labels))
    if (is.double(x)) {
        ltagged <- has_tag(labels)
    }

    tagged_labels <- labels[ltagged]
    labels <- labels[!ltagged]
    
    utag <- c()
    tagged <- logical(length(x))
    if (is.double(x)) {
        tagged <- has_tag(x)
    }
    if (any(tagged)) {
        utag <- sort(unique(na_tag(x[tagged])))
        x <- x[!tagged]
    }

    numtag <- c()
    if (length(utag) > 0) {
        numtag <- tag_na(utag)
        labtag <- c()

        if (length(tagged_labels) > 0) {
            labtag <- na_tag(tagged_labels)
        }

        # names(numtag) <- paste0("NA(", utag, ")")
        names(numtag) <- paste0(".", utag)
    
        for (i in seq(length(utag))) {
            if (any(isel <- labtag == utag[i])) {
                # only one can be true, impossible more
                names(numtag)[i] <- names(tagged_labels)[isel]
            }
        }
    }

    x <- x[!duplicated(x)]
    xmis <- logical(length(x))

    na_values <- attr(x, "na_values", exact = TRUE)
    na_range <- attr(x, "na_range", exact = TRUE)

    attrx <- attributes(x)
    attributes(x) <- NULL

    if (!is.null(na_values)) {
        xmis <- xmis | is.element(x, na_values)
    }
    
    if (!is.null(na_range)) {
        xmis <- xmis | (x >= na_range[1] & x <= na_range[2])
    }

    # TO DO: sort_labelled()...?!
    
    xnotmis <- sort(x[!xmis])
    xmis <- sort(x[xmis])
    
    if (length(xmis) > 0) {
        names(xmis) <- xmis
        for (i in seq(length(xmis))) {
            if (any(isel <- labels == xmis[i])) {
                names(xmis)[i] <- names(labels)[isel]
            }
        }
    }


    names(xnotmis) <- xnotmis
    if (length(xnotmis) > 0) {
        for (i in seq(length(xnotmis))) {
            if (any(isel <- labels == xnotmis[i])) {
                names(xnotmis)[i] <- names(labels)[isel]
            }
        }
    }

    result <- c(xnotmis, xmis, numtag)
    attr(result, 'missing') <- c(xmis, numtag)

    return(result)
}



`to_labels` <- function(x) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    if (inherits(x, "mixed_labelled")) {
        x <- unmix(x)
    }

    tagged <- logical(length(x))
    if (is.double(x)) {
        tagged <- has_tag(x)
    }
    
    labels <- names_values(x)
    
    attributes(x) <- NULL
    result <- x

    ltagged <- logical(length(labels))
    if (is.double(x)) {
        ltagged <- has_tag(labels)
    }

    
    if (any(ltagged)) {
        labels[ltagged] <- na_tag(labels[ltagged])
    }

    if (any(tagged)) {
        result[tagged] <- na_tag(x[tagged])
    }
    
    result[is.element(result, labels)] <- names(labels)[match(result[is.element(result, labels)], labels)]
    
    return(result)
}




`order_tagged` <- function(x, na.last = TRUE, decreasing = FALSE, method = c("auto", "shell", "radix"),
    na_values.last = TRUE) {
        
    method <- match.arg(method)
    
    tagged <- logical(length(x))
    if (is.double(x)) {
        tagged <- has_tag(x)
    }

    ix <- seq_along(x)

    truena <- ix[is.na(x) & !tagged]
    itagged <- c()

    if (is_mixed(x)) {
        x <- unmix(x)
    }
    attributes(x) <- NULL

    if (any(tagged)) {
        itagged <- ix[tagged][order(x[tagged], decreasing = decreasing, method = method)]
    }

    ix <- ix[!(is.na(x) | tagged)]
    x <- x[!(is.na(x) | tagged)]
    

    res <- c()
    if (!na.last) {
        res <- truena
    }

    if (!na_values.last) {
        res <- c(res, itagged)
    }
    
    res <- c(res, ix[order(x, decreasing = decreasing, method = method)])
    
    if (na_values.last) {
        res <- c(res, itagged)
    }
    
    if (na.last) {
        res <- c(res, truena)
    }

    return(res)
}


`sort_tagged` <- function(x, decreasing = FALSE, na.last = TRUE,
                           na_values.last = TRUE) {
    x[order_tagged(
        x,
        decreasing = decreasing,
        na.last = na.last,
        na_values.last = na_values.last
    )]
}
