.onAttach <- function(...) {
    to_load <- c("vctrs", "haven", "labelled")
    
    # code borrowed from package tidyverse
    
    # Attach the package from the same package library it was
    # loaded from before. https://github.com/tidyverse/tidyverse/issues/171
    load_library <- function(pkg) {
        if (pkg %in% loadedNamespaces()) {
            loc <- dirname(getNamespaceInfo(pkg, "path"))
            do.call(
                "library",
                list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
            )
        }
    }
    
    not_yet_loaded <- to_load[!is.element(paste0("package:", to_load), search())]

    if (length(not_yet_loaded) > 0) {

        packageStartupMessage(
            paste(sprintf("Also attaching package%s:", ifelse(length(not_yet_loaded) > 1, "s", "")), paste(not_yet_loaded, collapse = ", "))
        )

        suppressPackageStartupMessages(
            lapply(not_yet_loaded, load_library)
        )
    }

    if (.Call("_unlockEnvironment", asNamespace("base"), PACKAGE = "mixed")) {

        env <- as.environment("package:base")
        do.call("unlockBinding", list(sym = "c", env = env))
        
        env$c <- function(..., recursive = FALSE, use.names = TRUE) {
            dots <- list(...)
            
            atomic <- unlist(lapply(dots, is.atomic))
            
            if (all(atomic)) {
                mixed <- unlist(lapply(dots, is_mixed))
                if (any(mixed)) {
                    return(c_mixed_labelled(dots))
                }
                else {
                    tagged <- lapply(dots, has_tag)
                    if (any(unlist(tagged))) {
                        ltagged <- unlist(lapply(tagged, any))
                        tag_values <- sort(unique(unlist(lapply(seq(length(dots)), function(x) {
                            if (ltagged[x]) {
                                return(untag(dots[[x]][tagged[[x]]]))
                            }
                        }))))
 
                        if (any(is.element(unlist(dots[!ltagged]), tag_values))) {
                            do.call("cat", list("\n"))
                            do.call("stop", list("Tagged and untagged values are overlapped.\n\n", call. = FALSE))
                        }

                        tags <- tag(tag_values)
                        nms <- attr(unlist(dots), "names")
                        
                        dots[ltagged] <- lapply(dots[ltagged], function(x) {
                            for (i in seq(length(tag_values))) {
                                x[has_tag(x, tag_values[i])] <- unclass(tags[i])
                            }
                            return(x)
                        })

                        dots <- unlist(dots)
                        attributes(dots) <- attributes(tags)
                        if (!is.null(nms)) {
                            attr(dots, "names") <- nms
                        }
                        
                        return(dots)
                    }
                    else {
                        do.call(.Primitive("c"), c(dots, list(recursive = recursive, use.names = use.names)))
                    }
                }
            }
            else {
                do.call(.Primitive("c"), c(dots, list(recursive = recursive, use.names = use.names)))
            }
        }
    }
    

    env <- asNamespace("haven")
    if (.Call("_unlockEnvironment", env, PACKAGE = "mixed")) {
        do.call("assignInNamespace", list(
            x = "format_tagged_na",
            value = function(x, digits = getOption("digits")) {
                # tagged <- has_tag(x)

                # out <- format(vec_data(x), digits = digits)
                # if (any(tagged)) {
                    
                #     tags <- lapply(x[tagged], get_tag)

                #     numtags <- unlist(lapply(tags, is.numeric))
                    
                #     if (sum(numtags) > 0) {
                #         out[which(tagged)[numtags]] <- format(vec_data(unlist(tags[numtags])), digits = digits)
                #         tagged[which(tagged)[numtags]] <- FALSE
                #     }

                #     out[tagged] <- paste0(".", get_tag(x[tagged]))
                # }

                # format(out, justify = "right")

                out <- format(vec_data(x), digits = digits)
                tagged <- has_tag(x)
                out[tagged] <- paste0("NA(", get_tag(x[tagged]), ")")

                # format again to make sure all elements have same width
                format(out, justify = "right")
            },
            ns = "haven"
        ))

        do.call("assignInNamespace", list(
            x = "obj_print_header.haven_labelled",
            value = function(x, ...) {
                if (!inherits(x, "noprint")) {
                    do.call("cat",
                        list(paste0("<", vec_ptype_full(x), "[", vec_size(x), "]>", get_labeltext(x), "\n"))
                    )
                }
                invisible(x)
            },
            ns = "haven"
        ))

        do.call("assignInNamespace", list(
            x = "obj_print_footer.haven_labelled",
            value = function(x, ...) {
                if (!inherits(x, "noprint")) {
                    if (!inherits(x, "haven_labelled_spss")) {
                        na_values <- attr(x, "na_values")
                        if (!is.null(na_values)) {
                            do.call("cat",
                                list(paste0("Missing values: ", paste(na_values, collapse = ", "), "\n"))
                            )
                        }

                        na_range <- attr(x, "na_range")
                        if (!is.null(na_range)) {
                            do.call("cat",
                                list(paste0("Missing range: [", paste(na_range, collapse = ", "), "]\n"))
                            )
                        }
                    }
        
                    print_labels(x)
                }
                invisible(x)
            },
            ns = "haven"
        ))
    }


    env <- asNamespace("labelled")
    if (.Call("_unlockEnvironment", env, PACKAGE = "mixed")) {

        do.call("assignInNamespace", list(
            x = "na_values<-.default",
            value = function(x, value) {
                if (!is.null(value)) {
                    x <- mixed_labelled(x,
                            labels = attr(x, "labels", exact = TRUE),
                            na_values = value,
                            na_range = attr(x, "na_range", exact = TRUE),
                            label = attr(x, "label", exact = TRUE)
                    )
                }
                return(x)
            },
            ns = "labelled"
        ))

        
        do.call("assignInNamespace", list(
            x = "na_values<-.haven_labelled",
            value = function(x, value) {
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
            },
            ns = "labelled"
        ))

        
        do.call("assignInNamespace", list(
            x = "na_range<-.default",
            value = function(x, value) {
                if (!is.null(value)) {
                    x <- mixed_labelled(x,
                        labels = unclass(attr(x, "labels", exact = TRUE)),
                        na_values = attr(x, "na_values", exact = TRUE),
                        na_range = value,
                        label = attr(x, "label", exact = TRUE)
                    )
                }
                return(x)
            },
            ns = "labelled"
        ))

        
        do.call("assignInNamespace", list(
            x = "na_range<-.haven_labelled",
            value = function(x, value) {
                if (is.null(value)) {
                    attr(x, "na_range") <- NULL
                    if (is.null(attr(x, "na_values", exact = TRUE))) {
                        x <- untag(x)
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
            },
            ns = "labelled"
        ))
        
        env <- as.environment("package:labelled")

        do.call("unlockBinding", list(sym = "tagged_na", env = env))
        env$tagged_na <- function(...) {
            tag(...)
        }

        do.call("unlockBinding", list(sym = "is_tagged_na", env = env))
        env$is_tagged_na <- function(x, tag = NULL) {
            has_tag(x = x, tag = tag)
        }

        do.call("unlockBinding", list(sym = "na_tag", env = env))
        env$na_tag <- function(x) {
            get_tag(x)
        }

        do.call("unlockBinding", list(sym = "labelled", env = env))

        env$labelled <- function(x = double(), labels = NULL, label = NULL) {
            haven::labelled(x, vec_data(labels), label)
        }
        
    }


    return(invisible())

}
