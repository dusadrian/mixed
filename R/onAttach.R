.onAttach <- function(...) {
    to_load <- c("vctrs", "haven")
    
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

    env <- asNamespace("haven")
    if (.Call("unlockEnvironment", env, PACKAGE = "mixed")) {
        
        do.call("unlockBinding", list(sym = "format_tagged_na", env = env))

        env$format_tagged_na <- function(x, digits = getOption("digits")) {
            tagged <- logical(length(x))
            if (is.double(x)) {
                tagged <- mixed::has_tag(x)
            }

            out <- format(vec_data(x), digits = digits)
            if (any(tagged)) {
                out[tagged] <- paste0(".", mixed::get_tag(x[tagged]))
            }
            
            format(out, justify = "right")
        }

        env <- as.environment("package:haven")

        do.call("unlockBinding", list(sym = "tagged_na", env = env))
        env$tagged_na <- function(...) {
            mixed::tag_na(...)
        }

        do.call("unlockBinding", list(sym = "is_tagged_na", env = env))
        env$is_tagged_na <- function(x, tag = NULL) {
            mixed::has_tag(x = x, tag = tag)
        }

        do.call("unlockBinding", list(sym = "na_tag", env = env))
        env$na_tag <- function(x) {
            mixed::get_tag(x)
        }

        do.call("unlockBinding", list(sym = "labelled", env = env))

        env$labelled <- function(x = double(), labels = NULL, label = NULL, ...) {
            x <- vec_data(x)
            
            oa <- list(...)
            
            if (any(is.element(c("na_values", "na_range"), names(oa)))) {
                mixed_labelled(
                    x = x,
                    labels = labels,
                    na_values = oa$na_values,
                    na_range = oa$na_range,
                    label = label
                )
            }
            else {
                labels <- stats::setNames(vec_cast(labels, x, x_arg = "labels", to_arg = "x"), names(labels))
                validate_labelled(new_labelled(x, labels = labels, label = label))
            }
        }

        # labelled <- !grepl("there is no package called", tryCatch(find.package("labelled"), error = function(e) e))

        
    }


    return(invisible())

}
