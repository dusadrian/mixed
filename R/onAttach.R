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
            out <- format(vec_data(x), digits = digits)
            out[is_tagged_na(x)] <- paste0(".", na_tag(x)[is_tagged_na(x)])
            format(out, justify = "right")
        }

        env <- as.environment("package:haven")
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

        # # this is necessary, to convert mixed_labelled objects back into labelled_spss
        # do.call("unlockBinding", list(sym = "write_sav", env = env))

        # env$write_sav <- function(data, path, compress = FALSE) {
        #     data <- validate_sav(unmix(data))
        #     write_sav_(data, normalizePath(path, mustWork = FALSE), compress = compress)
        #     invisible(data)
        # }

        
    }


    return(invisible())

}
