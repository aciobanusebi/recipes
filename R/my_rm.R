step_my_rm <- function(recipe,
                    ...,
                    role = NA,
                    trained = FALSE,
                    removals = NULL,
                    myFilter = function(x){colnames(x)},
                    skip = FALSE,
                    id = rand_id("my_rm")) {
  add_step(recipe,
           step_my_rm_new(
             terms = ellipse_check(...),
             role = role,
             trained = trained,
             removals = removals,
             myFilter = myFilter,
             skip = skip,
             id = id
           ))
}

step_my_rm_new <- function(terms, role, trained, removals, myFilter, skip, id) {
  step(
    subclass = "rm",
    terms = terms,
    role = role,
    trained = trained,
    removals = removals,
    myFilter = myFilter,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_my_rm <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_my_rm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = x$myFilter(training[,col_names]),
    myFilter = x$myFilter,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_my_rm <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
}

print.step_my_rm <-
  function(x, width = max(20, options()$width - 22), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Variables removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else
        cat("No variables were removed")
    } else {
      cat("Delete terms ", sep = "")
      cat(format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


#' @rdname step_my_rm
#' @param x A `step_my_rm` object.
#' @export
tidy.step_my_rm <- tidy_filter