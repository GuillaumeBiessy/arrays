#' Quick Array Creation
#'
#' Create an Array without providing the dim argument
#'
#' @param data The array data
#' @param dimnames The array dimension names
#'
#' @return An array
#' @export
aMake <- function(data, dimnames) {

  map(dimnames, length) %>% array(data = data, dimnames = dimnames)
}

#' Array Sum over Dimensions
#'
#' Return an array summed over one or several dimensions
#'
#' @param x An array, a vector
#' @param d Numeric or character vector. The dimensions to be considered.
#' @param keep Boolean. Should those dimensions be kept or dropped in the final
#'   result. By default the sum is made over all dimensions that are not
#'   explicitely named.
#' @param na.rm Should NA value be removed ? Default : FALSE
#'
#' @return An array or vector containing the sum over the undesired dimensions
#' @export
aSums <- function(x, d, keep = T, na.rm = FALSE) {

  stopifnot(is.numeric(d) || is.character(d))

  if (!is.array(x)) {
    if (is.character(d)) d <- rep(1, length(d))
    stopifnot(length(d) <= 1)
    if (length(d) == keep) return(x)
    return(sum(x, na.rm = na.rm))
  }

  stopifnot(!is.null(names(dim(x))))

  if (is.character(d)) {
    ndims <- names(dim(x))
    stopifnot(all(d %in% ndims))
    d <- match(d, ndims)
  }

  stopifnot(all(d <= length(dim(x))))

  dims <- seq_along(dim(x))
  cdims <- setdiff(dims, d)

  head_d <- if(keep) cdims else d
  tail_d <- if(!keep) cdims else d

  if(length(head_d) == 0) return(x)
  if(length(tail_d) == 0) return(sum(x))

  x <- aperm(x, c(head_d, tail_d))

  return(colSums(x, na.rm = na.rm, dims = length(head_d)))
}

#' Array Slice Insertion
#'
#' Slice an array and modify values from a subset over one of the dimensions
#'
#' @param x An array, a vector
#' @param d Numeric or character. The dimension to subset.
#' @param values Numeric or character vector. The values to subset.
#' @param index Should numeric values be treated as indexes ?
#' @param replacement Replacement for selected subset
#'
#' @return An array or vector with modified subset over one dimension.
#' @export
aInsert <- function(x, d, values, replacement, index = F) {

  stopifnot(is.numeric(d) || is.character(d), length(d) == 1)

  if (!is.array(x)) {
    stopifnot(is.character(d) || d == 1)
    return(x[values])
  }

  nd <- length(dim(x))
  if (is.character(d)) {
    ndims <- names(dim(x))
    stopifnot(all(d %in% ndims))
    d <- match(d, ndims)
  }

  if (!index) values <- as.character(values)
  indices <- rep(list(rlang::missing_arg()), nd)
  indices[[d]] <- values

  eval(rlang::expr(x[!!!indices] <- replacement))

  return(x)
}

#' Array Slicing
#'
#' Slice an array, returning a subset over one of the dimensions
#'
#' @param x An array, a vector
#' @param d Numeric or character. The dimension to subset.
#' @param values Numeric or character vector. The values to subset.
#' @param index Should numeric values be treated as indexes ?
#'
#' @return An array or vector subset over one dimension.
#' @export
aSlice <- function(x, d, values, index = F) {

  stopifnot(is.numeric(d) || is.character(d), length(d) == 1)

  if (!is.array(x)) {
    stopifnot(is.character(d) || d == 1)
    return(x[values])
  }

  nd <- length(dim(x))
  if (is.character(d) ) {
    ndims <- names(dim(x))
    stopifnot(all(d %in% ndims))
    d <- match(d, ndims)
  }

  if (!index) values <- as.character(values)
  indices <- rep(list(rlang::missing_arg()), nd)
  indices[[d]] <- values

  return(eval(rlang::expr(x[!!!indices])))
}

#' Array Transformation
#'
#' Transform an array, keeping the desired indices and dimensions
#'
#' @param x A vector or an array
#' @param l A list whose components are named after some of the array dimensions
#'   and contain the indices to keep
#' @param keep Boolean. Should the dimensions that are not in the list be summed
#'   over ?
#' @param reorder Boolean. Should the array dimensions be permuted so that they
#'   match the order of the list ?
#'
#' @return An array containing only the specified components
#' @export
aMorph <- function(x, l, keep = T, reorder = !keep) {

  if (!is.array(x)) {

    stopifnot(length(l) == 1)
    if (is.null(names(x))) {
      x <- set_names(x, seq_along(x))
    }
    new_x <- aMake(data = 0, dimnames = l)
    new_x[match(l[[1]], names(x))] <- x[match(l[[1]], names(x))]
    return(x)
  }

  nl <- names(l)
  ndims <- names(dim(x))
  ndimnames <- dimnames(x)

  stopifnot(!is.null(nl), !is.null(ndims), all(nl %in% ndims))

  for (d in ndims) {

    if (!d %in% nl) next

    olddimnames <- newdimnames <- ndimnames
    newdimnames[[match(d, ndims)]] <- l[[match(d, nl)]]
    commonnames <- intersect(ndimnames[[d]], l[[d]])
    x <- aMake(data = 0, dimnames = newdimnames) %>%
      aInsert(d, commonnames, aSlice(x, d, commonnames))
    ndimnames <- newdimnames
  }

  if (!keep) x <- aSums(x, nl)
  if (reorder && is.array(x)) {

    d <- match(names(dim(x))[names(dim(x) %in% nl)], nl)
    dims <- seq_along(dim(x))
    cdims <- setdiff(dims, d)
    x <- aperm(x, c(d, cdims))
  }

  return(x)
}

#' Array Mergeing
#'
#' Merge a list of arrays
#'
#' @param l A list of arrays
#' @param varname The name to be given to the variable that contains the array
#'   source
#' @param fdims The dimensions for the final array, minus the array source. In
#'   case this argument is not provided, summation is performed over dimensions
#'   not present in at least one array, then all indices present in at least one
#'   array are included.
#'
#' @return An agregated array
#' @export
aMerge <- function(l, varname = "Source", fdims = NULL) {

  commondims <- l %>% map(dimnames) %>% map(names) %>% reduce(intersect)
  fdims <- fdims %||% l %>% map(dimnames) %>% map(~.x[commondims]) %>% (purrr::transpose) %>% map(reduce, union)

  out <- l %>% map(aMorph, fdims, keep = FALSE) %>% unlist %>%
    aMake(dimnames = c(fdims, l %>% names %>% list %>% set_names(varname)))
  return(out)
}

#' Data.frame to array conversion
#'
#' Convert a data.frame object to an array
#'
#' @param data A data.frame.
#' @param covariates Character vector. The covariates to use for array
#'   dimensions. Unselected dimensions will be collapsed.
#' @param value.var Character. The name of the column which contains the value
#'   for the array.
#' @param fill Numeric. The value to use when the combination of covariates does
#'   not exist in the data.frame
#'
#' @return An array whose dimensions correspond to the selected covariates.
#' @export
df_to_array <- function(data, covariates, value.var, fill = 0) {

  f <- covariates %>% paste(collapse = "+") %>% paste("~ .")
  df <- data.table::dcast(
    data, f, fun.aggregate = sum, value.var = value.var, drop = F, fill = fill)
  data.table::setkeyv(df, rev(covariates))

  names_A <- map(df[,- length(df), with = F], unique)
  A <- aMake(data = df$., dimnames = names_A)
  return(A)
}
