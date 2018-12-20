
# Pipeline Class ----------------------------------------------------------

#' Pipeline function
#'
#' Creates a pipeline object to save data transformation logic. The expression
#' is either a named function or user defined function
#'
#' @param expr pipeline function. default is identity function
#' @param desc optional description input
#' @param uid A character string used to uniquely identify the pipeline
#'
#' @export
#' @return pipeline object with expression stored
#' 
#' @examples 
#'  
#' # basic pipeline
#' pipeline()
pipeline <- function(expr = identity,
                     desc = "",
                     uid = random_string("pipe")) {
  
  checkmate::assert_function(expr)
  checkmate::assert_character(desc)
  checkmate::assert_character(uid)
 
  structure(
    list(
      expr = expr,
      output = NULL,
      desc = desc,
      uid = uid,
      created_on  = Sys.time(),
      runtime = NULL
    ),
    class = "pipeline"
  )
}


#' @rdname print
#' @export
print.pipeline <- function(pipe, ...) {
  cat("Pipeline:", pipe$uid, "---------------- \n\n")
  cat("Description:\n", pipe$desc, "\n\n")
  cat("Expression:\n")
  print(pipe$expr)
  cat("\nCreated at:", as.character(pipe$created_on), "\n")
  cat("Runtime:", ifelse(is.null(pipe$runtime),
                         "< not run yet > \n\n",
                         paste(round(pipe$runtime, 2), "seconds\n\n")))
  cat("Sample Output:\n")
  print(head(pipe$output))
}



#' Pipeline Execute function
#'
#' Execute function executes the pipeline expression on the input and stores in
#' output location
#'
#' @param x input object to apply pipeline expression on. 
#' @param pipe pipeline object
#' @param ... additional arguments to pass to pipeline expression
#'
#' @export
#' @return updated pipeline object with output
#' @examples
#'
#' pipe <- pipeline(expr = function(e) mean(e$mpg))
#' execute(mtcars, pipe)
execute <- function(x, pipe, ...){
  UseMethod("execute")
}


#' @rdname execute
#' @export
execute.data.frame <- function(x, pipe, ...){
  checkmate::assert_class(pipe, "pipeline")
  a1 <- Sys.time()
  pipe$output <- pipe$expr(x, ...)
  a2 <- Sys.time()
  pipe$runtime <- as.numeric(a2 - a1)
  pipe
}



#' @rdname execute
#' @export
execute.NULL <- function(x, pipe, ...){
  checkmate::assert_class(pipe, "pipeline")
  a1 <- Sys.time()
  pipe$output <- NULL
  a2 <- Sys.time()
  pipe$runtime <- as.numeric(a2 - a1)
  pipe
}



#' Pipeline Flow function
#'
#' Flow function executes the pipeline expression on the input and returns the
#' data output only
#'
#' @inheritParams execute
#'
#' @export
#' @return data output resulting from pipeline execution
#' @examples
#'
#' pipe <- pipeline(expr = function(e) mean(e$mpg))
#' flow(mtcars, pipe)
flow <- function(x, pipe, ...){
  checkmate::assert_class(pipe, "pipeline")
  pipe$expr(x, ...)
}
