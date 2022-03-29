#' Make a variable chatty
#'
#' Call `chatty(x, y)` to make the variables `x` and `y` chatty. A message
#' will then be printed every time they are accessed or modified. If you set
#' deep to true, when the variable is given directly as an argument to a function
#' (as in `fun(x)` but not `fun(x + 1)` the argument will become chatty again.
#'
#' @param ... variables to make chatty
#' @param f function to used on modified values before printing them, by default
#'   the full modified object is printed
#' @param silent.access Whether to make access silent
#' @param silent.modification Whether to make modifications silent
#' @param deep Whether to make function arguments chatty when function are passed
#'   a chatty variable as an input
#'
#' @return Returns `NULL` invisibly, called for side effects.
#' @export
chatty <- function(..., f = identity, silent.access = FALSE, silent.modification = FALSE, deep = FALSE) {
  f_sym <- substitute(f)
  if (is.function(f)) {
    f_name <- as.character(f_sym)
    f <- list(f)
    if (is.symbol(f_sym) && ! f_name %in% c("", "identity")) {
      names(f) <- f_name
    }
  }
  if (is.null(names(f))) names(f) <- rep("", length(f))
  f <- lapply(f, rlang::as_function)
  vars <- as.character(eval(substitute(alist(...))))
  for (var in vars) {
    chatty_impl(var, f, silent.access, silent.modification, deep, parent.frame(), desc = NULL)
  }
  invisible(NULL)
}

chatty_impl <- function(x, f = identity, silent.access, silent.modification, deep, caller_env, desc, parent_fun) {
  closure <- new.env(parent = caller_env)
  closure$definition_env <- caller_env
  closure$f <- f
  closure$var_name <- x
  closure$var_desc <-  paste0("`", x, "`")
  if(!is.null(desc)) closure$var_desc <- paste0(desc, " -> ", closure$var_desc, " through `", parent_fun, "()`")
  if(exists(x, caller_env, inherits = FALSE)) {
    closure$value <- get(x, caller_env)
    rm(list = x, envir = caller_env)
  }
  closure$silent.access <- silent.access
  closure$silent.modification <- silent.modification
  makeActiveBinding(
    closure$var_name,
    function(v) {
      var_name <- closure$var_name
      caller_env <- sys.frame(-1)
      sc <- sys.calls()
      if(length(sc) > 1) {
        call_msg <- capture.output(sc[[length(sc)]])
        if(call_msg[[1]] == "(function(v) {") {
          call_msg <- capture.output(sc[[length(sc) - 1]])
        }
        call_msg <- paste(call_msg, collapse = "\n")
      } else {
        call_msg <- NULL
      }

      last_source_pos <- tail(Position(function(x) is.call(x) && identical(x[[1]], quote(source)), sc), 1)
      if(!is.na(last_source_pos)) {
        sourced_path <- eval(sc[[c(last_source_pos, 2)]], sys.frame(last_source_pos))
        message(sourced_path)
      }

      if(deep && !identical(closure$definition_env, caller_env)) {
        # the function call that featured the chatty variable
        parent_call <- sys.call(-1)
        if(!is.null(parent_call)) {

          parent_fun <- deparse1(parent_call[[1]])
          # exception for print or we get it every time
          if (parent_fun != "print") {
            # the complete call, with named arguments
            parent_call_matched <- match.call(eval(parent_call[[1]], caller_env), parent_call)
            args <- as.list(parent_call_matched)[-1]
            new_name <- names(Filter(function(x) identical(x, as.name(var_name)), args))
            # convert promise to regular binding
            assign(new_name, closure$value, envir = caller_env)
            # convert regular binding to active
            chatty_impl(new_name, f, silent.access, silent.modification, deep, caller_env, closure$var_desc, parent_fun = parent_fun)
            # not DRY
            closure$var_desc <- sprintf("%s -> `%s` through `%s()`", closure$var_desc, new_name, parent_fun)
          }
        }
      }

      if(missing(v)) {
        if (!closure$silent.access) {
          msg <- paste0("access: ", closure$var_desc)
          message(msg)
          if(!is.null(call_msg)) message(call_msg)
        }
      } else {

        if (!closure$silent.modification) {
          msg <- paste0("modification: ", closure$var_desc)
          message(msg)
          if(!is.null(call_msg)) message(call_msg)
        }

        closure$value <- v
        var_name <- var_name
        f <- closure$f
        f_names <- names(f)

        for (i in seq_along(f)) {
          f_name <- f_names[[i]]
          msg <- if (f_name != "") {
            sprintf("%s(%s):", f_name, var_name)
          }
          if (!closure$silent.modification) {
            print(f[[i]](closure$value))
          }
        }
      }

      cat("\n")
      closure$value
    },
    env = caller_env)
}
