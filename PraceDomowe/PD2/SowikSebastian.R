l = list(a=c(1,2,3), b=c(7,9,8))
min(l[['b']])

homework <- function(fun_not_nse) {
  ret <- function(nse_arg, env_var) {
    fun_not_nse(eval(substitute(nse_arg), env_var))
  }
}

min_nse <- homework(min)
min_nse(b, l)


ret <- function(nse_arg, env_var) {
  min(eval(quote(nse_arg), env_var))
}
ret(l, a)

get_element_nse <- function(input_list, element_name) {
  eval(substitute(element_name), input_list)
}
get_element_nse(l, a)


homework2 <- function(fun_not_nse) {
  ret <- function(nse_arg, env_var=parent.frame()) {
    browser()
    fun_not_nse(eval(quote(nse_arg), env_var))
  }
}

min_nse2 <- homework2(min) 
l2 <- list(a=c(1,2,3,4))
min_nse2(a, l2)
min_nse2(c(1,2,3,4))


min_nse3 <- function(data, ...) {
  group_vars <- quote(list(...))
  browser()
  min(eval(substitute(group_vars,data)))
}


grouped_mean2 <- function(.data, .summary_var, ...) {
  summary_var <- enquo(.summary_var)
  group_vars <- enquos(...)
  
  .data %>%
    group_by(!!!group_vars) %>%
    summarise(mean = mean(!!summary_var))
}
