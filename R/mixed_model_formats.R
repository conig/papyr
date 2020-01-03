#' mm_fe
#'
#' Produces fixed effect tables for lme4 objects
#'
#' @param model a lme4 object
#' @importFrom dplyr %>%
#' @export mm_fe

mm_fe = function(model){
  fixed_table = stats::coef(summary(model)) %>%
    data.frame %>%
    tibble::rownames_to_column() %>%
    dplyr::left_join(stats::confint(model, method = "Wald") %>%
                       data.frame() %>%
                       tibble::rownames_to_column(),
                     by = "rowname")

  fixed_table = fixed_table %>%
    dplyr::rename(Predictors = rowname,
                  b = Estimate,
                  SE = Std..Error,
                  p = Pr...z..,
                  lower = X2.5..,
                  upper = X97.5..)
  return(fixed_table)
}

#' mm_re
#'
#' Produces random effect table for lme4 objects
#' @param model a lme4 object
#' @param simple_names If true, latex commands are stipped
#' @param sigma a Bool. If true, residual variance is included
#' @importFrom dplyr %>%
#' @export mm_re

mm_re = function(model, simple_names = T, sigma = F){

  var_summary = insight::get_variance(model)
  tau_const = var_summary$var.intercept %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(type = "tau.00")

  tau_slope = var_summary$var.slope %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(type = "tau.11")

  cor.slope_int = var_summary$cor.slope_intercept %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(type = "p.01")

  re_vars = tibble::tibble(rowname = "_sigma", . = var_summary$var.residual, type = "sigma2" )

  #ICC ----
  iccs = performance::icc(model)$ICC_adjusted
  iccs = tibble::tibble(rowname = "ICC","." = iccs, type = "ICC")

  re_vars = dplyr::bind_rows(re_vars, tau_const, tau_slope, cor.slope_int, iccs)

  if(!simple_names){
    sigma_name = "$\\sigma^2$"
    tau_name = "$\\tau_{00}$ "
    tau_s_name = "$\\tau_{11}$ "
    p_name = "$\\rho_{01}$ "
    icc_name = "$ICC$"
  }else{
    sigma_name = "sigma2"
    tau_name = "tau_00 "
    tau_s_name = "tau_11 "
    p_name = "rho_01 "
    icc_name = "ICC"
  }

  re_vars$rowname[re_vars$type == "sigma2"] = sigma_name
  re_vars$rowname[re_vars$type == "tau.00"] = paste0(tau_name,
                                                     re_vars$rowname[re_vars$type == "tau.00"])
  re_vars$rowname[re_vars$type == "tau.11"] = paste0(tau_s_name,
                                                     re_vars$rowname[re_vars$type == "tau.11"])
  re_vars$rowname[re_vars$type == "p.01"] = paste0(p_name,
                                                     re_vars$rowname[re_vars$type == "p.01"])

  re_vars$rowname[re_vars$type == "ICC"] = icc_name

  if(!sigma){ #if not sigma, suppress.
    re_vars = re_vars %>%
      dplyr::filter(type != "sigma2")
  }

  random_effects = re_vars %>%
    dplyr::select(Effect = rowname, est = ".")
  # %>%
  #   dplyr::select(Predictors = rowname, "$\\beta$" = ".") %>%
  #   purrr::modify_if(is.numeric,function(x) as.character(digits(x,round)))
  #
  return(random_effects)
}


#' mm_table
#'
#' Creates a table of fixed and random effects
#'
#' @param model a model of glmerMod
#' @param transf function to transform estimates
#' @param transf_name string, used to rename 95\% CI column
#' @param fixed_names a vector of predictor names
#' @param round a scalar, defaults to 2
#' @param round_p a scalar. The number of digits to round p to.
#' @param simple_names a bool. If True, simple names are given
#' @param collapse a string. Value to separate confidence intervals with
#' @param brackets a vector. passed to glue, bracket.
#' @importFrom dplyr %>% left_join bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom purrr modify_if
#' @export mm_table

#round = 2; round_p = 3; fixed_names = NULL; simple_names = F; collapse = " - "; brackets = c("(",")"); transf = NULL, transf_name = NULL

mm_table = function(model,
                    transf = NULL,
                    transf_name = NULL,
                    round = 2,
                    round_p = 3,
                    fixed_names = NULL,
                    #random_names = NULL,
                    simple_names = F,
                    collapse = " - ",
                    brackets = c("(", ")")) {


  # define fixed effects -------------------------------------------
  fixed_table = mm_fe(model)

  if(!is.null(transf)){ # if transformation requested, transform confidence interval
    fixed_table$lower = transf(fixed_table$lower)
    fixed_table$upper = transf(fixed_table$upper)

  }

  if (!is.null(fixed_names)) {
    fixed_table$`Predictors` = fixed_names
  }

 #define random effects ------------------------------------------------

  random_effects = mm_re(model, simple_names = simple_names) %>%
    dplyr::select(Predictors = Effect, "$\\beta$" = est) %>%
    purrr::modify_if(is.numeric,function(x) as.character(digits(x,round)))

  #round fixed table ------------------------
  rounded_fixed = fixed_table %>%
    dplyr::mutate(lower = digits(lower, round),
           upper = digits(upper, round))

  #perform roundings and formating

  if(is.null(transf)){
    rounded_fixed = rounded_fixed %>%
      dplyr::mutate(`95% CI` = glue::glue("[{lower}, {upper}]") %>% as.character())
  }else{
    rounded_fixed = rounded_fixed %>%
      dplyr::mutate(temp_estimate = digits(transf(b)),round) %>%
      dplyr::mutate(`95% CI` = glue::glue(" {temp_estimate} [{lower}, {upper}]") %>% as.character())
  }

  rounded_fixed = rounded_fixed %>%
    dplyr::mutate(b = digits(b, round),
           SE = digits(`SE`, round),
           p = round_p(p, round_p))


  final_fixed = rounded_fixed %>%
    dplyr::select("Predictors", `$\\beta$` = b, `$SE$` = SE, `95% CI`, `$p$` = p)

  if(!is.null(transf_name)){
    names(final_fixed)[names(final_fixed) == "95% CI"] = transf_name
  }

  #merged table ---------------------------------------
  table_out = final_fixed %>%
    dplyr::bind_rows(tibble::tibble(Predictors = "**Random Effects**"),
              random_effects)

  table_out[is.na(table_out)] = " "

  if(simple_names){
    table_out = table_out %>%
      dplyr::rename(beta = "$\\beta$")
  }

  return(table_out)
}


globalVariables(c(".","coef", "p","$\\beta$","SE","OR","rowname","95% CI","Estimate",
                  "Pr...t..","Pr...z..","Std..Error","X2.5..","X97.5..","b","lower","remain","upper", "Effect","est"))
