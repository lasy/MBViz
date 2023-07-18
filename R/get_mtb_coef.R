
#' Retrieve the coefficients of associations between the explanatory and response variables
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`) (the input of `ade4::randboot`)
#' @param boot (optional) the output of `ade4::randboot` to display the confidence intervals
#' @param Y_var (optional) a `integer` or a `character` specifying the indices
#' or names of selected response variables if one desires to display the
#' coefficients for a subset of variables.
#' @param CI (optional) The width of the confidence interval that should be displayed. Default is 0.95.
#' @param as_matrix (optional) a `logical` specifying if the coefficients have to be returned in matrix form (n_input x n_output)
#'
#' @return a `tibble`
#' @export
#' @importFrom dplyr tibble as_tibble mutate row_number group_by summarize left_join join_by arrange n
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_wrap
#' @importFrom stats quantile
#' @importFrom forcats fct_rev
#' @importFrom purrr map list_rbind
get_mtb_coef <- function(res, boot = NULL, Y_var = NULL, CI = 0.95, as_matrix = FALSE) {

  input_var <- blocks_and_variables(res)
  all_Y_vars <- names(res$XYcoef)
  if (is.null(Y_var)) Yvars <- all_Y_vars
  else if (all(is.character(Y_var))) {
    if (all(Y_var %in% all_Y_vars))  Yvars <- Y_var
    else stop("Y_var must only include variables in the Y block\n")
  }
  else if (all(as.integer(Y_var) == Y_var)) {
    if (all(Y_var %in% 1:length(all_Y_vars))) Yvars <- all_Y_vars[Y_var]
    else stop("Y_var indices must be in 1:ncol(Y)\n")
  } else stop("Y_var must be a character or integer (vector).")

  nf <- retrieve_nf(res, boot)

  coefs <-
    purrr::map(
      .x = Yvars,
      .f = function(Yvar, res) {
        tibble(
          variable = rownames(res$XYcoef[[Yvar]]),
          value = res$XYcoef[[Yvar]][, nf],
          Yvar = Yvar
        )
      },
      res = res
    ) %>%
    purrr::list_rbind() %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable),
      Yvar = Yvar %>% factor(., levels = all_Y_vars)
    ) %>%
    left_join(input_var, by = join_by(variable))

  if (!is.null(boot)) {
    if ("boot_mbplsda" %in% class(boot)) {
      boot_summary <-
        purrr::map(
          .x = Yvars,
          .f = function(Yvar, boot){
            boot$XYcoef[[Yvar]] %>%
              dplyr::select(variables, block, Q2.5, Q97.5) %>%
              dplyr::rename(variable = variables, lo = Q2.5, up = Q97.5) %>%
              dplyr::mutate(
                variable = variable %>% factor(., levels = input_var$variable),
                block = block %>% factor(., levels = input_var$block %>% levels()),
                Yvar = Yvar
              )
          },
          boot = boot
        ) %>%
        purrr::list_rbind()

    } else {

      boot_res <-
        purrr::map_dfr(
          .x = Yvars,
          .f = function(Yvar, boot) {
            boot$XYcoef[[Yvar]]$boot %>%
              as_tibble() %>%
              mutate(i = row_number()) %>%
              pivot_longer(
                cols = -i,
                names_to = "variable",
                values_to = "value"
              ) %>%
              mutate(Yvar = Yvar)
          },
          boot = boot
        ) %>%
        mutate(
          variable = variable %>% factor(., levels = input_var$variable),
          Yvar = Yvar %>% factor(., levels = all_Y_vars)
        )

      boot_summary <-
        boot_res %>%
        group_by(variable, Yvar) %>%
        summarize(
          lo = quantile(value, p = (1-CI)/2),
          up = quantile(value, p = CI + (1-CI)/2),
          p = abs(sum(value > 0) - sum(value < 0))/n(),
          .groups = "drop"
        ) %>%
        left_join(., input_var %>% select(variable, block), by = "variable")
    }
    coefs <- coefs %>% left_join(boot_summary, by = join_by(variable, block, Yvar))
  }

  coefs <-
    coefs %>%
    dplyr::mutate(Yvar = Yvar %>% factor(., levels = all_Y_vars))

  if (as_matrix) {
    coefs <-
      coefs %>%
      tidyr::pivot_wider(id_cols = variable, names_from = Yvar, values_from = value) %>%
      as.data.frame() %>%
      magrittr::set_rownames(unique(coefs$variable)) %>%
      dplyr::select(-variable) %>%
      as.matrix()
  }

  coefs
}
