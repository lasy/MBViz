
#' Retrieve and format the observed (and bootstrapped) cumulative Variable Importance in Projection (VIPC)
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`)
#' @param boot (optional) the output of `ade4::randboot`
#' @param CI (optional) The width of the confidence interval that should be displayed. Default is 0.95.
#'
#' @return a `tibble` object
#' @export
#'
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate left_join join_by arrange
#' @importFrom stringr str_wrap
get_mtb_vipc <- function(res, boot = NULL, CI = 0.95) {

  input_var <- blocks_and_variables(res)
  nf <- retrieve_nf(res = res, boot = boot)

  vipc <-
    tibble(variable = rownames(res$vipc), value = res$vipc[,nf]) %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable %>% levels())
    ) %>%
    left_join(input_var, by = join_by(variable))

  if (!is.null(boot)) {
    if ("boot_mbplsda" %in% class(boot)) {
      vipc <-
        vipc %>%
        left_join(
          boot$vipc %>%
            dplyr::select(variables, block, Q2.5, Q97.5) %>%
            dplyr::rename(variable = variables, lo = Q2.5, up = Q97.5) %>%
            dplyr::mutate(
              variable = variable %>% factor(., levels = input_var$variable),
              block = block %>% factor(., levels = input_var$block %>% levels())
            ),
          by = join_by(variable, block)
        )
    } else {

      bootstraps_vipc <-
        boot$vipc$boot %>%
        t() %>%
        set_colnames(1:nrow(boot$vipc$boot)) %>%
        as_tibble() %>%
        mutate(
          variable =
            colnames(boot$vipc$boot) %>%
            factor(., levels = input_var$variable %>% levels())
        ) %>%
        pivot_longer(
          cols = -variable,
          names_to = "b",
          values_to = "value"
        )

      bootstraps_vipc_summary <-
        bootstraps_vipc %>%
        group_by(variable) %>%
        summarize(
          lo = quantile(value, p = (1-CI)/2),
          up = quantile(value, p = CI + (1-CI)/2),
          .groups = "drop"
        )

      vipc <- vipc %>% left_join(bootstraps_vipc_summary, by = join_by(variable))
    }
  } else {
    vipc <- vipc %>% mutate(lo = NA, up = NA)
  }

  vipc
}
