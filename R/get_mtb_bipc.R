
#' Retrieve and format the observed (and bootstrapped) cumulative Block Importance in Projections (BIPC)
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`)
#' @param boot (optional) the output of `ade4::randboot`
#' @param CI The confidence interval that should be displayed. Default is 0.95.
#' @param wrap_block_names (optional) `logical` specifying if block names (x-axis) should be wrapped. Default is `TRUE`.
#'
#' @return a `tibble` object
#' @export
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate select
#' @importFrom stats quantile
get_mtb_bipc <- function(res, boot = NULL, CI = 0.95){

  input_var <- blocks_and_variables(res)
  if ("mbplsda" %in% class(res)) {
    nf <- ifelse(is.null(boot), res$nf, length(boot$faX))
  } else {
    nf <- ifelse(is.null(boot), res$nf, which.min(abs(boot$bipc$obs[1] - res$bipc[1,])))
  }

  bipc <-
    tibble(block = rownames(res$bipc), value = res$bipc[,nf]) %>%
    mutate(
      block = block %>% factor(., levels = input_var$block %>% levels())
    ) %>%
    left_join(input_var %>% dplyr::select(-variable) %>% dplyr::distinct(), by = join_by(block))

  if (!is.null(boot)) {
    if ("boot_mbplsda" %in% class(boot)) {
      bipc <-
        bipc %>%
        left_join(
          boot$bipc %>%
            dplyr::select(blocks, Q2.5, Q97.5) %>%
            dplyr::rename(block = blocks, lo = Q2.5, up = Q97.5) %>%
            dplyr::mutate(block = block %>% factor(., levels = input_var$block %>% levels())),
          by = join_by(block)
        )
    } else {

      bootstraps_bipc <-
        boot$bipc$boot %>%
        t() %>%
        set_colnames(1:nrow(boot$bipc$boot)) %>%
        as_tibble() %>%
        mutate(
          block =
            colnames(boot$bipc$boot) %>%
            factor(., levels = input_var$block %>% levels())
        ) %>%
        pivot_longer(
          cols = -block,
          names_to = "b",
          values_to = "value"
        )

      bootstraps_bipc_summary <-
        bootstraps_bipc %>%
        group_by(block) %>%
        summarize(
          lo = quantile(value, p = (1-CI)/2),
          up = quantile(value, p = CI + (1-CI)/2),
          .groups = "drop"
        )

      bipc <- bipc %>% left_join(bootstraps_bipc_summary, by = join_by(block))
    }
  } else {
    bipc <- bipc %>% mutate(lo = NA, up = NA)
  }
  bipc
}
