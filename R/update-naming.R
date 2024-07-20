#' @export
update_naming <- function(df) {

  fct_reorg <- function(fac, ...) {
    fct_recode(fct_relevel(fac, ...), ...)
  }

  df %>%
    mutate(
      age_group = fct_reorg(age_group,
                            "15-19" = "Y015_019",
                            "20-24" = "Y020_024",
                            "25-29" = "Y025_029",
                            "15-24" = "Y015_024"
      ),
      indicator = fct_reorg(indicator,
                            "Not sexually active" = "nosex12m",
                            "One cohabiting partner" = "sexcohab",
                            "Non-regular or multiple partners(s) +" = "sexnonregplus",
                            "Non-regular or multiple partner(s)" = "sexnonreg",
                            "FSW" = "sexpaid12m"
      ),
      iso3 = fct_reorg(iso3,
                       "Botswana" = "BWA",
                       "Cameroon" = "CMR",
                       "Kenya" = "KEN",
                       "Lesotho" = "LSO",
                       "Mozambique" = "MOZ",
                       "Malawi" = "MWI",
                       "Namibia" = "NAM",
                       "Eswatini" = "SWZ",
                       "Tanzania" = "TZA",
                       "Uganda" = "UGA",
                       "South Africa" = "ZAF",
                       "Zambia" = "ZMB",
                       "Zimbabwe" = "ZWE"
      )
    )
}
