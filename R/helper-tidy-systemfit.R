
#' Tidy `systemfit` fit results into tibble
#'
#' @param fit.sys
#'
#' @return tibble
#' @export tidy_systemfit
#'
#' @examples
#' 
tidy_systemfit <- function(fit.sys){
  #smry.sys <- summary(fit.sys)
  coefs <- fit.sys$coefficients
  rnames <- rownames(coefs)
  cnames <- colnames(coefs)
  
  tbl_smry <- as_tibble(coefs)
  names(tbl_smry) <- cnames
  tbl_smry$vars <- rnames
  tbl_smry <- tbl_smry %>%
    separate(vars, into = c("eq","vars"),sep="_")%>%
    select(eq, vars, everything())
  return(tbl_smry)
}
