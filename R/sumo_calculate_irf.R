# IMPULSE RESPONSE FUNCTION

#' Calculate impulse response functions
#'
#' This function calculates impulse response functions using either a VAR or VEC model,
#' depending on the stationarity of the data.
#'
#' @param data Data frame. The time series data to be used for the model.
#' @param lag.max Integer. Maximum lag that should be used for model selection. Defaults to 20.
#' @param irf_ahead Integer. Number of periods to forecast ahead for impulse response functions. Defaults to 20.
#' @param ic_use Character. The information criterion to be used for model selection. Can be one of "HQ", "AIC", "FPE", or "SC". Defaults to "HQ".
#' @param var_type Character. Type of deterministic regressors to include in the VAR model. Can be one of "const", "trend", "both", or "none". Defaults to "const".
#' @param ecdet Character. The deterministic regressors to include in the cointegration test. Can be one of "none", "const", "trend". See `ca.jo` for details.
#' @param spec Character. Specification of the deterministic components in the VECM. Can be one of "longrun", "transitory". See `ca.jo` for details.
#' @param season Integer. The frequency of the seasonality in the cointegration test. See `ca.jo` for details.
#' @param shock_size Numeric. Size of shock for impulse response functions. Defaults to 0.05.
#' @param kpss_null Character. The null hypothesis for the KPSS test. Can be either "Level" or "Trend". See `ur.kpss` for details.
#'
#' @return A list containing the calculated impulse response functions.

data <- fuel_prices



sumo_calculate_irf <- function(data,
                               lag.max = 20,
                               irf_ahead = 20,
                               ic_use = "HQ",
                               var_type = c("const", "trend", "both", "none"),
                               ecdet,
                               spec,
                               season,
                               shock_size = 0.05,
                               kpss_null = c("Trend", "Level")
                               ) {

  ic_used <- switch(ic_use,
                    "AIC" = 1,
                    "HQ"  = 2,
                    "SC"  = 3,
                    "FPE" = 4)


  # Remove date
  data_ls <- remove_date_datetime_cols(fuel_prices)

  # Estimate VAR or VEC based on ca.jo outcome
  model <- estimate_var_vec(data     = data_ls$modified_df,
                            lag.max  = lag.max,
                            var_type = var_type,
                            ic_used  = ic_used,
                            ecdet    = ecdet,
                            spec     = spec,
                            season   = season
                            )

  # Calculate IRF
  if (inherits(model, "varest")) {

    irf <- vars::irf(model, n.ahead = irf_ahead)

  } else if (inherits(model, "ca.jo")) {

    var_model <- vars::vec2var(model)
    irf <- vars::irf(var_model, n.ahead = irf_ahead)

  } else {
    stop("model should be a VAR or VEC model.")
  }

  return(irf)

}



# Estimate VAR or VEC
estimate_var_vec <- function(data, var_type, ic_used, ecdet, spec, season, lag.max = 10) {

  p_values <- check_stationarity(data)

  var_order <- vars::VARselect(data, lag.max = lag.max, type = var_type)$selection[ic_used]


  if(all(p_values < 0.05)) { # if all variables are stationary
    model <- vars::VAR(data, p = var_order, type = var_type)

  } else { # if any variable is not stationary
    model <- urca::ca.jo(data,
                         type   = "trace",
                         ecdet  = ecdet,
                         spec   = spec,
                         K      = var_order,
                         season = season
                         )
  }

  return(model)
}



# Check stationarity
check_stationarity <- function(data) {
  p_values <- sapply(data, function(x) tseries::adf.test(x)$p.value)
  names(p_values) <- colnames(data)
  return(p_values)
}



