

remove_date_datetime_cols <- function(df) {
  # Detect date and datetime columns
  date_cols <- sapply(df, function(x) inherits(x, c("Date", "POSIXt")))

  # Initialize an empty list to hold the date/datetime columns
  date_lists <- list()

  # If there's any date or datetime column
  if(any(date_cols)) {
    # Save the date and datetime column(s) to a list
    date_lists <- df[, date_cols, drop = FALSE]

    # Now, remove the date and datetime columns from df
    df <- df[, !date_cols]
  } else {
    print("No date or datetime column found.")
  }

  # Return both the modified dataframe and the date/datetime lists
  list(modified_df = df, date_lists = date_lists)
}
