library(tidyverse)

Sys.setlocale("LC_CTYPE", "Icelandic_Iceland.utf8")
#Sys.setlocale("LC_COLLATE", "Icelandic_Iceland.utf8")

bl_endp_key <- AzureStor::storage_endpoint(
  "https://datalakesumotest.blob.core.windows.net",
  key = Sys.getenv("AZURE_TEST")
)

container_gasvaktin <- AzureStor::storage_container(bl_endp_key, "gasvaktin")

gasvaktin_tbl <- AzureStor::storage_read_csv(container_gasvaktin, "historical/gasvaktin_historical.csv")


fuel_prices <- gasvaktin_tbl %>%
  filter(str_detect(name, "Reykjavíkurv")) %>%
  select(date, company_id, bensin) %>%
  pivot_wider(names_from = company_id, values_from = bensin) %>%
  set_names("date", "price_1", "price_2")


usethis::use_data(fuel_prices, overwrite = TRUE, internal = TRUE)
