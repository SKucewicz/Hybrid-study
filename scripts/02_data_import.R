cat("importing data.\n")
#PART 1: SET DIRECTORY & DATA IMPORT----
modex_xfit <- readr::read_csv(here::here("data", "SK_hybrid.csv"))
modex_xfit <- as_tibble(modex_xfit) 

modex_supfit <- readr::read_csv(here::here("data", "SK_SBS.csv"))
modex_supfit <- as_tibble(modex_supfit) 

cat("Data successfully imported.\n")