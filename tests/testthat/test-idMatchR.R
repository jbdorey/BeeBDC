  # Read in a flagged test dataset
data("beesFlagged")
  # Create a dummy "priorData" dataset using the first fifty rows
priorRun <- beesFlagged %>%
  dplyr::slice_head(n = 50)


testOut <- BeeDC::idMatchR(
  currentData = beesFlagged %>% dplyr::mutate(database_id = paste0("NewTest_ID", dplyr::row_number())),
  priorData = priorRun,
  # First matches will be given preference over later ones
  matchBy = tibble::lst(c("gbifID"),
                        c("catalogNumber", "institutionCode", "dataSource"),
                        c("occurrenceID", "dataSource"),
                        c("recordId", "dataSource"),
                        c("id"),
                        # Because INHS was entered as it's own dataset but is now included in the GBIF download...
                        c("catalogNumber", "institutionCode")),
  # You can exclude datasets from prior by matching their prefixs — before first underscore:
  # Which datasets are static and should be excluded from matching?
  excludeDataset = c("ASP", "BMin", "BMont", "CAES", "EaCO", "Ecd", "EcoS",
                     "Gai", "KP", "EPEL", "CAES", "EaCO", "FSCA", "SMC", "Lic", "Arm"))


TEST <- BeeDC::idMatchR(
  currentData = beesFlagged,
  priorData = priorRun,
  # First matches will be given preference over later ones
  matchBy = tibble::lst(c("gbifID"),
                        c("catalogNumber", "institutionCode", "dataSource"),
                        c("occurrenceID", "dataSource"),
                        c("recordId", "dataSource"),
                        c("id"),
                        # Because INHS was entered as it's own dataset but is now included in the GBIF download...
                        c("catalogNumber", "institutionCode")),
  # You can exclude datasets from prior by matching their prefixs — before first underscore:
  # Which datasets are static and should be excluded from matching?
  excludeDataset = c("ASP", "BMin", "BMont", "CAES", "EaCO", "Ecd", "EcoS",
                     "Gai", "KP", "EPEL", "CAES", "EaCO", "FSCA", "SMC", "Lic", "Arm")) %>%
  verify_output(.)







