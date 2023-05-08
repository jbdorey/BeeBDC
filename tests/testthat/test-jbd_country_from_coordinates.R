data("beesFlagged")

(figures <-
  BeeDC::jbd_create_figures(
    data = tibble::tibble(beesFlagged %>% dplyr::select(!.uncer_terms)),
    path = paste0(tempdir()),
    database_id = "database_id",
    workflow_step = "space",
    save_figures = TRUE)
)




