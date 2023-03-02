#' @author Zhengjia Wang
#' @date Feb 08, 2023
#' @license Apache-2.0
#' 
#' @title Load and export RAVE voltage data (with epoch and reference)
#' @param subject string in <project/subject> format (e.g. 'demo/DemoSubject')
#' @param electrodes electrodes to export, or \code{NULL} to export all
#' @param reference_name channel reference name
#' @param epoch_name trial epoch table name
#' @param time_window time window, can be a vector of length 2 or a list of
#' vectors with 2 elements; e.g. \code{c(-1, 2)} (1 second before onset to
#' 2 seconds after onset), or \code{list(c(-1,2), c(3,4))} (-1~2 and 3~4 sec)
#' @param save_path where to save exports to, will be created as directory;
#' existing folders will be backed up
#' @returns Nothing. Please go to `save_path` to check exported voltage data
#' @examples
#' 
#' snippet <- raveio::load_snippet("repository-load-epoched-referenced-voltage",
#'                                 local = FALSE)
#' snippet(subject = "demo/DemoSubject", reference_name = "default", 
#'         epoch_name = "auditory_onset", time_window = c(-1, 2),
#'         save_path = "exports")
#' 
#' END OF DOCS
NULL

# ---- Global variables ----------------------------------------------------

# # Uncomment to change variabels
# subject <- "demo/DemoSubject"
# electrodes <- NULL
# reference_name <- "default"
# epoch_name <- "auditory_onset"
# time_window <- c(-1, 2)
# save_path <- "./exports/demo/DemoSubject/voltage"

# ---- Code part ----------------------------------------------------------
force(subject)
force(time_window)
force(save_path)
`%?<-%` <- dipsaus::`%?<-%`
electrodes %?<-% NULL
subject_instance <- raveio::as_rave_subject(subject_id = subject, strict = FALSE)
reference_name <- subject_instance$reference_names[[1]]
epoch_name <- subject_instance$epoch_names[[1]]

if( !epoch_name %in% subject_instance$epoch_names ) {
  stop("Cannot find epoch [", epoch_name, "]. Available epoch names are: ",
       paste0(subject_instance$epoch_names))
}


if( !reference_name %in% subject_instance$reference_names ) {
  stop("Cannot find reference [", reference_name, "]. Available references are: ",
       paste0(subject_instance$reference_names))
}

if(!length(electrodes)) {
  electrodes <- subject_instance$electrodes
}
raveio::with_future_parallel({
  repository <- raveio::prepare_subject_voltage_with_epoch(
    subject = subject, electrodes = electrodes, 
    reference_name = reference_name, epoch_name = epoch_name, 
    time_windows = time_window
  )
})
# reset parallel


if(file.exists(save_path)) {
  raveio::backup_file(save_path, quiet = FALSE, remove = TRUE)
}
raveio::dir_create2(file.path(save_path, "data"))
save_path <- normalizePath(save_path)

# save
write.csv(repository$reference_table, file = file.path(save_path, "reference.csv"))
write.csv(repository$electrode_table, file = file.path(save_path, "electrodes.csv"))
write.csv(repository$epoch$table, file = file.path(save_path, "epoch.csv"))

raveio::with_future_parallel({
  dipsaus::lapply_async2(repository$electrode_list, function(e) {
    arr <- repository$voltage$data_list[[sprintf("e_%d", e)]]
    h5file <- file.path(save_path, "data", sprintf("%d.h5", e))
    raveio::save_h5(
      arr[drop = FALSE], file = h5file, name = "data",
      ctype = "numeric", quiet = TRUE
    )
    
    raveio::save_h5(
      repository$voltage$dimnames$Time, file = h5file, name = "time_points",
      ctype = "numeric", quiet = TRUE
    )
    
    raveio::save_h5(
      as.integer(repository$voltage$dimnames$Trial), file = h5file, name = "trial",
      ctype = "integer", quiet = TRUE
    )
    
  }, plan = FALSE, callback = function(e) {
    sprintf("Exporting | Electrode channel %d", e)
  })
})

raveio::save_yaml(list(
  subject = subject_instance$subject_id,
  sample_rates = unique(subject_instance$raw_sample_rates),
  electrodes = dipsaus::deparse_svec(electrodes),
  time_window = time_window,
  data = "voltage",
  refrenced = TRUE
), file = file.path(save_path, "information.yaml"))

future::plan("sequential")
