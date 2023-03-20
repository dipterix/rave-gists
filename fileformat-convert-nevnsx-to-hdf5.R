#' @title Convert `Blackrock-Microsystems` NSP data to RAVE-importable format (HDF5)
#' @author Zhengjia Wang
#' @date 2023-03-20
#' @license Apache-2.0
#' @description
#' Converts `NEV`, `NSx` (ns1, ns2, ..., ns6) data to HDF5 format and store in
#' RAVE raw directory as a session block
#' @param project_name RAVE project name; mandatory
#' @param subject_code subject code; mandatory
#' @param path_neuroevent path to neuro-event file `.nev`; mandatory
#' @param export_prefix folder name prefix of the export; mandatory
#' @param exclude_spike whether to exclude spike data when exporting (default is
#' `TRUE`); this will greatly save time and memory if you choose not to use
#' Blackrock spike soring algorithm
#' @param exclude_nsx which `NSx` to exclude when exporting (default is `NULL`);
#' for example, `exclude_nsx=c(1,2)` will prevent 'ns1' and 'ns2' from being 
#' imported
#' @param force whether to re-export the data if existing files exist? default
#' is `FALSE`
#' @examples
#' 
#' snippet <- raveio::load_snippet(
#'   topic = "fileformat-convert-nevnsx-to-hdf5")
#' snippet(
#'   project_name = "devel",
#'   subject_code = "for_ZW",
#'   path_neuroevent = "~/Downloads/FT_1_p.nev",
#'   export_prefix = "sub-testSubject_ses-FT_task-p",
#'   exclude_spike = TRUE,
#'   exclude_nsx = NULL,
#'   force = FALSE
#' )
#' 
#' 
#' END OF DOC
NULL

# -------- Global variables ----------------------------------------------------

# Un-comment and change accordingly if you run the script offline
# project_name <- "devel"
# subject_code <- "for_ZW"
# 
# path_neuroevent <- "~/Downloads/FT_1_p.nev"
# export_prefix <- "sub-testSubject_ses-FT_task-p"
# exclude_spike <- TRUE
# exclude_nsx <- NULL 
# force <- FALSE

# -------- Code part: do not edit unless you know what you are doing -----------

`%?<-%` <- dipsaus::`%?<-%`
exclude_spike %?<-% TRUE
exclude_nsx %?<-% NULL
force %?<-% FALSE

# Create subject instance using `strict=FALSE` (subject might not exist)
subject <- raveio::RAVESubject$new(project_name = project_name, 
                                   subject_code = subject_code, 
                                   strict = FALSE)

# Check if spike is to be excluded
if( exclude_spike ) {
  exclude_events <- "spike"
} else {
  exclude_events <- NULL
}

exported <- FALSE
if( !force ) {
  # try to load existing data
  tryCatch({
    nsp <- readNSx::get_nsp(file.path(subject$preprocess_settings$raw_path, export_prefix))
    exported <- TRUE
  }, error = function(e) {
    # failed, do nothing to let next step to pick up `exported` flag
  })
}

if(!exported) {
  
  # export NSP data 
  readNSx::import_nsp(
    path = path_neuroevent,
    prefix = file.path(subject$preprocess_settings$raw_path, export_prefix), 
    exclude_events = exclude_events,
    exclude_nsx = exclude_nsx,
    partition_prefix = "_part"
  )
  
}

nsp <- readNSx::get_nsp(file.path(subject$preprocess_settings$raw_path, export_prefix))

# debug information
print(nsp)

message("Done exporting .nev & nsx data with prefix:\n  ", nsp$nev$prefix)
message("If you want to import these data into RAVE, please use [", names(raveio::IMPORT_FORMATS)[[1]], "] format.")

invisible(nsp)
