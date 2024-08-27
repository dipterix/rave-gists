#' @title Archive subject into a zip file for sharing
#' @description The archived zip file can be loaded via 
#' `raveio::install_subject`.
#' 
#' @param subject subject ID in `project/subject` format
#' @param path where to save to subject, default is cache directory
#' @param includes data to include in the archive, see 
#' `?raveio::archive_subject`; default is all but original raw signal data
#' @returns Path to archived file
#' END OF DOC
NULL

# ---- Global variables --------------------------------------------------------

# subject <- "demo/DemoSubject"
# path <- NA # or NULL to accept default
# includes <- NULL # default is to include all

# ---- Code --------------------------------------------------------------------

subject_instance <- raveio::as_rave_subject(subject)

`%?<-%` <- dipsaus::`%?<-%`
path %?<-% NA

if(length(path) != 1 || is.na(path)) {
  now <- format(Sys.time(), "%Y%m%dT%H%M%S.zip")
  path <- file.path(raveio::cache_root(), "_archive", subject_instance$project_name, subject_instance$subject_code, now)
  padir <- raveio::dir_create2(dirname(path))
} else {
  if(!endsWith(tolower(path), ".zip")) {
    stop("Path must end with zip.")
  }
  padir <- dirname(path)
  if(!fs::file_access(padir, "write")) {
    stop("Path ", padir, " does not exist or is not writable.")
  }
}


all_data <- c("orignal_signals", "processed_data", "rave_imaging", "pipelines", "notes",
              "user_generated")
includes %?<-% c("processed_data", "rave_imaging", "pipelines", "notes", "user_generated")
includes <- includes[includes %in% all_data]
if(!length(includes)) {
  includes <- all_data
}

# No cache will save around 50% space
path <- normalizePath(path, mustWork = FALSE)

work_path <- file.path(
  raveio::cache_root(),
  "_archive",
  subject_instance$project_name,
  subject_instance$subject_code,
  "archive"
)
work_path <- raveio::dir_create2(work_path)
raveio::archive_subject(
  subject = subject_instance,
  path = path,
  includes = includes,
  config = list(
    orignal_signals = list(include_all = FALSE), 
    processed_data = list(include_cache = FALSE)
  ), 
  work_path = work_path
)

path

