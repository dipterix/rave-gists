#' @title Download datasets from OpenNeuro
#' 
#' @date May 09, 2025
#' @author Zhengjia Wang
#' @license Apache-2.0
#' 
#' 
#' @param dataset The dataset to retrieve, for example `ds005953`
#' @param tag The tag (revision) of the dataset to retrieve; default is 
#' `NULL`, i.e. the most current version
#' @param subjects subjects to download; default is `NULL` (all available
#' subjects); notice if this is set, then only these subjects will be 
#' downloaded. All the stimuli or derivatives may be missing. It is strongly
#' recommended not to set this parameter unless the dataset is too large
#' @param parent_dir directory where the data will be downloaded to; default
#' is `raveio::raveio_getopt("bids_data_dir")` (if unset, default path is
#' `~/rave_data/bids_dir` under your home directory)
#' 
#' @examples
#' 
#' 
#' 
#' 
#' END OF DOC
NULL

# ---- Global variables --------------------------------------------------------

## Mandatory
# dataset <- "ds005953"

## Optional
# tag <- NULL
# subjects <- NULL


# ---- Code body ---------------------------------------------------------------

# Initialize global variables
force(dataset)
`%?<-%` <- dipsaus::`%?<-%`

tag %?<-% NULL
subjects %?<-% NULL

parent_dir %?<-% raveio::raveio_getopt("bids_data_dir", "~/rave_data/bids_dir")


# Make sure openneuro-py has been installed
ravemanager::ensure_rpymat()
openneuro <- tryCatch(
  {
    rpymat::import("openneuro")
  },
  error = function(e) {
    ravemanager::add_py_package("openneuro-py", method = "pip")
    rpymat::import("openneuro")
  }
)

# Intialize directories
parent_dir <- raveio::dir_create2(parent_dir)

target_dir <- normalizePath(file.path(parent_dir, dataset, fsep = "/"), winslash = "/", mustWork = FALSE)

if(!length(subjects)) {
  openneuro$download(dataset = dataset, target_dir = target_dir, tag = tag)
} else {
  include <- as.list(sprintf("*sub-%s/*", subjects))
  openneuro$download(dataset = dataset, target_dir = target_dir, include = include, tag = tag)
}
