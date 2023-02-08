#' This file converts RAVE `.fst` file to common format `.hdf5`
#' @author Zhengjia Wang
#' @date Feb 08, 2023
#' @license Apache-2.0
#' 
#' @param hdf5_path path to the `.fst` file
#' @param hdf5_path path to 'HDF5' file; if file exists prior to conversion, 
#'   then that file will be erased first. Please make sure the files are 
#'   backed up.
#' @param exclude_names table names to exclude; default is `NULL` (save all 
#'   table columns)
#'
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

# # Uncomment the following variables if you want to set them and run manually
#
# fst_path <- "~/rave_data/data_dir/demo/_project_data/power_explorer/exports/KC_demo_export-20210525-071414.fst"
# hdf5_path <- tempfile()
# exclude_names <- NULL


# ---- ENDS: variables ---------------------------------------------------------

`%?<-%` <- dipsaus::`%?<-%`
exclude_names %?<-% NULL

raveio_version <- utils::packageVersion("raveio")
if( utils::compareVersion(as.character(raveio_version), "0.0.9.9015") ) {
  convert_fst_to_hdf5 <- function(fst_path, hdf5_path, exclude_names = NULL) {
    
    library(raveio)
    
    tbl <- fst::fst(fst_path)
    
    if(file.exists(hdf5_path)) {
      unlink(hdf5_path)
    }
    
    nms <- names(tbl)
    nms <- nms[!nms %in% exclude_names]
    
    for(nm in nms) {
      dat <- tbl[[nm]]
      if(is.factor(dat)) { dat <- as.character(dat) }
      compress_level <- 9
      if(is.numeric(dat)) {
        compress_level <- 4
      }
      save_h5(x = dat, file = hdf5_path, name = nm, level = compress_level, replace = TRUE)
    }
    
    read_mat2(hdf5_path, ram = FALSE)
  }
} else {
  convert_fst_to_hdf5 <- raveio::convert_fst_to_hdf5
}

convert_fst_to_hdf5(fst_path, hdf5_path, exclude_names)

message("Done.")
