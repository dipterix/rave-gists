#' @author Zhengjia Wang
#' @date Jan 08, 2025
#' @license Apache-2.0
#' 
#' @title Generate subcortical surfaces from atlas segmentation files
#' 
#' @param subject_code RAVE Subject code
#' @param project_name RAVE project name; default is `"YAEL"`
#' @param roi_path path to the atlas file
#' @param roi_keys a list of ROI keys; see 'Examples'
#' @param preview whether to preview the results; default is `TRUE`
#' 
#' @examples
#' 
#' snippet <- raveio::load_snippet("image-generate-subcortical-surfaces")
#' snippet(subject_code = "DemoSubject",
#'         project_name = "YAEL",
#'         roi_path = "~/rave_data/raw_dir/DemoSubject/rave-imaging/fs/mri/aparc+aseg.mgz",
#' 
#'         roi_keys = list(
#'           # name = c(left key, right key)
#'           "hippocampus" = c(17, 53)
#'         ),
#' 
#'         preview = TRUE)
#' 
#'  
#' END OF DOC
NULL

# ---- Variable Section --------------------------------------------------------
# subject_code <- "DemoSubject"
# project_name <- "YAEL"
# 
# roi_path <- "~/rave_data/raw_dir/DemoSubject/rave-imaging/fs/mri/aparc+aseg.mgz"
# 
# roi_keys <- list(
#   "hippocampus" = c(17, 53)
# )
# 
# preview <- TRUE

# ---- Code Section ------------------------------------------------------------

# initialize variables
`%?<-%` <- dipsaus::`%?<-%`
project_name %?<-% "YAEL"
preview %?<-% TRUE

# ---- start!

subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code)
brain <- raveio::rave_brain(subject)

volume <- threeBrain::read_volume(roi_path)

subcortical_path <- raveio::dir_create2(file.path(brain$base_path, "surf", "subcortical"))

# generate subcortical ROI surfaces
lapply(names(roi_keys), function(name) {
  keys <- roi_keys[[name]]
  if(!length(keys)) { return() }
  if(length(keys) == 1) {
    keys <- c(keys, keys)
  }
  left_key <- as.numeric(keys[[1]])
  right_key <- as.numeric(keys[[2]])
  
  left_fname <- sprintf("lh.%s-%.0f", name, left_key)
  right_fname <- sprintf("rh.%s-%.0f", name, right_key)
  
  threeBrain::volume_to_surf(
    volume = volume,
    threshold_lb = left_key - 0.5,
    threshold_ub = left_key + 0.5,
    save_to = file.path(subcortical_path, left_fname)
  )
  
  threeBrain::volume_to_surf(
    volume = volume,
    threshold_lb = right_key - 0.5,
    threshold_ub = right_key + 0.5,
    save_to = file.path(subcortical_path, right_fname)
  )
  
  return()
})

if(preview) {
  brain <- raveio::rave_brain(subject, surfaces = names(roi_keys))
  print(brain$plot())
}

message("Done. Please check folder\n  ", subcortical_path)
invisible(subcortical_path)
