#' @author Zhengjia Wang
#' @date May 29, 2024
#' @license Apache-2.0
#' 
#' @title Calculate accurate `MNI152` coordinates using `YAEL` preprocess
#' 
#' @param t1w_path path to T1-weighted MRI. Please make sure this is the 
#' `NIfTI` file used as input by `FreeSurfer`
#' @param project_name RAVE project name
#' @param subject_code RAVE subject code
#' @param preview (optional) whether to show the visualizations (one on 
#' template and the other on native brain) so you can compare the differences;
#' default is `TRUE`
#' @param template (optional) which `MNI152` template to use, choices are 
#' `mni_icbm152_nlin_asym_09a` (default), `mni_icbm152_nlin_asym_09b`, and 
#' `mni_icbm152_nlin_asym_09c`.
#' 
#' @details
#' Please configure `python` environments via `ravemanager::configure_python()` 
#' This script uses `YAEL` proposed pipeline. This pipeline has been integrated
#' into `RAVE` imaging preprocessing and electrode localization modules and
#' normally you wouldn't need this script. However, for some users (Windows
#' users, or those who installed & processed subjects before this function
#' is available), this script can be used as "post-process" that refines 
#' the `MNI152` and `MNI305` coordinates in `electrodes.csv`. To ensure 
#' reproducibility, please check your `RAVE` versions:
#' 
#' ------ RAVE core package information: ----------
#'    ravemanager:      1.0.42
#'    rave:             1.0.3.3
#'    ravetools:        0.1.6
#'    filearray:        0.1.6
#'    shidashi:         0.1.6
#'    rpymat:           0.1.7
#'    dipsaus:          0.2.8.9003
#'    threeBrain:       1.1.0.9019
#'    raveio:           0.9.0.52
#'    ravedash:         0.1.3.30
#'    readNSx:          0.0.4
#'    rpyANTs:          0.0.3.9003
#'    rutabaga:         0.1.7
#'    ravebuiltins:     1.0.5
#' 
#' @examples
#' 
#' yael_postproc <- raveio::load_snippet("coordinate-compute-MNI-using-YAEL")
#' 
#' result <- yael_postproc(
#'   t1w_path = '~/rave_data/raw_dir/Pt003/rave-imaging/fs/mri/orig/001.mgz',
#'   project_name = "YAEL",
#'   subject_code = "Pt003",
#'   template = "mni_icbm152_nlin_asym_09a",
#'   preview = TRUE
#' )
#' 
#' result$subject
#' result$electrode_table
#' 
#' 
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

# t1w_path <- '~/rave_data/raw_dir/Pt003/rave-imaging/fs/mri/orig/001.mgz'
# project_name <- "YAEL"
# subject_code <- "Pt003"
# # can be a/b/c. Normally MNI152 refers to MNI152 a or b
# # A is faster (~10 min), B is more accurate (~1 hr)
# template <- "mni_icbm152_nlin_asym_09a"

# ---- code body ---------------------------------------------------------------

# ---- Default arguments
`%?<-%` <- dipsaus::`%?<-%`

preview %?<-% TRUE
template %?<-% "mni_icbm152_nlin_asym_09a"

# normalize T1w image
raveio::yael_preprocess(
  subject_code = subject_code,
  t1w_path = t1w_path,
  normalize_template = "mni_icbm152_nlin_asym_09a"
)

# Load subject
subject <- raveio::RAVESubject$new(project_name = project_name,
                                   subject_code = subject_code,
                                   strict = FALSE)

# get Scanner T1w RAS
electrode_table <- subject$get_electrode_table()
scan_ras <- electrode_table[, c("T1R", "T1A", "T1S")]

# load YAEL (pre)process
process <- raveio::as_yael_process(subject)

# Get MNI152 coordinates
mni152 <- process$transform_points_to_template(
  native_ras = scan_ras, 
  template_name = "mni_icbm152_nlin_asym_09a"
)

# MNI305
mni305 <- cbind(mni152, 1) %*% t(solve(raveio::MNI305_to_MNI152))

# Update electrode table
electrode_table$MNI152_x <- mni152[, 1]
electrode_table$MNI152_y <- mni152[, 2]
electrode_table$MNI152_z <- mni152[, 3]

electrode_table$MNI305_x <- mni305[, 1]
electrode_table$MNI305_y <- mni305[, 2]
electrode_table$MNI305_z <- mni305[, 3]

# Save electrodes.csv
raveio::save_meta2(
  data = electrode_table,
  meta_type = "electrodes",
  project_name = project_name,
  subject_code = subject_code
)

# Check the results on template brain
subject <- raveio::RAVESubject$new(project_name = project_name,
                                   subject_code = subject_code,
                                   strict = FALSE)
native_brain <- raveio::rave_brain(subject)

# using FreeSurfer template `cvs_avg35_inMNI152`
template_brain <- tryCatch({
  threeBrain::merge_brain(native_brain, template_subject = "cvs_avg35_inMNI152")
}, error = function(e) {
  # You don't have this template, download it on the fly
  threeBrain::download_template_subject(subject_code = "cvs_avg35_inMNI152")
  threeBrain::merge_brain(native_brain, template_subject = "cvs_avg35_inMNI152")
})

# This line fixes the alignment issues on this template
template_brain$template_object$xfm <- solve(raveio::MNI305_to_MNI152)


if( preview ) {
  
  # For anatomical slices
  template_brain$template_object$set_electrodes(
    data.frame(
      Electrode = electrode_table$Electrode,
      x = electrode_table$MNI152_x,
      y = electrode_table$MNI152_y,
      z = electrode_table$MNI152_z,
      Label = electrode_table$Label
    ),
    coord_sys = "scannerRAS"
  )
  
  # Randomly pick an electrode to plot
  electrode_to_plot <- sample(electrode_table$Electrode, 1)
  message("Plotting anatomical slices for randomly selected electrode contact: ", electrode_to_plot)
  
  # Plot anat slices on native subject
  message("Plotting on native brain")
  native_brain$plot_electrodes_on_slices(electrodes_to_plot = electrode_to_plot, zoom = 2)
  
  message("Plotting on template brain")
  template_brain$template_object$plot_electrodes_on_slices(electrodes_to_plot = electrode_to_plot, zoom = 2)
  
  message("Generating 3D viewers")
  native_viewer <- native_brain$plot(title = sprintf("Native Brain - %s", subject_code))
  native_viewer_path <- threeBrain::save_brain(native_viewer, tempfile(fileext = ".html"))
  utils::browseURL(native_viewer_path)
  
  # Remove electrodes temporarily set on template object
  template_brain$template_object$electrodes$objects <- list()
  template_viewer <- template_brain$plot(title = "Template Brain - MNI152")
  template_viewer_path <- threeBrain::save_brain(template_viewer, tempfile(fileext = ".html"))
  utils::browseURL(template_viewer_path)
}

# Return if this script is called as a function
invisible(list(
  subject = subject,
  native = native_brain,
  template = template_brain,
  electrode_table = electrode_table
))

