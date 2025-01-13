#' @author Zhengjia Wang
#' @date Jan 07, 2025
#' @license Apache-2.0
#' 
#' @title Burn electrode contacts to RGB(A) NIfTI mask image
#' @param subject_code (mandatory) RAVE subject code
#' @param project_name (mandatory) RAVE project name
#' @param t1_path (optional) path to `T1` MRI; default is the same image used
#' by 3D viewer underlay
#' @param save_path (optional) path to save; default is under the same
#' directory as `t1_path`
#' @param color (optional) color(s) of the contacts to be burned, can be a 
#' length of one or more
#' @param contact_radius (optional) radius of the contacts; default is to be 
#' determined by `Radius` column in electrode table, or 1 mm (or 2 mm diameters) 
#' if nothing is found
#' @param contact_channels (optional) contacts to be burned; default is
#' `NULL` (all contacts); alternatively you can specify `c(1:12)` for burning
#' contacts of the first 1-12 channels. Missing positions will be ignored.
#' WARNING: The contacts order will be ignored, so if you are using `color`, 
#' the order of `color` is consistent with the `electrodes.csv`
#' @param reshape (optional) for super-resolution; set to `FALSE` to use the 
#' same resolution as the underlay, or `TRUE` to double the imaging resolution, 
#' or a number of three (such as `c(512, 512, 512)`) to force the images 
#' generated in such resolution; default is `FALSE`
#' @param alpha_channel (optional) whether the returning image should be 
#' transparent (`RGBA`) or just `RGB` colors; default is `FALSE` due to many 
#' legacy programs don't support `RGBA` NIfTI formats
#' @param blank_underlay whether to use blank image as underlay instead of
#' `T1` image? Default is `TRUE`
#' @param preview (optional) integers to preview the contacts; default is 
#' `NULL`, set to `1` or `c(1,2,3)` to preview the contacts overlay.
#' 
#' 
#' #' @examples
#'
#' snippet <- raveio::load_snippet("image-burn-contacts-to-t1")
#' results <- snippet(
#'   project_name = "demo",
#'   subject_code = "DemoSubject",
#'   preview = c(1,2,3,4),
#'   
#'   # just preview, do not save
#'   save_as = NA
#' )
#' 
#' results
#' 
#' 
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

## Un-comment the followings to customize

# subject_code <- "DemoSubject"
# project_name <- "demo"
# 
# # automatically determined 
# t1_path <- NA
# save_path <- NULL
# 
# # can be length of 1 or more
# color <- c("red", "green", "blue")
# 
# # automatically determined from electrodes.csv
# contact_radius <- NA
# 
# # Which contact(s) to plot? default is NULL (all)
# contact_channels <- NULL
# 
# reshape <- TRUE
# 
# alpha_channel <- FALSE
# 
# preview <- NULL

# ---- code body ---------------------------------------------------------------

### Initialize variables

`%?<-%` <- dipsaus::`%?<-%`

t1_path %?<-% NA
save_path %?<-% NULL

# can be length of 1 or more
color %?<-% "white"

# automatically determined from electrodes.csv
contact_radius %?<-% NA

reshape %?<-% FALSE

alpha_channel %?<-% FALSE

preview %?<-% NULL

contact_channels %?<-% NULL
blank_underlay %?<-% TRUE


# ---- Start! ------------------------------------------------------------------
subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code)
brain <- raveio::rave_brain(subject = subject)

if(is.na(t1_path)) {
  t1_path <- brain$volumes$T1$group$group_data$volume_data$absolute_path
}

# get electrode coordinates in native T1 MRI
electrode_table <- subject$get_electrode_table()

if(!length(contact_channels)) {
  tkr_ras <- electrode_table[, c("Coord_x", "Coord_y", "Coord_z")]
} else {
  tkr_ras <- electrode_table[electrode_table$Electrode %in% contact_channels, c("Coord_x", "Coord_y", "Coord_z")]
}

is_valid <- rowSums(tkr_ras^2) > 0
scan_ras <- brain$electrodes$apply_transform_points(tkr_ras, from = "tkrRAS", to = "scannerRAS")
scan_ras[!is_valid, ] <- NA

if(!length(contact_radius) || isTRUE(is.na(contact_radius))) {
  contact_radius <- electrode_table$Radius
}
if(!length(contact_radius)) {
  contact_radius <- 1
}

# burn image
burned <- ieegio::burn_volume(
  image = ieegio::read_volume(t1_path),
  ras_position = scan_ras,
  col = color,
  radius = contact_radius,
  reshape = reshape,
  alpha = alpha_channel, 
  preview = preview, 
  blank_underlay = blank_underlay
)

if(length(save_path) != 1) {
  save_name <- gsub("\\.(nii|nii.gz)$", "_burned.nii.gz", x = basename(t1_path), ignore.case = TRUE)
  save_path <- file.path(dirname(t1_path), save_name)
}
if(!is.na(save_path)) {
  ieegio::write_volume(burned, save_path)
}

# return if run with raveio
invisible(burned)
