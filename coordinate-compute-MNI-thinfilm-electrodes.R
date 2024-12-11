#' @author Zhengjia Wang
#' @date Dec 10, 2024
#' @license Apache-2.0
#' 
#' @title Calculate `MNI` template coordinates for thin-film micro-ECoG arrays
#'
#' @param subject_code (string, mandatory) subject code
#' @param project_name (string, mandatory) which project contains the localized
#' electrode coordinates
#' @param T1w (string, optional) path to original `T1w` image, must exist if 
#' non-linear normalization is needed
#' @param CT (string, optional) path to `CT`
#' @param T2w (string, optional) path to `T2w`
#' @param fGATIR (string, optional) path to `fGATIR`
#' @param preopCT (string, optional) path to preop-`CT`
#' @param FLAIR (string, optional) path to `FLAIR`
#' @param T1wContrast (string, optional) path to `T1w` with contrast
#' @param normalization (logical, optional) whether to run non-linear
#' normalization; default is depended on whether `ANTsPyx` is installed.
#' Please make sure `ANTsPyx` is installed if you want this feature.
#' If you set this to `FALSE`, then volumetric transform will most
#' likely to be `affine`.
#' @param template_name (string, optional) normalize `MNI` template; default is
#' `'mni_icbm152_nlin_asym_09b'`
#' @param flip_hemisphere (logical, optional) whether to flip the hemisphere;
#' default is `FALSE`
#' @param volume_surface_interpolator (float, optional) interpolation factor;
#' set to `0` for volumetric-mapping only; set to `1` for surface mapping only;
#' set to somewhere between `(0, 1)` for a little bit of both; default is `0.3`
#' @param project_surface (string, optional) surface to project onto during the
#' surface normalization; default is `"pial"`, other choices are `"white"` or
#' `"smoothwm"`
#' @param volumetric_transform (string, optional) which volumetric transform
#' to use; default is `"auto"`-matically determined; other choices are
#' `"affine"` or `"nonlinear"`. Notice when non-linear transform is explicitly
#' specied but missing, then the program will raise an error
#' @param preview (logical/string, optional) whether to generate a preview;
#' default is `TRUE`. Set to `FALSE` to avoid showing the viewer. Alternatively,
#' set to a string if you would like to save the viewer to the path
#' @param save_as (string, optional) file name of which the new electrode
#' table will be saved as. if `save_as` is a valid file name, then the table
#' will be saved back to the subject's meta folder. Existing files will be
#' backed up. If invalid, then no file will be created. Default is
#' `"electrodes.csv"`
#'
#' @examples
#'
#' compute_thinfilm_mni <- raveio::load_snippet("coordinate-compute-MNI-thinfilm-electrodes")
#' results <- compute_thinfilm_mni(
#'   project_name = "YAEL",
#'   subject_code = "ThinFilm001",
#'   T1w = "/path/to/T1w.nii.gz",
#'   preview = TRUE,
#'   save_as = "electrodes_nonlinear_mapped.csv"
#' )
#' 
#' results$electrode_table
#'
#'
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

## Un-comment the followings to customize

# # RAVE subject & project
# subject_code <- "ThinFilm001"
# project_name <- "YAEL"  # where electrodes are localized
#
# # path to image modalities
# T1w <- "/Users/dipterix/rave_data/raw_dir/ThinFilm001/rave-imaging/fs/mri/orig/001.nii.gz"
# CT <-
# fGATIR <-
# T2w <-
# T1wContrast <-
# preopCT <-
# FLAIR <-
#
# # Normalization settings
# normalization <- rpyANTs::ants_available()
# template_name <- "mni_icbm152_nlin_asym_09b"
# flip_hemisphere <- FALSE
# volume_surface_interpolator <- 0.3
# project_surface <- "pial"         # pial, white, smoothwm
# volumetric_transform <- "auto"    # auto, affine, nonlinear
#
# # Visualization, save
# preview <- TRUE                   # or path to which the viewer will be saved
# save_as <- "electrodes.csv"       # or `NULL` to avoid replacing the existing table

# ---- code body ---------------------------------------------------------------

### Initialize variables

`%?<-%` <- dipsaus::`%?<-%`

project_name %?<-% "YAEL"

T1w %?<-% NULL
CT %?<-% NULL
T2w %?<-% NULL
fGATIR %?<-% NULL
preopCT %?<-% NULL
FLAIR %?<-% NULL
T1wContrast %?<-% NULL

# Make sure Python is configured
normalization %?<-% rpyANTs::ants_available()
template_name %?<-% "mni_icbm152_nlin_asym_09b" # ...a or ...c
flip_hemisphere %?<-% FALSE
volume_surface_interpolator %?<-% 0.3
project_surface %?<-% "pial"        # pial, white, smoothwm
volumetric_transform %?<-% "auto"   # auto, affine, nonlinear
# TRUE/FALSE, or a string of path where the viewer is to be stored
preview %?<-% TRUE

# A string of the file name to be saved back to meta folder
# default is to replace the original electrodes.csv
# the previous `electrodes.csv` will be backed up
# If you do not wish to save anything, set `save_as` as `NULL`
save_as %?<-% "electrodes.csv"

### Check variables and ensure the environment
if( normalization && !rpyANTs::ants_available() ) {
  if(length(T1w)) {
    ravemanager::configure_python()
  } else {
    raveio::catgl("`T1w` is not specified... Skipping normalization", level = "WARNING")
    normalization <- FALSE
  }
}

if( volume_surface_interpolator < 0 ) {
  volume_surface_interpolator <- 0
} else if(volume_surface_interpolator > 1) {
  volume_surface_interpolator <- 1
}

modalities <- list(
  T1w = T1w,
  CT = CT,
  T2w = T2w,
  fGATIR = fGATIR,
  preopCT = preopCT,
  FLAIR = FLAIR,
  T1wContrast = T1wContrast
)

lapply(names(modalities), function(modality) {
  path <- modalities[[modality]]
  if(is.null(path)) {
    return()
  }
  if(!file.exists(path)) {
    stop("Image ", modality, " is specified but the path is invalid. Please check if all images exist.")
  }
})

### Basic commons
# Please localize the subject in RAVE first
subject <- raveio::RAVESubject$new(project_name = project_name,
                                   subject_code = subject_code,
                                   strict = TRUE)

freesurfer_path <- subject$freesurfer_path
if(is.na(freesurfer_path) || !file.exists(freesurfer_path)) {
  stop("Cannot find subject's FreeSurfer path. Please make sure the RAVE subject has rave-imaging/fs folder")
}

if(!length(subject$get_electrode_table())) {
  stop("No electrode found. Please localize the subject first")
}

### map T1 to template (non-linear)
if( normalization ) {
  raveio::catgl("Coregister images and normalize T1w to {template_name}", level = "INFO")
  raveio::yael_preprocess(
    subject_code = subject_code,
    t1w_path = modalities$T1w,
    ct_path = modalities$CT,
    t2w_path = modalities$T2w,
    fgatir_path = modalities$fGATIR,
    preopct_path = modalities$preopCT,
    flair_path = modalities$FLAIR,
    t1w_contrast_path = modalities$T1wContrast,
    normalize_template = template_name,
    add_surfaces = FALSE,
    verbose = TRUE
  )
}


# transform thin-film electrode contacts to MNI coordinate space
electrode_table = raveio::transform_thinfilm_to_mni152(
  subject = subject,
  flip_hemisphere = flip_hemisphere,
  interpolator = volume_surface_interpolator,
  project_surface = project_surface,
  volumetric_transform = volumetric_transform
)

native_brain <- raveio::rave_brain(subject)
native_brain$set_electrodes(electrode_table, priority = "sphere")
native_brain$set_electrode_values(electrode_table)
template <- threeBrain::merge_brain(native_brain, template_subject = "cvs_avg35_inMNI152")
title <- sprintf(
  "%s [interp=%.1f,flip_hemi=%s,surf=%s,volu=%s]",
  subject$subject_code,
  volume_surface_interpolator,
  ifelse(flip_hemisphere, "Y", "N"),
  project_surface,
  volumetric_transform
)

if( !isFALSE(preview) ) {
  widget <- template$plot(title = title)
  
  if(is.character(preview)) {
    threeBrain::save_brain(widget, path = preview, title = subject$subject_code)
    raveio::catgl("3D viewer preview saved to:\n  {preview}", level = "INFO")
  } else {
    raveio::catgl("Rendering 3D preview...", level = "INFO")
    print(widget)
  }
}

save_as <- trimws(gsub("[/ ]+", "", save_as))
save_as <- save_as[!is.na(save_as) & nzchar(save_as)]
if(length(save_as)) {
  save_as <- save_as[[1]]
  if(!grepl("\\.csv$", save_as, ignore.case = TRUE)) {
    save_as <- sprintf("%s.csv", save_as)
  }
  if(tolower(save_as) == "electrodes.csv") {
    save_as <- tolower(save_as)
  }
  
  save_as <- file.path(subject$meta_path, save_as)
  # save as csv and back up existing file
  raveio::safe_write_csv(
    electrode_table,
    file = save_as,
    row.names = FALSE
  )
  save_as <- normalizePath(save_as, winslash = "/")
  raveio::catgl("Electrode path saved:\n  {save_as}", level = "INFO")
}

# When running as snippet, return this list
invisible(list(
  path = save_as,
  template_brain = template,
  electrode_table = electrode_table
))
