#' @author Zhengjia Wang
#' @date Oct 20, 2025
#' @license Apache-2.0
#' 
#' @title Compute YBA (Yale Brain Atlas) from non-linear deformation
#' @description
#' This script is based on YBA-690 atlas and needs image preprocessing. Please
#' check out `ravecore::cmd_run_yael_preprocess(...)` to calculate non-linear
#' registration to the template brain first.
#' 
#' @param subject_id RAVE subject ID in 'project/subject' format
#' @param radius radius of searching for atlas labels
#' @param atlas_path atlas path to `YBA.nii[.gz]`, default is to download 
#' from the internet
#' @param preview whether to preview the 3D viewer; default is true
#' @returns A table of YBA labels with the top 4 possible labels if the 
#' electrodes are localized; the label table is also saved to the subject meta
#' folder, under `meta/YBA.csv`. A 3D viewer will be created under raw
#' `rave-imaging/visualization/YBA.html`. The native atlas will be created 
#' under `rave-imaging/fs/mri/YBA_aseg.nii.gz`
#' 
#' @examples
#' 
#' # Example usage:
#' 
#' comput_YBA = ravepipeline::load_snippet("image-compute-volumetric-YBA-atlas")
#' comput_YBA(
#'   subject_id = "YAEL/Precision012",
#'   radius = 2, preview = TRUE
#' )
#' 
#' 
#' END OF DOC
NULL

# ---- Global inputs -----------------------------------------------------------
subject_id = "YAEL/Precision012"
atlas_path = NULL
radius = 2
preview = TRUE

# ---- Code --------------------------------------------------------------------
# Initialize values
force(subject_id)
`%?<-%` <- dipsaus::`%?<-%`
atlas_path %?<-% ieegio::ieegio_sample_data('atlases/YBA/YBA690.nii.gz')
radius %?<-% 2
preview %?<-% TRUE

subject = ravecore::as_rave_subject(subject_id, strict = FALSE)
colormap = threeBrain::load_colormap(system.file("palettes", "datacube2", "YBA690ColorLUT.json", package = 'threeBrain'))
brain = ravecore::rave_brain(subject)

# Check if imaging files are complete
if(is.null(brain)) {
  stop("Subject 3D models/imaging files are not found: ", subject$subject_id)
}

# Check if the template mapping exists
yael <- ravecore::as_yael_process(subject)
has_mapping <- FALSE
template_names <- c(
  "mni_icbm152_nlin_sym_09b",
  "mni_icbm152_nlin_sym_09a",
  "mni_icbm152_nlin_sym_09c",
  "mni_icbm152_nlin_asym_09b",
  "mni_icbm152_nlin_asym_09a",
  "mni_icbm152_nlin_asym_09c"
)
for(template_name in template_names) {
  tryCatch(
    {
      message("Check template: ", template_name)
      mapping <- yael$get_template_mapping(template_name = template_name)
      if(!is.null(mapping)) {
        has_mapping <- TRUE
      }
    },
    error = function(e) {
    }
  )
  if(has_mapping) {
    message("Found mapping to ", template_name)
    break
  }
}

if(!has_mapping) {
  stop("Unable to find non-linear volumetric mapping to MNI152 template. Please run `ravecore::cmd_run_yael_preprocess(...)` first!")
}


# Create atlas from MNI152 -> native brain
ravecore::generate_atlases_from_template(
  subject,
  dirname(atlas_path),
  template_name = template_name,
  as_job = FALSE,
  surfaces = FALSE
)

# Save a copy to rave-imaging/fs/mri/ for visualizations later
atlas_native <- file.path(subject$imaging_path, "atlases", basename(atlas_path))
atlas_native2 <- file.path(brain$base_path, "mri", "YBA_aseg.nii.gz")
v <- ieegio::read_volume(atlas_native)
ieegio::write_volume(v, atlas_native2)

# Compute labels
has_electrodes <- nrow(brain$electrodes$raw_table) > 0
if(has_electrodes) {
  labels = brain$electrodes$get_atlas_labels(
    atlas_native2,
    lut = colormap,
    radius = radius
  )
  
  labels$Electrode = brain$electrodes$raw_table$Electrode
  label_path <- file.path(subject$meta_path, "YBA.csv")
  write.csv(labels, file = label_path)
  
  message("Saving labels to \n\t", label_path)
} else {
  labels <- NULL
}


# visualization
if(has_electrodes) {
  labels <- read.csv(file.path(subject$meta_path, "YBA.csv"))
  label_text <- sort(unique(labels$Label1))
  colors <- sapply(label_text, function(lbl) {
    x <- colormap$map[[colormap$get_key(lbl) + 1]]
    rgb(x$R, x$G, x$B, maxColorValue = 255)
  })
  labels$Label1 <- factor(labels$Label1, levels = label_text)
  
  brain$set_electrode_values(labels)
}

brain$add_atlas("YBA_aseg", color_format = "RGBAFormat")
widget <- brain$plot(
  palettes = list(Label1 = colors),
  voxel_colormap = colormap,
  controllers = list(
    "Display Data" = "Label1",
    "Voxel Display" = "anat. slices",
    "Voxel Type" = "YBA_aseg"
  )
)
vis_folder <- ravepipeline::dir_create2(file.path(subject$imaging_path, "visualizations"))
message("Saving 3D viewer...")
path <- threeBrain::save_brain(widget, path = file.path(vis_folder, "YBA.html"))
print(path)
if(isTRUE(preview)) {
  print(widget)
}

invisible(labels)
