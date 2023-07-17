image_path <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/YAH/MRI/MRI_RAW.nii"
template_subject <- "cvs_avg35_inMNI152"
template_mapping <- "antsRegistrationSyNRepro[a]" #"a: Affine" # s: SYN (SDR)
resample <- FALSE
work_path <- "~/Desktop/junk2"
electrode_file <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/data/demo/YAH/rave/meta/electrodes.csv"

debug <- TRUE
# System preparation
ants <- rpyANTs::load_ants()
antspynet <- rpyANTs::load_antspynet()
py <- rpyANTs::import("__main__", convert = FALSE)

if(length(electrode_file) == 1 && file.exists(electrode_file)) {
  electrode_table <- data.matrix(read.csv(electrode_file)[, c("T1R","T1A","T1S")])
} else {
  electrode_table <- NULL
}


# ensure template
ensure_template <- function(template_code) {
  template_mri_path <- file.path(threeBrain::default_template_directory(), template_code)
  if(!dir.exists(template_mri_path)) {
    threeBrain::download_template_subject(subject_code = template_code)
  }
  dname_prefix <- c("brain.finalsurfs", "brain", "T1")
  fnames <- rbind(
    sprintf("%s.mgz", dname_prefix),
    sprintf("%s.nii.gz", dname_prefix),
    sprintf("%s.nii", dname_prefix)
  )
  template_mri_path <- file.path(template_mri_path, "mri", as.vector(fnames))
  template_mri_path <- template_mri_path[file.exists(template_mri_path)]
  if(!length(template_mri_path)) {
    stop(sprintf("The template that you choose [%s] does not have T1 MRI matching search list", template_code))
  }
  template_mri_path[[1]]
}
template_subject_path <- ensure_template(template_subject)


# prepare working directory
work_path <- raveio::dir_create2(work_path)
volume_path <- raveio::dir_create2(file.path(work_path, "mri"))
transform_path <- raveio::dir_create2(file.path(volume_path, "transforms"))
segmentation_path <- raveio::dir_create2(file.path(volume_path, "segmentation"))
surface_path <- raveio::dir_create2(file.path(work_path, "surf"))

# Read in original image
image_original <- rpyANTs::as_ANTsImage(image_path, strict = TRUE)
image_original$to_file(file.path(volume_path, "orig.nii.gz"))
input_image <- image_original


# Resample
if( resample ) {
  input_image <- rpyANTs::ants_resample_image(
    image_original,
    resample_params = c(256, 256, 256),
    use_voxels = TRUE,
    interp_type = "linear"
  )
}
input_image$to_file(file.path(volume_path, "resampled.nii.gz"))

# Brain mask
probability_mask <- rpyANTs::antspynet_brain_extraction(x = input_image, modality = "t1", verbose = debug)
brain_mask <- ants$threshold_image(probability_mask, 0.5, 1.0, 1L, 0L)
brain_mask = ants$morphology(brain_mask, "close", 6L)$iMath_fill_holes()
brain_mask$to_file(file.path(volume_path, "brain_mask.nii.gz"))

# Skull-strip
skull_strip <- input_image$clone()
skull_strip[brain_mask < 0.5] <- 0L
skull_strip$to_file(file.path(volume_path, "brain.nii.gz"))
if( debug ) {
  input_image$plot(
    overlay = skull_strip,
    ncol = 4L,
    nslices = 12L,
    crop = TRUE,
    overlay_cmap = "jet",
    overlay_alpha = 0.2,
    title = "RAVE-ANTs Brain extraction"
  )
}

# MNI mapping
template_brain <- rpyANTs::as_ANTsImage(template_subject_path)
template_brain$to_file(file.path(transform_path, "template.nii.gz"))

registration = rpyANTs::ants_registration(
  fixed = template_brain,
  moving = skull_strip,
  type_of_transform = template_mapping,
  verbose = debug
)

fwdtransforms <- rpyANTs::py_to_r(registration$fwdtransforms)
for(ii in seq_along(fwdtransforms)) {
  file.copy(fwdtransforms[[ii]], file.path(transform_path, sprintf("fwdtransforms-%d.mat", ii)), overwrite = TRUE)
}
# affine <- solve(rpyANTs::as_ANTsTransform(fwdtransforms)[])
# xfm_path <- file.path(transform_path, "talairach.xfm")

invtransforms <- rpyANTs::py_to_r(registration$invtransforms)
for(ii in seq_along(invtransforms)) {
  file.copy(invtransforms[[ii]], file.path(transform_path, sprintf("invtransforms-%d.mat", ii)), overwrite = TRUE)
}

# morphed to MNI
preprocessed_image = rpyANTs::ants_apply_transforms(
  fixed = template_brain,
  moving = skull_strip,
  verbose = debug,
  transformlist = registration$fwdtransforms,
  interpolator = "linear"
)
preprocessed_image$to_file(file.path(transform_path, "morphed.nii.gz"))

if( debug ) {
  template_brain$plot(
    overlay = preprocessed_image,
    ncol = 4L,
    nslices = 12L,
    crop = TRUE,
    overlay_cmap = "jet",
    overlay_alpha = 0.2,
    title = "RAVE-ANTs Morphed native overlaid on MNI brain"
  )
}

# Image segmentation - Atropos
atropos_segmentation <- rpyANTs::antspynet_deep_atropos(
  x = input_image,
  do_preprocessing = TRUE,
  use_spatial_priors = TRUE,
  aseg_only = FALSE,
  verbose = debug
)
atropos_path <- raveio::dir_create2(file.path(segmentation_path, "atropos"))
atropos_segmentation$segmentation_image$to_file(file.path(atropos_path, "aseg-atropos.nii.gz"))
prob_images <- atropos_segmentation$probability_images
prob_images[1]$to_file(file.path(atropos_path, "prob-atropos-1-csf.nii.gz"))
prob_images[2]$to_file(file.path(atropos_path, "prob-atropos-2-gray-matter.nii.gz"))
prob_images[3]$to_file(file.path(atropos_path, "prob-atropos-3-white-matter.nii.gz"))
prob_images[4]$to_file(file.path(atropos_path, "prob-atropos-4-deep-gray-matter.nii.gz"))
prob_images[5]$to_file(file.path(atropos_path, "prob-atropos-5-brain-stem.nii.gz"))
prob_images[6]$to_file(file.path(atropos_path, "prob-atropos-6-cerebellum.nii.gz"))

if( debug ) {
  input_image$plot(
    overlay = atropos_segmentation$segmentation_image,
    ncol = 4L,
    nslices = 12L,
    crop = TRUE,
    overlay_cmap = "jet",
    overlay_alpha = 0.6,
    title = "RAVE-ANTs Deep Atropos Segmentation"
  )
}

# Image segmentation & parcellation - DKT
DKTatlas_segmentation <- rpyANTs::antspynet_desikan_killiany_tourville_labeling(
  x = input_image,
  do_preprocessing = TRUE, 
  return_probability_images = FALSE,
  do_lobar_parcellation = TRUE, 
  verbose = debug
)

dkt_path <- raveio::dir_create2(file.path(segmentation_path, "DKTatlas"))
DKTatlas_segmentation$segmentation_image$to_file(file.path(dkt_path, "aparc.DKTatlas+aseg.nii.gz"))
DKTatlas_segmentation$segmentation_image$to_file(file.path(volume_path, "aparc.DKTatlas+aseg.nii.gz"))
DKTatlas_segmentation$lobar_parcellation$to_file(file.path(dkt_path, "DKTatlas-lobar.nii.gz"))


if( debug ) {
  input_image$plot(
    overlay = DKTatlas_segmentation$segmentation_image,
    ncol = 4L,
    nslices = 12L,
    crop = TRUE,
    overlay_cmap = "viridis",
    overlay_alpha = 0.6,
    title = "RAVE-ANTs Deep DKT Parcellation"
  )
}


# Generate surface
DTK_lobar <- DKTatlas_segmentation$lobar_parcellation

cortical_mask_left <- DTK_lobar$threshold_image(low_thresh=1L, high_thresh=4L, inval=1L, outval=0L)
cortical_mask_left[brain_mask < 0.5] <- 0L
cortical_mask_left[prob_images[1] > 0.5] <- 0L

cortical_mask_right <- DTK_lobar$threshold_image(low_thresh=7L, high_thresh=10, inval=1L, outval=0L)
cortical_mask_right[brain_mask < 0.5] <- 0L
cortical_mask_right[prob_images[1] > 0.5] <- 0L

cortical_mask_left$to_file(file.path(volume_path, "mask_left.nii.gz"))
cortical_mask_right$to_file(file.path(volume_path, "mask_right.nii.gz"))

# pial
pial_mask <- prob_images[2]$threshold_image(0.5, 1.0, 1L, 0L)
pial_mask_left = pial_mask$clone()
pial_mask_left[cortical_mask_left < 0.5] = 0
# pial_mask_left <- pial_mask_left$morphology("close", 1L)$iMath_fill_holes()

pial_mask_right = pial_mask$clone()
pial_mask_right[cortical_mask_right < 0.5] = 0
# pial_mask_right <- pial_mask_right$morphology("close", 1L)$iMath_fill_holes()

pial_mask_left$to_file(file.path(volume_path, "mask_pial_left.nii.gz"))
pial_mask_right$to_file(file.path(volume_path, "mask_pial_right.nii.gz"))

# white matter
white_matter <- prob_images[3]$threshold_image(0.5, 1.0, 1L, 0L)

white_matter_left = white_matter$clone()
white_matter_left[cortical_mask_left < 0.5] = 0

white_matter_right = white_matter$clone()
white_matter_right[cortical_mask_right < 0.5] = 0

white_matter_left$to_file(file.path(volume_path, "mask_white_left.nii.gz"))
white_matter_right$to_file(file.path(volume_path, "mask_white_right.nii.gz"))

# generate surfaces
IJK2tkrRAS <- threeBrain::get_ijk2ras(input_image, type = "tkr")

generate_surface <- function(mask, name, smooth_delta = 2) {
  surface <- ravetools::mesh_from_volume(
    volume = mask[],
    output_format = "freesurfer",
    IJK2RAS = IJK2tkrRAS,
    threshold = 0.5,
    verbose = debug,
    remesh = TRUE,
    remesh_voxel_size = 1.5,
    smooth = TRUE,
    smooth_delta = smooth_delta,
    smooth_lambda = 1
  )
  freesurferformats::write.fs.surface(
    filepath = file.path(surface_path, name),
    vertex_coords = surface$vertices,
    faces = surface$faces,
    format = "bin"
  )
}
generate_surface(cortical_mask_left, "lh.pial")
generate_surface(cortical_mask_right, "rh.pial")
generate_surface(white_matter_left, "lh.white")
generate_surface(white_matter_right, "rh.white")

brain <- threeBrain::threeBrain(
  path = work_path,
  subject_code = "TEMP",
  surface_types = c("white", "pial"),
  atlas_types = "aparc.DKTatlas+aseg"
)

# Convert electrodes
if(length(electrode_table)) {
  template_brain <- threeBrain::merge_brain(template_subject = template_subject)
  template_object <- template_brain$template_object
  
  electrode_table_LPS <- data.frame(
    x = -electrode_table[,1],
    y = -electrode_table[,2],
    z = electrode_table[,3]
  )
  valid_electrodes <- rowSums(electrode_table_LPS^2) > 0
  
  # trans <- raveio::read_mat2(fwdtransforms)
  
  # If you use affine transform
  mni_coord_lps_py <- ants$apply_transforms_to_points(
    dim = 3L,
    points = electrode_table_LPS,
    transformlist = registration$fwdtransforms,
    verbose = debug, whichtoinvert = list(TRUE))
  
  # # If you use SYN transform
  # mni_coord_lps_py <- ants$apply_transforms_to_points(
  #   dim = 3L,
  #   points = electrode_table_LPS,
  #   transformlist = registration$invtransforms,
  #   verbose = debug)
  
  mni_coord_lps <- rpyANTs::py_to_r(mni_coord_lps_py)
  mni_coord_lps[!valid_electrodes, ] <- 0
  MNI305 <- #template_object$vox2vox_MNI305 %*% 
    rbind(
    -mni_coord_lps$x,
    -mni_coord_lps$y,
    mni_coord_lps$z,
    1
  )
  
  mni_coord <- data.frame(
    Subject = template_object$subject_code,
    Electrode = seq_len(nrow(mni_coord_lps)),
    Coord_x = 1,
    Coord_y = 1,
    Coord_z = 1,
    Label = "NoLabel",
    MNI305_x = MNI305[1,],
    MNI305_y = MNI305[2,],
    MNI305_z = MNI305[3,]
  )
  template_brain$set_electrodes(mni_coord)
  
  if(debug) {
    print(template_brain$plot())
  }
  
  
} else {
  mni_coord <- NULL
}

# native brain
if( debug && length(electrode_table) ) {
  
  IJK2RAS <- threeBrain::get_ijk2ras(input_image)
  native_tkrRAS <- IJK2tkrRAS %*% solve(IJK2RAS) %*% rbind(t(electrode_table), 1)
  valid_electrodes <- rowSums(electrode_table^2) > 0
  native_tkrRAS[, !valid_electrodes] <- 0
  
  brain$set_electrodes(data.frame(
    Electrode = seq_len(ncol(native_tkrRAS)),
    Coord_x = native_tkrRAS[1, ],
    Coord_y = native_tkrRAS[2, ],
    Coord_z = native_tkrRAS[3, ],
    Label = "NoLabel"
  ))
  print(brain$plot())
  
}
