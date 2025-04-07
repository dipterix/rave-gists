#' @title Import `NSD` electrode data and generate RAVE viewer
#' @date 2025-04-07
#' @author Zhengjia Wang
#' @license Apache-2.0
#' @description
#' Import the FreeSurfer reconstruction files and electrode coordinates from
#' `NSD` iEEG dataset (BIDS format) to RAVE.
#' 
#' * Please make sure RAVE is up-to-date (see the date in this help function).
#'   If you installed RAVE prior to this date, please update RAVE via 
#'        `ravemanager::update_rave()`
#'   or check the full installation guide at
#'        https://rave.wiki/posts/installation/update.html
#' 
#' @preparation
#' Please prepare your `NSD` folder in BIDS format. This converter requires
#' `FreeSurfer` directory and subjects' electrode coordinate file in 
#' `scanRAS` space. Here is an example layout:
#' 
#' NSDiEEG_dataset (root)/
#' ├─sub-01
#' │ └─ses-ieeg01/
#' │   └─ieeg/
#' |     └─sub-01_ses-ieeg01_electrodes.tsv  <- electrode coordinates in T1w
#' ├─sub-02/
#' └─derivatives/
#'   └─freesurfer/
#'     ├─sub-01/                             <- Subject's FreeSurfer directory
#'     │ ├─mri/   
#'     │ ├─surf/
#'     │ ├─label
#'     │ └─...
#'     └─sub-02/
#'     
#' @param bids_project_path (mandatory, string) absolute path to the BIDS 
#' dataset root directory (that contains `datasets_description.json`)
#' @param bids_subject_code (mandatory, string) subject code, with or without 
#' the leading `sub-`
#' @param rave_project_name (optional, string) project name in RAVE, default is 
#' the folder name of the `bids_project_path`
#' @param freesurfer_name (optional, string) `FreeSurfer` folder name under
#' the derivatives folder; default is `'freesurfer'`; change this argument 
#' if the folder name includes version numbers (e.g. `freesurfer-7.4.1`)
#' @param override_freesurfer (optional, logical) whether to override existing
#' `FreeSurfer` import if the same subject has been imported to RAVE before;
#' default is `FALSE`. Set to `TRUE` to force update the files
#' @param save_to_bids (optional, logical) whether to save files back to
#' BIDS derivatives folder; default is `TRUE`, which saves the `RAVE`-formatted
#' `electrodes.csv` and the final viewer under the `derivatives/rave/` folder.
#' 
#' @returns A list of paths where the `electrodes.csv` and RAVE-3DViewer is
#' stored. 
#' 
#' 
#' @examples
#' 
#' To run the script, you can either download the script, uncomment the 
#' `Global variables` section below, or use `ravepipeline::load_snippet`
#' function:
#' 
#' 
#' import_nsd_electrodes <- ravepipeline::load_snippet("import-nsd-electrodes")
#' 
#' # Print this documentation
#' print(import_nsd_electrodes)
#' 
#' import_nsd_electrodes(
#'   bids_project_path = "~/Downloads/nsd_data/NSDiEEG",
#'   bids_subject_code = "06", save_to_bids = FALSE)
#' #> Migrating `freesurfer` directory with overwrite: FALSE
#' #>   Source: ~/Downloads/nsd_data/NSDiEEG/derivatives/freesurfer/sub-06
#' #>   Destination: ~/rave_data/raw_dir/S06/rave-imaging/fs
#' #> Found the following electrode coordinate files:
#' #>          parsed data_type     suffix extension    sub    ses          space
#' #>          <AsIs>    <char>     <char>    <char> <char> <char>         <char>
#' #> 1: sub-06/s....      ieeg electrodes       tsv     06 ieeg01           <NA>
#' #> 2: sub-06/s....      ieeg electrodes       tsv     06 ieeg01 MNI152NLin2009
#' #> 3: sub-06/s....      ieeg electrodes       tsv     06 ieeg01         MNI305
#' #> Using BIDS tabular to generate RAVE electrodes.csv: 
#' #>  ~/Downloads/nsd_data/NSDiEEG/./sub-06/ses-ieeg01/ieeg/sub-06_ses-ieeg01_electrodes.tsv
#' #> Compute T1w scanner RAS -> FreeSurfer surface coordinates...
#' #> `Destrieux_label_text` found... using existing labeling.
#' #> Found MNI152 from BIDS tabular
#' #>  ~/Downloads/nsd_data/NSDiEEG/./sub-06/ses-ieeg01/ieeg/sub-06_ses-ieeg01_space-MNI152NLin2009_electrodes.tsv
#' #> Saving the electrode coordinates
#' #> Computing mapping to fsaverage by projecting contacts to surface: smoothwm
#' #> Saving to `rave/meta/electrodes.csv` with MNI152 and fsaverage mapping.
#' #> Saving the viewer to: 
#' #>   ~/rave_data/raw_dir/S06/rave-imaging/nsd_import.html
#' 
#' 
#' END OF DOC
NULL


# ---- Global variables --------------------------------------------------------

# # Mandatory inputs
# bids_project_path <- "~/Downloads/nsd_data/NSDiEEG"
# bids_subject_code <- "06"
# 
# # Optional inputs: uncomment to change
# rave_project_name <- NA
# freesurfer_name <- "freesurfer"
# override_freesurfer <- FALSE
# save_to_bids <- TRUE

# ---- Code body ---------------------------------------------------------------

# Make sure inputs are properly set
force(bids_project_path)
force(bids_subject_code)

`%?<-%` <- dipsaus::`%?<-%`

rave_project_name %?<-% NA
freesurfer_name %?<-% "freesurfer"
override_freesurfer %?<-% FALSE
save_to_bids %?<-% TRUE


# ensure fsaverage template; download if not exists
ensure_threeBrain_template <- function(template_name) {
  path <- file.path(threeBrain::default_template_directory(), template_name)
  if(!dir.exists(path)) {
    options(timeout = 3600)
    threeBrain::download_template_subject(template_name)
  }
  invisible(path)
}

ensure_threeBrain_template("fsaverage")


# Load BIDS project
bids_project <- bidsr::bids_project(bids_project_path)

# Determine the RAVE project name; default is from the BIDS dataset name
if(length(rave_project_name) != 1 || is.na(rave_project_name)) {
  rave_project_name <- bids_project$name
}

# Create BIDS subject instance for dataset query
bids_subject <- bidsr::bids_subject(project = bids_project, subject_code = bids_subject_code)

# Create RAVE subject
# The subject code must start with a letter and cannot be sub-xxx (Reserved for future BIDS support)
# sub-06 will be `S06` in RAVE
rave_subject <- raveio::RAVESubject$new(
  project_name = rave_project_name,
  subject_code = sprintf("S%s", bids_subject@subject_code),
  strict = FALSE
)

# Initialize paths
rave_subject$initialize_paths(include_freesurfer = TRUE)

# Find FreeSurfer folder in the freesurfer folder
bids_freesurfer_path <- bidsr::resolve_bids_path(x = bids_subject,
                                                 storage = "derivative",
                                                 prefix = freesurfer_name)

# FreeSurfer destination in the RAVE folder
rave_freesurfer_path <- file.path(rave_subject$imaging_path, "fs")

# Print out message so user can see what's happening
message(sprintf(
  "Migrating `freesurfer` directory with overwrite: %s\n  Source: %s\n  Destination: %s",
  override_freesurfer,
  bids_freesurfer_path,
  rave_freesurfer_path
))


if(dir.exists(bids_freesurfer_path)) {
  bids_fs_files <- list.files(
    bids_freesurfer_path,
    all.files = FALSE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE,
    ignore.case = TRUE,
    pattern = "(label|mri|surf|RAVE)$"
  )
  if(length(bids_fs_files)) {
    
    if(!override_freesurfer) {
      if( dir.exists(rave_freesurfer_path) ) {
        brain <- threeBrain::threeBrain(
          path = rave_freesurfer_path,
          subject_code = rave_subject$subject_code,
          surface_types = c("pial", "white", "smoothwm", "inflated", "sphere.reg"),
          atlas_types = c("wmparc", "aparc+aseg")
        )
        if(is.null(brain)) {
          override_freesurfer <- TRUE
        }
      }
    }
    # copy the files to FreeSurfer path
    if(override_freesurfer || !file.exists(rave_freesurfer_path)) {
      # Do not delete, just rename instead
      raveio::backup_file(rave_freesurfer_path, remove = TRUE)
      rave_freesurfer_path <- raveio::dir_create2(rave_freesurfer_path)
      for(f in bids_fs_files) {
        cat(sprintf("Migrating `%s`\n", f))
        file.copy(f, rave_freesurfer_path, recursive = TRUE)
      }
    }
    
  }
} else {
  if(!dir.exists(rave_freesurfer_path)) {
    stop("BIDS FreeSurfer derivative path is missing. Please check the FreeSurfer path (printed above)")
  }
  warning("Cannot find BIDS FreeSurfer folder. Using existing copy in RAVE", immediate. = TRUE)
}

# Load surface object
brain <- threeBrain::threeBrain(
  path = rave_freesurfer_path,
  subject_code = rave_subject$subject_code,
  surface_types = c("pial", "white", "smoothwm", "inflated", "sphere.reg"),
  atlas_types = c("wmparc", "aparc+aseg")
)

if(is.null(brain)) {
  stop("Unable to obtain the FreeSurfer models. Please make sure the FreeSurfer folder is imported correctly.")
}

# Parse files in BIDS folder with filters (for the subject)
# datatype: ieeg (raw)
# suffix+extension: _electrodes.tsv
bids_ieeg_files <- bidsr::query_bids(bids_subject, search_params = list(
  storage = "raw",
  data_types = "ieeg",
  suffixes = "electrodes"
))

bids_ieeg_files <- bids_ieeg_files[bids_ieeg_files$sub %in% bids_subject$subject_code, ]

message("Found the following electrode coordinate files:")
print(bids_ieeg_files)

# Find electrodes
electrode_files <- bids_ieeg_files[bids_ieeg_files$suffix %in% "electrodes", ]

if(!nrow(electrode_files)) {
  stop(sprintf("Unable to find any electrode coordinate file for subject `sub-%s`", bids_subject_code))
}

# Find *_electrodes.tsv in scanner coordinate (native space)
# Either the space is scanRAS or missing (default is T1w)
is_scanner_coord <- is.na(electrode_files$space) | startsWith(tolower(electrode_files$space), "scan")
nfound <- sum(is_scanner_coord)
if(nfound != 1) {
  if(nfound == 0) {
    stop(sprintf("Cannot properly determine which file contains the T1w scanner coordinate. Found %s files", nfound))
  } else {
    warning(sprintf("Cannot properly determine which file contains the T1w scanner coordinate. Found %s files. Using the first available file", nfound))
  }
}
elec_coord_scan <- electrode_files$parsed[is_scanner_coord][[1]]
elec_coord_scan <- bidsr::resolve_bids_path(bids_project, format(elec_coord_scan))

message("Using BIDS tabular to generate RAVE electrodes.csv: \n\t", elec_coord_scan)

elec_coord_scan_tabular <- bidsr::as_bids_tabular(elec_coord_scan)$content

# Create electrodes.csv from `elec_coord_scan_tabular`
# Translate hemisphere 
hemi <- elec_coord_scan_tabular$hemisphere
if(length(hemi)) {
  hemi <- tolower(hemi)
  hemi[startsWith(hemi, "l")] <- "left"
  hemi[startsWith(hemi, "r")] <- "right"
} else {
  hemi <- "auto"
}
# Translate size: Surface area of the electrode, units MUST be in mm^2.
# Assuming the size is 4pi*r^2; default is 1mm (we need diameter, or 2*r)
size <- as.numeric(elec_coord_scan_tabular$size) * 0.16
if(length(size)) {
  size[!is.finite(size) | size <= 0] <- 1
} else {
  size <- 1
}

# Make sure the xyz make sense, RAVE uses exact 0, 0, 0 for invalid electrodes
x <- elec_coord_scan_tabular$x
x[!is.finite(x)] <- 0

y <- elec_coord_scan_tabular$y
y[!is.finite(y)] <- 0

z <- elec_coord_scan_tabular$z
z[!is.finite(z)] <- 0

# Check if `Destrieux_label_text` exists
destrieux_label_text <- elec_coord_scan_tabular$Destrieux_label_text

electrode_table <- data.frame(
  Electrode = seq_len(nrow(elec_coord_scan_tabular)),
  Label = elec_coord_scan_tabular$name,
  T1R = x,
  T1A = y,
  T1S = z,
  ElectrodeShaft = gsub("[0-9]+$", "", elec_coord_scan_tabular$name),
  Hemisphere = hemi,
  Radius = size
)

message("Compute T1w scanner RAS -> FreeSurfer surface coordinates...")
brain$set_electrodes(electrode_table, coord_sys = "scannerRAS")

# Freesurfer coordinates will be calculated
electrode_table <- brain$electrodes$raw_table

# Atlas label
if(length(destrieux_label_text)) {
  message("`Destrieux_label_text` found... using existing labeling.")
  electrode_table$Destrieux_label_text <- destrieux_label_text
} else {
  aparc <- brain$atlas_types[brain$atlas_types %in% c("wmparc", "aparc+aseg")]
  if(length(aparc)) {
    message("Calculating DKT atlas label.")
    try({
      aparc <- aparc[[1]]
      aparc_path <- brain$atlases[[aparc]]$group$group_data$volume_data$absolute_path
      aparc_label <- brain$electrodes$get_atlas_labels(atlas = aparc_path, radius = 2)
      electrode_table$DKT_label_text <- aparc_label$Label1
    })
  }
}

# Will be used later to generate viewer
atlas_label_name <- c("Destrieux_label_text", "DKT_label_text")
atlas_label_name <- atlas_label_name[atlas_label_name %in% names(electrode_table)]
if(length(atlas_label_name)) {
  atlas_label_name <- atlas_label_name[[1]]
  electrode_table$FSLabel <- electrode_table[[atlas_label_name]]
  atlas_label_name <- "FSLabel"
} else {
  atlas_label_name <- "[None]"
}

# Get MNI152 coordinates with space-MNI152xxx entity
is_mni152_coord <- !is.na(electrode_files$space) & startsWith(tolower(electrode_files$space), "mni152")
if(sum(is_mni152_coord)) {
  elec_coord_152 <- electrode_files$parsed[is_mni152_coord][[1]]
  elec_coord_152 <- bidsr::resolve_bids_path(bids_project, format(elec_coord_152))
  message("Found MNI152 from BIDS tabular\n\t", elec_coord_152)
  elec_coord_152_tabular <- bidsr::as_bids_tabular(elec_coord_152)$content
  mni152 <- sapply(electrode_table$Label, function(name) {
    sel <- which(elec_coord_152_tabular$name == name)
    if(length(sel)) {
      sel <- sel[[1]]
    } else {
      return(c(0, 0, 0))
    }
    sub <- elec_coord_152_tabular[sel, ]
    xyz <- c(sub$x, sub$y, sub$z)
    if(!all(is.finite(xyz))) {
      xyz <- c(0, 0, 0)
    }
    xyz
  })
  invalids <- colSums(mni152^2) == 0
  mni305 <- solve(raveio::MNI305_to_MNI152) %*% rbind(mni152, 1)
  mni305[, invalids] <- 0
  
  electrode_table$MNI305_x <- mni305[1, ]
  electrode_table$MNI305_y <- mni305[2, ]
  electrode_table$MNI305_z <- mni305[3, ]
  
  electrode_table$MNI152_x <- mni152[1, ]
  electrode_table$MNI152_y <- mni152[2, ]
  electrode_table$MNI152_z <- mni152[3, ]
} else {
  message("Unable to find MNI152 from BIDS tabular... Using rough estimation from Affine transform...")
  scan_ras <- electrode_table[, c("T1R", "T1A", "T1S")]
  invalids <- rowSums(scan_ras^2) == 0
  mni305 <- t(brain$electrodes$apply_transform_points(scan_ras, from = "scannerRAS", to = "MNI305"))
  mni305[, invalids] <- 0
  mni152 <- t(brain$electrodes$apply_transform_points(scan_ras, from = "scannerRAS", to = "MNI152"))
  mni152[, invalids] <- 0
  
  electrode_table$MNI305_x <- mni305[1, ]
  electrode_table$MNI305_y <- mni305[2, ]
  electrode_table$MNI305_z <- mni305[3, ]
  
  electrode_table$MNI152_x <- mni152[1, ]
  electrode_table$MNI152_y <- mni152[2, ]
  electrode_table$MNI152_z <- mni152[3, ]
}

# save 
message("Saving the electrode coordinates ")
raveio::save_meta2(
  data = electrode_table,
  meta_type = "electrodes",
  project_name = rave_subject$project_name,
  subject_code = rave_subject$subject_code
)

surface_types <- brain$surface_types
if(all(c("pial", "sphere.reg") %in% surface_types)) {
  project_surface <- c("smoothwm", "white", "pial")
  project_surface <- project_surface[project_surface %in% surface_types][[1]]
  message("Computing mapping to fsaverage by projecting contacts to surface: ", project_surface)
  surface_mapping <- raveio::transform_point_to_template(subject = rave_subject, mapping_method = "surface", project_surface = project_surface)
  
  surface_mapping[invalids, ] <- 0
  
  electrode_table$DistanceShifted <- surface_mapping$DistanceShifted
  electrode_table$Sphere_x <- surface_mapping$Sphere_x
  electrode_table$Sphere_y <- surface_mapping$Sphere_y
  electrode_table$Sphere_z <- surface_mapping$Sphere_z
  
  message("Saving to `rave/meta/electrodes.csv` with MNI152 and fsaverage mapping.")
  raveio::save_meta2(
    data = electrode_table,
    meta_type = "electrodes",
    project_name = rave_subject$project_name,
    subject_code = rave_subject$subject_code
  )
} else {
  warning("Unable to find `surf/*h.sphere.reg` from the FreeSurfer path... No surface mapping will be calculated", immediate. = TRUE)
  # # check if we can find it in NSD data
  # is_mni305_coord <- !is.na(electrode_files$space) & startsWith(tolower(electrode_files$space), "mni305")
  # if(sum(is_mni305_coord)) {
  #   elec_coord_fsaverage <- electrode_files$parsed[is_mni305_coord][[1]]
  #   elec_coord_fsaverage <- bidsr::resolve_bids_path(bids_project, format(elec_coord_fsaverage))
  #   elec_coord_fsaverage_tabular <- bidsr::as_bids_tabular(elec_coord_fsaverage)$content
  #   
  #   fsaverage_path <- ensure_threeBrain_template("fsaverage")
  #   
  #   threeBrain::
  #   
  #   
  #   mni152 <- sapply(electrode_table$Label, function(name) {
  #     sel <- which(elec_coord_152_tabular$name == name)
  #     if(length(sel)) {
  #       sel <- sel[[1]]
  #     } else {
  #       return(c(0, 0, 0))
  #     }
  #     sub <- elec_coord_152_tabular[sel, ]
  #     xyz <- c(sub$x, sub$y, sub$z)
  #     if(!all(is.finite(xyz))) {
  #       xyz <- c(0, 0, 0)
  #     }
  #     xyz
  #   })
  # }
}

# save copies
if( save_to_bids ) {
  rave_bids_path <- bidsr::resolve_bids_path(bids_subject, storage = "derivative", prefix = "rave")
  meta_path <- raveio::dir_create2(file.path(rave_bids_path, "meta"))
  meta_path_electrodes <- file.path(meta_path, "electrodes.csv")
  utils::write.csv(x = electrode_table, file = meta_path_electrodes)
}


# Saving a viewer version
brain <- raveio::rave_brain(
  subject = rave_subject,
  surfaces = c("pial", "white", "smoothwm", "inflated", "sphere.reg"),
  overlays = c("aparc.a2009s+aseg", "wmparc"),
  annotations = "label/aparc.a2009s"
)

brain$set_electrode_values(table_or_path = electrode_table[!invalids, ])
viewer <- brain$plot(
  controllers = list(
    "Display Data" = "ElectrodeShaft",
    "Additional Data" = atlas_label_name, 
    "Volume Mapping" = "sphere.reg",
    "Projection Threshold" = 3
  )
)

if( save_to_bids ) {
  rave_bids_path <- bidsr::resolve_bids_path(bids_subject, storage = "derivative", prefix = "rave")
  viewer_folder <- raveio::dir_create2(file.path(rave_bids_path, "exports", "viewers"))
  viewer_path <- file.path(viewer_folder, "nsd_import.html")
  message("Saving the viewer to: \n  ", viewer_path)
  threeBrain::save_brain(widget = viewer, path = viewer_path, title = "NSD-RAVE")
} else {
  viewer_path <- file.path(rave_subject$imaging_path, "nsd_import.html")
  message("Saving the viewer to: \n  ", viewer_path)
  threeBrain::save_brain(widget = viewer, path = viewer_path, title = "NSD-RAVE")
}


# For running as RAVE snippet
invisible(list(
  subject = rave_subject$subject_id,
  electrode_path = file.path(rave_subject$meta_path, "electrodes.csv"),
  viewer_path = viewer_path
))
