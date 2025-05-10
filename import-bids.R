#' @title Import BIDS data to RAVE (experimental)
#' @date May 09, 2025
#' @author Zhengjia Wang
#' @license Apache-2.0
#' @format
#' Currently supports `BrainVision`, `Matlab` (each channel is a matlab file)
#' 
#' @preparation
#' Please prepare your folder in BIDS format. If your data is on OpenNeuro, run
#' `raveio::load_snippet("download-openneuro")` to see how to download the data
#' from OpenNeuro.
#' 
#' This converter suggests `FreeSurfer` directory and subjects' electrode 
#' coordinate file in `scanRAS` space. Here is an example layout:
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
#' @param rave_subject_prefix (optional, string) subject code prefix in RAVE,
#' default is the BIDS project name followed by an underscore ("_"). 
#' Notice RAVE does not recommend dash in its subject code
#' @param freesurfer_name (optional, string) `FreeSurfer` folder name under
#' the derivatives folder; default is `'freesurfer'`; change this argument 
#' if the folder name includes version numbers (e.g. `freesurfer-7.4.1`)
#' @param override_freesurfer (optional, logical) whether to override existing
#' `FreeSurfer` import if the same subject has been imported to RAVE before;
#' default is `FALSE`. Set to `TRUE` to force update the files
#' 
#' @returns 
#' 
#' 
#' @examples
#' 
#' # You can combine snippet "download-openneuro"
#' openneuro <- raveio::load_snippet("download-openneuro")
#' bids_project_path <- openneuro("ds005953")
#' 
#' print(bids_project_path)
#' 
#' 
#' END OF DOC
NULL

# ---- Global variables --------------------------------------------------------

# bids_project_path <- "~/rave_data/openneuro/ds005953"
# 
# bids_subject_code <- "01"
# 
# 
# # automatically determine the project name from BIDS project path
# rave_project_name <- NA
# rave_subject_prefix <- sprintf("%s_", basename(bids_project_path))
# 
# freesurfer_name <- "surface"
# override_freesurfer <- FALSE


# ---- Code body ---------------------------------------------------------------
# Initialize variables
force(bids_project_path)
force(bids_subject_code)

`%?<-%` <- dipsaus::`%?<-%`
rave_project_name %?<-% NA
rave_subject_prefix %?<-% sprintf("%s_", basename(bids_project_path))
freesurfer_name %?<-% "freesurfer"
override_freesurfer %?<-% FALSE


# Load BIDS project
bids_project <- bidsr::bids_project(bids_project_path)

if(length(rave_project_name) != 1 || is.na(rave_project_name)) {
  rave_project_name <- bids_project$name
}

bids_subject <- bidsr::bids_subject(project = bids_project, subject_code = bids_subject_code)

# Create RAVE subject
# The subject code must start with a letter and cannot be sub-xxx (Reserved for future BIDS support)
rave_subject <- raveio::RAVESubject$new(
  project_name = rave_project_name,
  subject_code = sprintf("%s%s", rave_subject_prefix, bids_subject@subject_code),
  strict = FALSE
)
rave_subject$initialize_paths(include_freesurfer = TRUE)

# Find FreeSurfer
bids_freesurfer_path <- bidsr::resolve_bids_path(x = bids_subject, storage = "derivative", prefix = freesurfer_name)
rave_freesurfer_path <- file.path(rave_subject$imaging_path, "fs")

message("Migrating `freesurfer` directory with overwrite: ", override_freesurfer)

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
    
  } else {
    # Consider Hermes's style of using GIFTI surfaces
    bids_fs_files <- list.files(
      bids_freesurfer_path,
      all.files = FALSE,
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = TRUE,
      no.. = TRUE,
      ignore.case = TRUE,
      pattern = "pial\\.[lr]\\.surf\\.gii$"
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
        raveio::dir_create2(file.path(rave_freesurfer_path, "mri"))
        rave_freesurfer_surfpath <- raveio::dir_create2(file.path(rave_freesurfer_path, "surf"))
        hemisphere_imported <- list(left = FALSE, right = FALSE)
        for(f in bids_fs_files) {
          cat(sprintf("Migrating `%s`\n", f))
          if(endsWith(tolower(f), "r.surf.gii")) {
            surface <- ieegio::as_ieegio_surface(f)
            # transform the space to T1w
            surface$geometry$vertices <- surface$geometry$transforms[[1]] %*% surface$geometry$vertices
            ieegio::write_surface(
              x = surface,
              con = file.path(rave_freesurfer_surfpath, "rh.pial"),
              format = "freesurfer",
              type = "geometry"
            )
            hemisphere_imported$right <- TRUE
          } else {
            surface <- ieegio::as_ieegio_surface(f)
            surface$geometry$vertices <- surface$geometry$transforms[[1]] %*% surface$geometry$vertices
            ieegio::write_surface(
              x = surface,
              con = file.path(rave_freesurfer_surfpath, "lh.pial"),
              format = "freesurfer",
              type = "geometry"
            )
            hemisphere_imported$left <- TRUE
          }
        }
        placeholder_surf <- ieegio::as_ieegio_surface(x = array(0, dim = c(3, 3)),
                                                      faces = matrix(c(1, 2, 3), ncol = 3),
                                                      face_start = 1)
        if(!hemisphere_imported$left) {
          # generate fake left surface
          ieegio::write_surface(
            x = placeholder_surf,
            con = file.path(rave_freesurfer_surfpath, "lh.pial"),
            format = "freesurfer",
            type = "geometry"
          )
        }
        if(!hemisphere_imported$right) {
          # generate fake right surface
          ieegio::write_surface(
            x = placeholder_surf,
            con = file.path(rave_freesurfer_surfpath, "rh.pial"),
            format = "freesurfer",
            type = "geometry"
          )
        }
      }
    }
  }
} else {
  warning("Cannot find BIDS FreeSurfer folder.", immediate. = TRUE)
}

# Load surface object
brain <- threeBrain::threeBrain(
  path = rave_freesurfer_path,
  subject_code = rave_subject$subject_code,
  surface_types = c("pial", "white", "smoothwm", "inflated", "sphere.reg"),
  atlas_types = c("wmparc", "aparc+aseg")
)

# Parse files in BIDS folder
bids_ieeg_files <- bidsr::query_bids(bids_subject, search_params = list(
  storage = "raw",
  data_types = "ieeg",
  suffixes = "electrodes",
  entity_filters = list(
    sub ~ sub == bids_subject_code
  )
))

# Find electrodes
electrode_files <- bids_ieeg_files[bids_ieeg_files$suffix %in% "electrodes", ]
nfound <- 0

if(nrow(electrode_files)) {
  message("Found the following electrode files:")
  print(bids_ieeg_files)
  
  if(length(electrode_files$space)) {
    is_scanner_coord <- is.na(electrode_files$space) | startsWith(tolower(electrode_files$space), "scan")
  } else {
    is_scanner_coord <- rep(TRUE, nrow(electrode_files))
  }
  nfound <- sum(is_scanner_coord)
  if(nfound != 1) {
    if(nfound == 0) {
      warning(sprintf("Cannot properly determine which file contains the T1w scanner coordinate. Found %s files", nfound))
    } else {
      warning(sprintf("Cannot properly determine which file contains the T1w scanner coordinate. Found %s files. Using the first available file", nfound))
    }
  }
} else {
  warning(sprintf("Unable to find any electrode coordinate file for subject `sub-%s`", bids_subject_code))
}

electrode_table <- NULL
if(nfound > 0) {
  # Meaning electrode coordinate files in T1 scanner space is found
  elec_coord_scan <- electrode_files$parsed[is_scanner_coord][[1]]
  elec_coord_scan <- bidsr::resolve_bids_path(bids_project, format(elec_coord_scan))
  
  message("Using BIDS tabular to generate RAVE electrodes.csv: \n\t", elec_coord_scan)
  
  elec_coord_scan_tabular <- bidsr::as_bids_tabular(elec_coord_scan)$content
  
  # Create electrodes.csv from `elec_coord_scan_tabular`
  hemi <- elec_coord_scan_tabular$hemisphere
  if(length(hemi)) {
    hemi <- tolower(hemi)
    hemi[startsWith(hemi, "l")] <- "left"
    hemi[startsWith(hemi, "r")] <- "right"
  } else {
    hemi <- "auto"
  }
  # RAVE uses radius instead of diameters
  size <- as.numeric(elec_coord_scan_tabular$size) / 2
  if(length(size)) {
    size[!is.finite(size) | size <= 0] <- 1
  } else {
    size <- 1
  }
  x <- elec_coord_scan_tabular$x
  x[!is.finite(x)] <- 0
  
  y <- elec_coord_scan_tabular$y
  y[!is.finite(y)] <- 0
  
  z <- elec_coord_scan_tabular$z
  z[!is.finite(z)] <- 0
  
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
  
  if(!is.null(brain)) {
    message("Compute T1w scanner RAS -> FreeSurfer surface coordinates...")
    brain$set_electrodes(electrode_table, coord_sys = "scannerRAS")
    
    # Freesurfer coordinates will be calculated
    electrode_table <- brain$electrodes$raw_table
    
    # Att atlas label
    if(length(destrieux_label_text)) {
      message("`Destrieux_label_text` found... using it.")
      electrode_table$Destrieux_label_text <- destrieux_label_text
    } else {
      aparc <- brain$atlas_types[brain$atlas_types %in% c("wmparc", "aparc+aseg")]
      if(length(aparc)) {
        message("Calculating DKT atlas label.")
        aparc <- aparc[[1]]
        aparc_path <- brain$atlases[[aparc]]$group$group_data$volume_data$absolute_path
        aparc_label <- brain$electrodes$get_atlas_labels(atlas = aparc_path, radius = 2)
        electrode_table$DKT_label_text <- aparc_label$Label1
      }
    }
    
    # Get MNI152 coordinates
    is_mni152_coord <- !is.na(electrode_files$space) & startsWith(tolower(electrode_files$space), "mni152")
    if(length(is_mni152_coord) && sum(is_mni152_coord)) {
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
    } else if(!identical(brain$xfm, diag(1, 4))){
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
  } else {
    electrode_table$Coord_x <- electrode_table$T1R
    electrode_table$Coord_y <- electrode_table$T1A
    electrode_table$Coord_z <- electrode_table$T1S
  }
  
  
  # save 
  raveio::save_meta2(
    data = electrode_table,
    meta_type = "electrodes",
    project_name = rave_subject$project_name,
    subject_code = rave_subject$subject_code
  )
  
  
  # Finally, compute the fsaverage position
  ensure_threeBrain_template <- function(template_name) {
    path <- file.path(threeBrain::default_template_directory(), template_name)
    if(!dir.exists(path)) {
      threeBrain::download_template_subject(template_name)
    }
    invisible(path)
  }
  
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
  } else {
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
  
  message("Saving to `rave/meta/electrodes.csv` with MNI152 and fsaverage mapping (if available).")
  raveio::save_meta2(
    data = electrode_table,
    meta_type = "electrodes",
    project_name = rave_subject$project_name,
    subject_code = rave_subject$subject_code
  )
  
  file.copy(
    file.path(rave_subject$meta_path, "electrodes.csv"),
    file.path(rave_subject$meta_path, "electrodes-from-bids.csv"), 
    overwrite = TRUE
  )
}



# Process signal data
message("Ingesting signals...")
bids_ieeg_files <- bidsr::query_bids(bids_subject, search_params = list(
  storage = "raw",
  data_types = "ieeg",
  suffixes = c("ieeg", "channels", "events"),
  entity_filters = list(
    sub ~ sub == bids_subject_code
  )
))

# case the table to get set of channels, events, and ieeg.xxx
bids_ieeg_files <- reshape2::dcast(
  bids_ieeg_files,
  data_type + sub + ses + task + run ~ sprintf("%s.%s", suffix, extension),
  value.var = "parsed"
)

bids_ieeg_files <- bids_ieeg_files[!is.na(bids_ieeg_files$channels.tsv), ]
n_blocks <- nrow(bids_ieeg_files)
names(bids_ieeg_files) <- tolower(names(bids_ieeg_files))

initial_ingestion <- raveio::with_future_parallel({
  lapply(seq_len(n_blocks), function(ii) {
    # ii <- 1
    sub <- bids_ieeg_files[ii, ]
    channel_path <- bidsr::resolve_bids_path(bids_project, format(sub$channels[[1]]))
    event_path <- bidsr::resolve_bids_path(bids_project, format(sub$events[[1]]))
    
    if(length(sub$ieeg.matlab) && !is.na(sub$ieeg.matlab)) {
      dset_name <- "ieeg.matlab"
    } else if(length(sub$ieeg.vhdr) && !is.na(sub$ieeg.vhdr)) {
      dset_name <- "ieeg.vhdr"
    } else {
      warning("Currently this script only supports official brainvision (vhdr/eeg), EDF, or Matlab formats")
    }
    
    bids_entity <- sub[[dset_name]][[1]]
    
    # get path to the data
    data_path <- bidsr::resolve_bids_path(bids_project, format(bids_entity))
    
    # RAVE block name
    bids_entity@extension <- ""
    block_name <- basename(format(bids_entity))
    block_path <- file.path(rave_subject$preprocess_settings$raw_path, block_name)
    
    message(sprintf("Ingesting block (%d of %d): %s", ii, n_blocks, block_name))
    
    # check event table
    if(!file.exists(event_path)) {
      message("No *event.tsv found in block: ", block_name, "... Skipping...")
      return()
    }
    
    # Generate epoch table
    event_table <- bidsr::as_bids_tabular(event_path)$content
    onset <- event_table$onset
    duration <- event_table$duration
    if(!length(duration)) {
      duration <- NA_real_
    } else {
      duration[!is.finite(duration)] <- NA_real_
    }
    trial_type <- event_table$trial_type
    
    stim_file <- event_table$stim_file
    if(length(stim_file)) {
      stim_file <- vapply(strsplit(stim_file, "/|\\\\"), function(x) { x[[length(x)]] }, "")
      if(!length(trial_type)) {
        trial_type <- stim_file
      }
    } else if(!length(trial_type)) {
      trial_type <- "NoCondition"
    }
    status <- event_table$status
    if(!length(status)) {
      status <- "good"
    }
    epoch_full <- data.frame(
      Block = block_name,
      Trial = seq_len(nrow(event_table)),
      Time = onset,
      Condition = trial_type,
      StimFile = stim_file,
      Status = status,
      Event_offset = onset + duration
    )
    run <- bids_entity$get_bids_entity("run", ifnotfound = 1)
    task <- bids_entity$get_bids_entity("task", ifnotfound = "UnknownTask")
    epoch_full$run <- run
    epoch_full$task <- task
    
    # save epoch for this block
    raveio::safe_write_csv(epoch_full, file.path(rave_subject$meta_path, sprintf("epoch_%s.csv", block_name)))
    
    block_path <- raveio::dir_create2(block_path)
    switch (
      dset_name,
      "ieeg.matlab" = {
        # `name`.mat
        # check the channel path
        if(file.exists(channel_path)) {
          channel_table <- bidsr::as_bids_tabular(channel_path)$content
        } else {
          # stop("TODO: handle BIDS channel paths with shared entities")
          message("No *channel.tsv found in block: ", block_name, "... Skipping...")
          return()
        }
        
        channel_files <- list.files(
          data_path,
          full.names = TRUE,
          all.files = FALSE,
          recursive = FALSE,
          include.dirs = FALSE,
          pattern = "\\.mat$",
          ignore.case = TRUE
        )
        if(!length(channel_files)) {
          message("No data found in block: ", block_name, "... Skipping...")
          return()
        }
        # Make sure we can read the file (or install proper python library)
        if(ii == 1) {
          sample_filepath <- channel_files[[1]]
          cat(sprintf("Trying to read sample file\n  ieegio::io_read_mat('%s')\n", sample_filepath))
          callr::r(
            func = function(path) {
              try({
                ieegio::io_read_mat(path)
              })
            },
            args = list(path = sample_filepath)
          )
        }
        
        channel_information <- raveio::lapply_async(channel_files, function(channel_file) {
          # No print unless it's an error
          utils::capture.output({
            channel_data <- ieegio::io_read_mat(channel_file)
          })
          data_names <- names(channel_data)
          data_sizes <- structure(
            names = data_names,
            sapply(data_names, function(nm) { length(channel_data[[nm]]) })
          )
          data_name <- data_names[which.max(data_sizes)][[1]]
          signal <- as.double(channel_data[[data_name]])
          
          channel_name <- gsub("\\.mat", "", tolower(basename(channel_file)))
          sel <- tolower(electrode_table$Label) %in% channel_name
          if(!any(sel)) {
            msg <- sprintf("Cannot find the channel information for file... make sure the file name is <ChannelName>.mat whose name can be found in electrode table\n\t%s",
                           channel_file)
            # warning(msg)
            return(msg)
          }
          channel_number <- electrode_table$Electrode[sel][[1]]
          
          # get channel information
          channel_info <- channel_table[tolower(channel_table$name) %in% channel_name, ]
          if(nrow(channel_info)) {
            channel_info <- as.list(channel_info[1, ])
          } else {
            channel_info <- list()
          }
          channel_info$channel <- channel_number
          channel_info_json <- jsonlite::toJSON(channel_info, auto_unbox = TRUE)
          dst_filename <- gsub("\\.mat", sprintf("_ch%d.h5", channel_number), basename(channel_file), ignore.case = TRUE)
          dst_path <- file.path(block_path, dst_filename)
          
          ieegio::io_write_h5(x = channel_info_json, file = dst_path, name = "meta", replace = TRUE, quiet = TRUE)
          ieegio::io_write_h5(x = signal, file = dst_path, name = "data", replace = TRUE, ctype = "numeric", quiet = TRUE)
          
          # print(channel_info_json)
          
          list(
            channel = channel_number,
            name = channel_info$name,
            type = c(channel_info$type, "SEEG")[[1]],
            sampling_frequency = c(channel_info$sampling_frequency, NA)[[1]]
          )
        }, callback = function(channel_file) {
          sprintf("%s|%s", block_name, basename(channel_file))
        })
        
        is_warning_msg <- vapply(channel_information, is.character, FALSE)
        warning_msgs <- channel_information[is_warning_msg]
        if(length(warning_msgs)) {
          warning(paste(warning_msgs, collapse = "\n"), immediate. = TRUE)
        }
        channel_information <- channel_information[!is_warning_msg]
        channel_information <- data.table::rbindlist(channel_information)
        
      },
      "ieeg.vhdr" = {
        # brainvision file
        cache <- ieegio::read_brainvis(data_path, extract_path = block_path)
        # check the channel path
        if(file.exists(channel_path)) {
          channel_table <- bidsr::as_bids_tabular(channel_path)$content
        } else {
          channel_table <- cache$channel_table
        }
        
        channel_information <- raveio::lapply_async(
          seq_len(nrow(channel_table)), function(od) {
            label <- channel_table$name[[od]]
            type <- channel_table$type
            if(length(type)) {
              type <- type[[od]]
            } else {
              type <- "SEEG"
            }
            
            dst_path <- file.path(block_path, sprintf("label-%s_channel-%s.h5", label, od))
            
            channel <- cache$get_channel(od)
            signal <- channel$value
            channel_info_json <- jsonlite::toJSON(channel$info, auto_unbox = TRUE)
            ieegio::io_write_h5(x = channel_info_json, file = dst_path, name = "meta", replace = TRUE, quiet = TRUE)
            ieegio::io_write_h5(x = signal, file = dst_path, name = "data", replace = TRUE, ctype = "numeric", quiet = TRUE)
            list(
              channel = od,
              name = label,
              type = type,
              sampling_frequency = c(channel$info$SampleRate, NA)[[1]]
            )
          },
          callback = function(od) {
            sprintf("Extracting channel|%s (%s)", od, block_name)
          }
        )
        
        channel_information <- data.table::rbindlist(channel_information)
        
      }
    )
    
    
    
    list(
      block = block_name,
      epoch = epoch_full,
      channels = channel_information
    )
    
  })
})

all_blocks <- sapply(initial_ingestion, "[[", "block")

# Construct epochs combined all blocks
epoch_full <- data.table::rbindlist(lapply(initial_ingestion, "[[", "epoch"))
n_trials <- nrow(epoch_full)
epoch_full$Trial <- seq_len(n_trials)
good_status <- epoch_full$Status %in% "good"
message("Found ", n_trials, " trials in total, ", sum(good_status), " good trials.")

message("Saving the full epoch to `meta/epoch_full.csv`")
raveio::safe_write_csv(epoch_full, file = file.path(rave_subject$meta_path, "epoch_full.csv"))

message("Saving the good-trial epoch to `meta/epoch_status_good.csv`")
epoch_good <- epoch_full[good_status, ]
epoch_good$Trial <- seq_along(epoch_good$Trial)
raveio::safe_write_csv(epoch_good, file = file.path(rave_subject$meta_path, "epoch_status_good.csv"))

# Find channels and types
channel_tables <- Reduce(
  x = lapply(initial_ingestion, "[[", "channels"),
  f = function(x, y) {
    re <- merge(x, y, by = c("channel", "name"), all = TRUE, suffixes = c("", ".y"))
    as.data.frame(re)[, c("channel", "name", "type", "sampling_frequency")]
  }
)

is_seeg_channel <- tolower(channel_tables$type) %in% c("seeg", "ecog", "ieeg", "dbs")
seeg_srate <- channel_tables$sampling_frequency[is_seeg_channel]
unique_srates <- unique(seeg_srate)

if(length(unique_srates) > 1) {
  message("Found more than one sEEG sampling frequencies: ", paste(unique_srates, collapse = ", "), "... Trying to figure out the proper one...")
  count <- sapply(unique_srates, function(srate) {
    sum(seeg_srate == srate)
  })
  unique_srates <- unique_srates[order(count, decreasing = TRUE)][[1]]
}
seeg_srate <- unique_srates
is_seeg_channel <- is_seeg_channel & channel_tables$sampling_frequency == seeg_srate

seeg_channels <- channel_tables$channel[is_seeg_channel]
seeg_names <- channel_tables$name[is_seeg_channel]

message("Found sEEG channels: ", dipsaus::deparse_svec(seeg_channels), "; sampling frequency will be ", seeg_srate, " Hz; names are:\n  ", paste(seeg_names, collapse = ", "))


aux_channels <- channel_tables$channel[!is_seeg_channel]
aux_names <- channel_tables$name[!is_seeg_channel]
if(length(aux_channels)) {
  aux_srate <- min(channel_tables$sampling_frequency[!is_seeg_channel])
  message("Found aux channels: ", dipsaus::deparse_svec(aux_channels), "; sampling frequency will be ", aux_srate, " Hz; names are:\n  ", paste(aux_names, collapse = ", "))
} else {
  aux_srate <- seeg_srate
}

# save.image(file = "~/Downloads/junk.RData")
# load("~/Downloads/junk.RData")
message("Ramping up RAVE: importing signal data...")
pipeline <- raveio::pipeline("import_lfp_native")
import_lfp_native <- pipeline$fork_to_subject(subject = rave_subject, label = "converted_from_bids")

import_lfp_native$set_settings(
  skip_validation = TRUE,
  force_import = TRUE,
  import_setup__project_name = rave_subject$project_name,
  import_setup__subject_code = rave_subject$subject_code,
  import_blocks__session_block = all_blocks,
  import_blocks__format = ".mat/.h5 file per electrode per block",
  
  # Backward compatible
  import_channels__unit = "NA",
  import_channels__sample_rate = seeg_srate,
  import_channels__electrodes = dipsaus::deparse_svec(seeg_channels),
  import_channels__electrode_file = "auto",
  
  # LFP macro channels
  import_channels__lfp_unit = "NA",
  import_channels__lfp_sample_rate = seeg_srate,
  import_channels__lfp_channels = dipsaus::deparse_svec(seeg_channels),
  
  # Aux
  import_channels__auxiliary_unit = "NA",
  import_channels__auxiliary_sample_rate = aux_srate,
  import_channels__auxiliary_channels = dipsaus::deparse_svec(aux_channels),
  
  # micro, not used
  import_channels__microwire_unit = "NA",
  import_channels__microwire_sample_rate = aux_srate,
  import_channels__microwire_channels = "",
  
  # No composed channel
  import_channels__compose_setup = list(),
  compose_setup = list()
)

import_lfp_native$run()

electrode_coords <- file.path(rave_subject$meta_path, "electrodes-from-bids.csv")
electrode_coords_new <- file.path(rave_subject$meta_path, "electrodes.csv")
if(file.exists(electrode_coords)) {
  raveio::backup_file(electrode_coords_new)
  file.copy(electrode_coords, electrode_coords_new, overwrite = TRUE)
}


cli::cli_h1(sprintf("Done importing RAVE subject: `%s`", rave_subject$subject_id))
cli::cli_alert_info("Please run {.run rave::start_rave()} to process this subject")

# message("RAVE: Notch-filter...")
# # RAVE's notch filter is essentially FIR filter that can run before reference
# 
# pipeline <- raveio::pipeline("notch_filter")
# notch_filter <- pipeline$fork_to_subject(subject = rave_subject, label = "converted_from_bids")
# 
# notch_filter$set_settings(
#   channel_types = "LFP",
#   subject_code = rave_subject$subject_code,
#   project_name = rave_subject$project_name,
#   notch_filter_upperbound = c(61, 122, 182),
#   notch_filter_lowerbound = c(59, 118, 178)
# )
# 
# notch_filter$run("apply_notch")

# message("RAVE: Wavelet...")
# 
# pipeline <- raveio::pipeline("wavelet_module")
# wavelet_module <- pipeline$fork_to_subject(subject = rave_subject, label = "nsd_convert")
# 
# wavelet_module$set_settings(
#   kernel_table = list(
#     Frequency = c(2L, 12L, 22L, 32L, 42L, 52L, 62L, 72L, 82L, 92L, 102L, 112L, 122L, 132L, 142L, 152L, 162L, 172L, 182L, 192L),
#     Cycles = c(3, 6, 8, 9, 11, 11, 12, 13, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20)
#   ),
#   pre_downsample = 1,
#   precision = "float",
#   subject_code = rave_subject$subject_code,
#   project_name = rave_subject$project_name,
#   target_sample_rate = 100
# )
# 
# wavelet_module$run("wavelet_params")

