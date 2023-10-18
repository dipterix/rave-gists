#' @author Zhengjia Wang
#' @date July 09, 2023
#' @license Apache-2.0
#' 
#' @title Convert `FieldTrip` coordinates to RAVE electrode CSV
#' @description
#' Imports and convert `FieldTrip` electrode coordinate file to RAVE.
#' If the ACPC is re-aligned, make sure you use the aligned MRI for 
#' visualization
#' 
#' @param rave_project RAVE project for searching the `FreeSurfer` files and
#' storing electrode table; default is `"YAEL"`
#' @param subject_code RAVE subject code
#' @param fspath (optional) If `FreeSurfer` files are not at the default path, 
#' where can I find it?
#' @param fieldtrip_coordpath path to `FieldTrip` electrode coordinate file,
#' Supported Matlab format is 5.0, please make sure `FieldTrip` coordinate
#' unit is `mm`, coordsys is `acpc` or `scanras`.
#' @param save_path where to save electrode table to? Default is 
#' `~/rave_data/data_dir/<rave_project>/<subject_code>/rave/meta/electrodes.csv`
#' @param preview Whether to preview the brain; default is true in interactive
#' mode
#' 
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

# rave_project <- "YAEL"
# subject_code <- "alexis"
# fspath <- "~/rave_data/raw_dir/alexis/rave-imaging/fs/"
# 
# # Only support Matlab 5.x formats
# fieldtrip_coordpath <- "~/Downloads/ft_data_ieegLoc/elec_acpc_f.mat"
# 
# save_path <- NULL
# preview <- FALSE

# ---- code body ---------------------------------------------------------------

# ---- Default arguments
`%?<-%` <- dipsaus::`%?<-%`
rave_project %?<-% "YAEL"
save_path %?<-% NULL
preview %?<-% interactive()


# ---- Load brain (in multiple ways)
# If `fspath` is not explicitly given, try to get from RAVE file structure
fspath %?<-% NA

subject <- raveio::RAVESubject$new(project_name = rave_project,
                                   subject_code = subject_code,
                                   strict = FALSE)
if( length(fspath) != 1 || is.na(fspath) || !file.exists(fspath) ) {
  # check if brain is prepared in RAVE
  fspath <- subject$freesurfer_path
}
brain <- threeBrain::threeBrain(path = fspath, subject_code = subject_code)
if(is.null(brain)) {
  stop("Invalid FreeSurfer path. Cannot convert the Fieldtrip coordinates. Please explicitly specify the FreeSurfer path using `fspath` argument, or copy the `fs` folder into\n  ", 
       file.path(subject$preprocess_settings$raw_path, "rave-imaging"))
}

# ---- Load fieldTrip coordinate files
if(!file.exists(fieldtrip_coordpath)) {
  stop("Invalid fieldTrip file (missing path)")
}

tryCatch({
  ft_coords <- raveio::read_mat2(file = fieldtrip_coordpath, ram = TRUE, engine = "r")
}, error = function(e) {
  stop("Cannot load fieldTrip file. If you are sure the fieldTrip file is valid, please contact the author(s) for additional support.")
})
ft_coords <- as.list(ft_coords)[[1]]
ft_coords <- ft_coords[,,1]

# Coordinate system checking
coordsys <- unlist(ft_coords$coordsys)
if(!isTRUE(coordsys %in% c("acpc", "scanras"))) {
  stop("Supported `coordsys` are 'acpc' or 'scanras'. Please contact the author(s) for additional support.")
}

# Channel labels
label <- unname(unlist(ft_coords$label))

# Get electrode coordinates in ACPC/scanRAS space
chanpos <- ft_coords$chanpos
elecpos <- ft_coords$elecpos
if( !length(chanpos) || !is.matrix(chanpos) ) {
  if( length(elecpos) && is.matrix(elecpos) ) {
    chanpos <- elecpos
  }
  
} else if( is.matrix(elecpos) && 
           (nrow(chanpos) != length(label) && length(label) == nrow(elecpos)) ){
  chanpos <- elecpos
}
if(!is.matrix(chanpos) && ncol(chanpos) < 3) {
  stop("Cannot find valid `chanpos` from the fieldTrip coordinate file.")
}
nelec <- nrow(chanpos)

# Trim channel labels if the lengths are inconsistent 
# (TODO: this might need adjustment using ft_coords$tra 
#        if I have time to dig into the ft format)
if(length(label) > nelec) {
  label <- label[seq_len(nelec)]
} else if(length(label) < nelec){
  label <- c(label, rep("Unknown", nelec - length(label)))
}

# Conversion
tkrRAS <- brain$Torig %*% solve(brain$Norig) %*% t(cbind(chanpos[,1:3,drop = FALSE], 1))

# Electrode table
electrode_table <- data.frame(
  Electrode = seq_len(nelec),
  Coord_x = tkrRAS[1, ],
  Coord_y = tkrRAS[2, ],
  Coord_z = tkrRAS[3, ],
  Label = label,
  T1R = chanpos[, 1],
  T1A = chanpos[, 2],
  T1S = chanpos[, 3],
  Radius = 1
)

if(length(save_path) != 1 || is.na(save_path) || save_path == "") {
  subject <- raveio::RAVESubject$new(project_name = rave_project,
                                     subject_code = subject_code,
                                     strict = FALSE)
  save_path <- file.path(subject$meta_path, "electrodes.csv")
}

raveio::dir_create2(dirname(save_path))

raveio::safe_write_csv(
  electrode_table,
  file = save_path,
  row.names = FALSE
)


if( preview ) {
  brain$set_electrodes(electrode_table)
  brain$plot()
}
