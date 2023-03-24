#' @title Visualize template subject with electrodes (already) in MNI space
#' @description This snippet read in MNI (fsaverage) coordinates and generates
#' RAVE 3D viewer
#' 
#' @param path_to_mni_coordinate_BIDS path to BIDS electrode coordinate. The 
#' file must be `tsv` format and headers must contain `name`, `x`, `y`, `z`
#' @param template_brain which template brain to use, default is `N27` 
#' (Collin 27) brain. Alternatively you can specify `fsaverage`
#' @param na_strings How to interpret string as `NA`. In BIDS, it's most likely
#' to be "n/a" (default)
#' @param electrode_value vector for each electrode value, or a function that
#' takes in the electrode file, or just `NULL` if prefer not to specify; 
#' default is `NULL`
#' @param data_name display name for `electrode_value`; default is `"value"`
#' @param data_color color palette for `electrode_value`; default is from
#' `orange`, `dodgerblue3`, `darkgreen`, to `orangered`, `brown`, and to 
#' `purple3`. You can alternate it with any vector of colors
#' @param controllers 3D viewer controller to override default settings; 
#' default is `list()`
#' 
#' @returns A RAVE 3D viewer widget
#' @examples
#' 
#' snippet <- raveio::load_snippet("visualize-mni-in-template-brain", local = FALSE)
#' snippet(
#'   path_to_mni_coordinate_BIDS = "~/Downloads/sub-HUP060_ses-presurgery_acq-seeg_space-fsaverage_electrodes.tsv",
#'   template_brain = "N27",
#'   na_strings = "n/a",
#'   electrode_value = function(coord) {
#'     gsub("[0-9]+$", "", coord$name)
#'   },
#'   data_name = "NamePrefix",
#'   data_color = c("orange", "dodgerblue3", "darkgreen", 
#'                  "orangered", "brown", "purple3"),
#'   controllers = list(
#'     "Right Opacity" = 0.3
#'   )
#' )
#' 
#' 
#' END OF DOC
NULL

# ---- Global variable ---------------------------------------------------------

# Pleas uncomment the following variables

# path_to_mni_coordinate_BIDS <- "~/Downloads/sub-HUP060_ses-presurgery_acq-seeg_space-fsaverage_electrodes.tsv"
# template_brain <- "N27"
# na_strings <- "n/a"
# 
# electrode_value <- function(coord) {
#   gsub("[0-9]+$", "", coord$name)
# }
# 
# data_name <- "Value"
# data_color <- c("orange", "dodgerblue3", "darkgreen", 
#                 "orangered", "brown", "purple3")
# 
# controllers <- list()

# ---- Code part ---------------------------------------------------------------

`%?<-%` <- dipsaus::`%?<-%`

template_brain %?<-% "N27"
na_strings  %?<-% "n/a"
electrode_value %?<-% NULL
data_name %?<-% "Value"
data_color %?<-% c("orange", "dodgerblue3", "darkgreen", 
                "orangered", "brown", "purple3")
controllers %?<-% NULL
controllers <- as.list(controllers)


coords <- read.table(path_to_mni_coordinate_BIDS, na.strings = "n/a", header = TRUE)

coords$Electrode <- seq_len(nrow(coords))
coords$Label <- coords$name

# We use template so this MNI305 is important
coords$MNI305_x <- coords$x
coords$MNI305_y <- coords$y
coords$MNI305_z <- coords$z

# Coord_xyz is not important since we use MNI coordinates on template brain
coords$Coord_x <- ifelse(is.na(coords$x), 0, coords$x)
coords$Coord_y <- ifelse(is.na(coords$y), 0, coords$y)
coords$Coord_z <- ifelse(is.na(coords$z), 0, coords$z)

if(!file.exists(file.path(threeBrain::default_template_directory(), template_brain))) {
  threeBrain::download_template_subject(template_brain)
}

brain <- threeBrain::merge_brain(template_subject = template_brain)
brain$template_object$set_electrodes(coords)


palettes <- list()

if( is.function(electrode_value) ) {
  value <- electrode_value( coords )
  palettes[[data_name]] <- data_color
} else if (length(electrode_value) > 0) {
  value <- electrode_value
  palettes[[data_name]] <- data_color
} else {
  value <- NULL
} 

if(!is.null(value)) {
  coords[[data_name]] <- value
}

brain$template_object$set_electrode_values(coords)

controllers[["Display Data"]] <- data_name
controllers[["Voxel Type"]]  %?<-% "aparc_aseg"
controllers[["Voxel Display"]]  %?<-% "side camera"
controllers[["Voxel Opacity"]]  %?<-% 0.3


brain$plot(
  controllers = controllers,
  palettes = palettes
)
