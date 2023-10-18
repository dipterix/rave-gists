#' @author Zhengjia Wang
#' @date Oct 18, 2023
#' @license Apache-2.0
#' 
#' @title Custom RAVE/YAEL brain visualization
#' @description
#' Load one or multiple RAVE brain and visualize data
#' 
#' @param config A list of subject brain & value configuration, see 'Examples'
#' @param use_template whether to use template when there is only one available
#' brain instance; default is `FALSE`
#' @param template_brain which template to use; default is `cvs_avg35_inMNI152`,
#' other choices are `bert`, `cvs_avg35`, `fsaverage`, `fsaverage_sym`, `N27`
#' @param additional_atlases Additional atlas files to load (see FreeSurfer/mri)
#' folder, examples are `aparc.a2009s+aseg`, `aparc+aseg`, ...
#' @param palettes A list of key-values, where keys are electrode variable names
#' and values are vectors of colors to show
#' @param value_ranges A list of key-values, where keys are electrode variable 
#' names and values are range of the value
#' @param controllers Default controllers (see the right control panel from 
#' viewers)
#' 
#' @examples
#' 
#' 
#' template_visualizer <- raveio::load_snippet("visualize-multi-subject-template")
#' 
#' # print docs
#' print(template_visualizer)
#' 
#' template_visualizer(
#'   config = list(
#'     
#'     # project/subject as keys, or simply subject_code
#'     "demo/DemoSubject" = list(
#'       
#'       # In case your freesurfer folder is not at RAVE folder
#'       fspath = NULL,
#'       
#'       # you can use either file path 
#'       values = "~/rave_data/data_dir/demo/DemoSubject/rave/meta/electrodes.csv"
#'     ),
#'     
#'     # or data.frame with column `Electrode` and `Time` (optional)
#'     "YAEL/yael_demo_001" = list(
#'       
#'       values = data.frame(
#'         Electrode = rep(25:36, each = 10),
#'         Time = rnorm(120),
#'         pVal = runif(120)
#'       )
#'       
#'     )
#'   ),
#'   
#'   # Optional
#'   use_template = FALSE,
#'   template_brain = "cvs_avg35_inMNI152",
#'   additional_atlases = NULL,
#'   
#'   # color, range, display
#'   palettes = list(
#'     pVal = colorRampPalette(c("red", "orange", "steelblue"), bias = 3)(101)
#'   ),
#'   value_ranges = list(
#'     pVal = c(0, 1)
#'   ),
#'   controllers = list(
#'     "Display Data" = "pVal"
#'   )
#' )
#' 
#' 
#' END OF DOC
NULL



# ---- variables ---------------------------------------------------------------

# length of 1 or more
config <- list(
  
  # project/subject as keys, or simply subject_code
  "demo/DemoSubject" = list(
    
    # In case your freesurfer folder is not at RAVE folder
    fspath = NULL,
    
    # you can use either file path 
    values = "~/rave_data/data_dir/demo/DemoSubject/rave/meta/electrodes.csv"
  ),
  
  # or data.frame with column `Electrode` and `Time` (optional)
  "YAEL/yael_demo_001" = list(
    
    values = data.frame(
      Electrode = rep(25:36, each = 10),
      Time = rnorm(120),
      pVal = runif(120)
    )
    
  )
  
)

# TRUE for always use template, even if there is one subject
use_template <- FALSE

# "bert", "cvs_avg35", "cvs_avg35_inMNI152", "fsaverage", "fsaverage_sym", "N27
template_brain <- "cvs_avg35_inMNI152"

additional_atlases <- "aparc.a2009s+aseg"

# color
palettes <- list(
  pVal = colorRampPalette(c("red", "orange", "steelblue"), bias = 3)(101)
)
value_ranges <- list(
  pVal = c(0, 1)
)
controllers <- list(
  "Display Data" = "pVal"
)

# ---- code body ---------------------------------------------------------------

# ---- Default arguments
`%?<-%` <- dipsaus::`%?<-%`
template_brain %?<-% "cvs_avg35_inMNI152"
additional_atlases %?<-% NULL
use_template %?<-% FALSE
palettes %?<-% list()
value_ranges %?<-% list()
controllers %?<-% list()

# ---- Make sure template exists, or download on the fly
# Maybe I should put this in the core package
if(!file.exists(file.path(threeBrain::default_template_directory(), template_brain))) {
  if(!template_brain %in% c("bert", "cvs_avg35", "cvs_avg35_inMNI152", "fsaverage", "fsaverage_sym", "N27")) {
    template_brain <- "cvs_avg35_inMNI152"
  }
  if(!file.exists(file.path(threeBrain::default_template_directory(), template_brain))) {
    threeBrain::download_template_subject(subject_code = template_brain)
  }
  options("threeBrain.template_subject" = template_brain)
}

brain_list <- lapply(names(config), function(subject) {
  item <- config[[subject]]
  
  if(!grepl("/", subject)) {
    subject <- sprintf("YAEL/%s", subject)
  }
  # convert to RAVE subject instance
  subject <- raveio::as_rave_subject(subject, strict = FALSE)
  
  if( length(subject$freesurfer_path) == 1 && !is.na(subject$freesurfer_path) ) {
    brain <- raveio::rave_brain(subject)
  } else {
    brain <- threeBrain::threeBrain(path = item$fspath, subject_code = subject$subject_code)
    brain$set_electrodes(subject$meta_data("electrodes"))
  }
  if(is.null(brain)) { return(NULL) }
  
  brain$set_electrode_values(item$values)
  brain
})

# Remove invalid brains
brain_list <- dipsaus::drop_nulls(brain_list)
if(!isTRUE(use_template) && length(brain_list) == 1) {
  brain <- brain_list[[1]]
} else {
  brain <- threeBrain::merge_brain(.list = brain_list, template_surface_types = template_brain)
}

brain$plot(
  atlases = c("aparc+aseg", additional_atlases),
  palettes = palettes,
  val_ranges = value_ranges,
  controllers = controllers
)
