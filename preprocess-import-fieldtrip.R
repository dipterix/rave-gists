#' @author Zhengjia Wang
#' @date July 09, 2023
#' @license Apache-2.0
#' 
#' @title Imports FieldTrip to RAVE (trial as blocks)
#' @description
#' Imports and convert FieldTrip to RAVE, each "trial" (in FieldTrip)
#' is treated as session/block/run in RAVE (RAVE does not process raw
#' data at trial level)
#' 
#' @param file_path File path to the FieldTrip data (Matlab, tested with 
#' Matlab >= v7.3)
#' @param project_name RAVE project name to use (you can name it with your task)
#' @param subject_code Subject code to use
#' @param data_name the data name in FieldTrip data (depending on how you save
#' data, default is `NULL`, which uses the largest dataset in Matlab file)
#' @param block_prefix RAVE block prefix (you can name it with your 
#' `{session}_{task}`)
#' @param do_import whether to import the data into RAVE with given project
#' and subject; default is true
#' @param launch_rave whether to launch RAVE at the end; default is true if
#' running in an interactive R session
#' 
#' @examples
#' 
#' # load the script
#' import_ft <- raveio::load_snippet("preprocess-import-fieldtrip", local = FALSE)
#' 
#' # print documentation
#' print(import_ft)
#' 
#' # run script
#' 
#' 
#'
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

# DIPSAUS DEBUG START
# 
# project_name <- "lombard"
# subject_code <- "DM1006"
# file_path <- "~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/Latane/sub-DM1006_ses-intraop_task-lombard_ft-raw.mat"
# data_name <- NULL
# block_prefix <- "intraop_lombard"
# 
# launch_rave <- TRUE

# ---- code body ---------------------------------------------------------------
`%?<-%` <- dipsaus::`%?<-%`

data_name %?<-% NULL
do_import %?<-% TRUE

# If you see `ERROR:root:ERROR: MATLAB type not supported: table, (uint32)`, it's fine
message("Don't panic if you see message below: `ERROR:root:ERROR: MATLAB type not supported: table, (uint32)`. You are fine!")
raw_data <- raveio::read_mat(file_path, engine = "py")

if(!length(data_name)) {
  nms <- names(raw_data)
  size <- sapply(nms, function(nm) {
    object.size(raw_data[[nm]])
  })
  data_name <- nms[which.max(size)][[1]]
}
data_list <- raw_data[[data_name]]

# Configuration

# Number of channels
channel_counts <- data_list$hdr$nChans

# Number of runs (RAVE blocks), stored as trials
block_counts <- data_list$hdr$nTrials

# Sampling frequency
fsample <- data_list$fsample

# collect channel information
channel_table <- data.frame(Electrode = seq_len(channel_counts))
tryCatch({
  col <- unlist(data_list$label)
  if(length(col) == channel_counts) {
    channel_table$Label <- col
  }
  
  col <- unlist(data_list$hdr$chantype)
  if(length(col) == channel_counts) {
    channel_table$Type <- col
  }
  
  col <- unlist(data_list$hdr$chanunit)
  if(length(col) == channel_counts) {
    channel_table$Unit <- col
  }
}, error = function(...) {})


# Convert to RAVE raw data format
progress <- dipsaus::progress2("Converting to RAVE",
                               shiny_auto_close = TRUE,
                               max = channel_counts * block_counts)

save_block <- function(block_num) {
  # DIPSAUS DEBUG START
  # # quick debug using shortcut to set variables
  # block_num <- 1
  # channel_num <- 1
  # chn <- 1
  
  # get block name and create path
  block_name <- sprintf("%s_%03d", block_prefix, block_num)
  block_path <- file.path(raveio::raveio_getopt("raw_data_dir"), subject_code, block_name)
  
  # create 
  block_path <- raveio::dir_create2(block_path)
  
  # get block data
  block_data <- data_list$trial[[ block_num ]]
  if(!is.matrix(block_data)) {
    block_data <- array(block_data, dim = c(channel_counts, length(block_data) / channel_counts))
  } else {
    if(nrow(block_data) < channel_counts) {
      stop(sprintf("Number of channel [%s] in data is smaller than the expected [%s] in block [%s]", 
                   nrow(block_data), channel_counts, block_name))
    }
  }
  
  lapply(seq_len(channel_counts), function(chn) {
    
    progress$inc(sprintf("%s - %s", block_name, chn))
    channel_data <- block_data[chn, ]
    channel_data[!is.finite(channel_data)] <- 0
    channel_file <- file.path(block_path, sprintf("chan_%s.h5", chn))
    
    # save data
    raveio::save_h5(
      x = channel_data,
      file = channel_file,
      name = "data",
      level = 7,
      replace = TRUE,
      new_file = FALSE,
      ctype = "numeric",
      quiet = TRUE
    )
    
    # save channel information
    info <- as.list(channel_table[chn, ])
    raveio::save_h5(
      x = jsonlite::toJSON(info, auto_unbox = TRUE),
      file = channel_file,
      name = "info",
      level = 7,
      replace = TRUE,
      ctype = "character",
      quiet = TRUE
    )
    
  })
  
  
  # save time-point information
  time <- data_list$time[[ block_num ]]
  if(length(time) && is.numeric(time)) {
    raveio::save_h5(
      x = time,
      file = file.path(block_path, "time.h5"),
      name = "data",
      level = 7,
      replace = TRUE,
      new_file = FALSE,
      ctype = "numeric",
      quiet = TRUE
    )
  }
  
  return()
}
lapply(seq_len(block_counts), save_block)

# We don't need large data anymore
rm(raw_data, data_list); gc()
# --- Conversion is done, imporing into RAVE

if( do_import ) {
  
  # load into RAVE using RAVE pipeline
  pipeline <- raveio::pipeline("import_lfp_native")
  
  # set pipeline 
  pipeline$set_settings(
    skip_validation = FALSE,
    import_setup__subject_code = subject_code,
    import_setup__project_name = project_name,
    import_channels__unit = "NA",
    import_channels__sample_rate = fsample,
    import_channels__electrodes = seq_len(channel_counts),
    import_channels__electrode_file = "auto",
    import_blocks__session_block = sprintf("%s_%03d", block_prefix, seq_len(block_counts)),
    import_blocks__format = ".mat/.h5 file per electrode per block",
    force_import = TRUE
  )
  pipeline$run(scheduler = "none", type = "smart")
  
  if(ncol(channel_table) > 1) {
    subject <- pipeline$read("subject")
    electrodes <- subject$get_electrode_table()
    for(nm in names(channel_table)[-1]) {
      electrodes[[nm]] <- channel_table[[nm]]
    }
    raveio::save_meta2(
      data = electrodes,
      meta_type = "electrodes",
      project_name = subject$project_name,
      subject_code = subject$subject_code
    )
  }
}

if(interactive() && launch_rave) {
  rave::start_rave2()
}
