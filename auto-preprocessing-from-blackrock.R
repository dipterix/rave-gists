

# --------- parameters needs to update every time ----------------------
project_name <- "devel"
subject_code <- 'PAV999'
log_filepath <- "~/rave_data/raw_dir/PAV999/PAV999_testinglog.xlsx"
data_partition <- 1
launch_rave_gui <- TRUE
launch_time_range <- c(-1, 2)

# Where to get electrode channels
electrode_nsp_type <- "ns3"
# Set if not all electrodes are to be imported
electrode_subset <- "1-7"
# where to import electrodes.csv
electrode_coordinate_from_project <- "automated"

# settings for `import_lfp_native`
importLFP_skip_validation <- FALSE
importLFP_force_import <- TRUE
all_blocks_must_exist <- FALSE

# settings for `wavelet_module`
wavelet_predownsample <- 4 
wavelet_frequencies<- seq(from = 2, to = 200, by = 2)


# ------- create folders for each blocks and copy files into it---------
`%?<-%` <- dipsaus::`%?<-%`


# set the dir for data and log
blackrock_dir <- raveio::raveio_getopt("blackrock_dir", default = '~/Dropbox (PENN Neurotrauma)/PennEMU/EMU_Data')

subject_raw_root <- file.path(raveio::raveio_getopt("raw_data_dir"), subject_code)

# set the working dir at the log dir and load log, get the data information

log_table <- readxl::read_excel(log_filepath)

if(!"RAVE Project" %in% names(log_table)) {
  log_table[["RAVE Project"]] <- project_name
}


#' @description Function to import (for each block) the blackrock files into 
#' RAVE, and save configurations
#' @return path prefix in RAVE's raw directory, or `NULL` if the original files
#' are not found
import_blackrock <- function(block, task_name, run = 1, ...) {
  
  # DIPSAUS DEBUG START
  # row <- log_table[1, ]
  # block <- row$`EMU Block`
  # task_name <- row$Task
  # run <- row$`Task Block`
  
  task_name <- strsplit(task_name, "[ ]+")[[1]]
  task_name <- paste(stringr::str_to_title(task_name), collapse = "")
  rave_block_name <- sprintf("sub-%s_ses-%s_task-%s_run-%s", subject_code, block, task_name, sprintf("%02d", as.integer(run)))
  
  file_pattern <- sprintf("^%s_Datafile_%s\\.nev$", subject_code, block)
  subject_source_root <- file.path(blackrock_dir, subject_code)
  nev_source_path <- list.files(subject_source_root, pattern = file_pattern, recursive = TRUE, full.names = TRUE)
  
  if(!length(nev_source_path)) {
    # TODO: nev file not exists, write to a log file
    
    return()
  }
  
  rave_block_prefix <- file.path(subject_raw_root, rave_block_name)
  
  # If the data has been converted from BR to HDF5 before, do not re-convert
  if(dir.exists(paste0(rave_block_prefix, "_ieeg_part1"))) {
    return(rave_block_prefix)
  }
  
  nev_source_path <- nev_source_path[[1]]
  nsx_source_path <- gsub("\\.nev$", "", nev_source_path, ignore.case = TRUE)
  nsx_source_path <- paste0(nsx_source_path, sprintf(".ns%d", 1:9))
  nsx_source_path <- nsx_source_path[file.exists(nsx_source_path)]
  
  readNSx::import_nsp(
    path = nev_source_path, 
    prefix = rave_block_prefix, 
    exclude_events = "spike", 
    partition_prefix = "_part"
  )
  # copy blackrock files to the RAVE folder as backups
  backup_folder <- file.path(paste0(rave_block_prefix, "_ieeg"), "raw")
  raveio::dir_create2(backup_folder)
  for(source_filepath in c(nev_source_path, nsx_source_path)) {
    file.copy(source_filepath, file.path(
      backup_folder, basename(source_filepath)), 
      overwrite = TRUE, recursive = FALSE,
      copy.mode = TRUE, copy.date = TRUE)
  }
  
  
  return( rave_block_prefix )
  
}

# Call import_blackrock on all blocks
n_blocks <- nrow(log_table)
path_prefix <- lapply(seq_len(n_blocks), function(ii) {
  
  block_info <- log_table[ii, ]
  prefix <- import_blackrock(
    block = block_info[["EMU Block"]],
    task_name = block_info[["Task"]],
    run = block_info[["Task Block"]]
  )
  return(prefix)
  
})

  
# find blocks and file prefix
selection <- log_table[["RAVE Project"]] %in% project_name
project_block_prefix <- path_prefix[selection]

# Check if all blocks exist
block_is_missing <- vapply(project_block_prefix, is.null, FALSE)
if(any(block_is_missing) && all_blocks_must_exist) {
  warning("There are ", sum(block_is_missing), " blocks missing from project ", project_name)
  return()
}

# initialize the subject folder
subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
subject$initialize_paths()

# get block folder
get_block_from_prefix <- function(prefix) {
  sprintf("%s_ieeg_part%s", basename(prefix), data_partition)
}
rave_blocks <- unlist(lapply(unlist(project_block_prefix), get_block_from_prefix))

# get channel numbers
sample_block_prefix <- project_block_prefix[[1]]
nsx <- readNSx::get_nsx(sample_block_prefix, electrode_nsp_type)
electrodes_to_import <- nsx$header_extended$CC$electrode_id
if(length(electrode_subset)) {
  electrodes_to_import <- electrodes_to_import[
    electrodes_to_import %in% dipsaus::parse_svec(electrode_subset)
  ]
}

if(length(electrodes_to_import)) {
  message("Channels to import: ", dipsaus::deparse_svec(electrodes_to_import))
} else {
  stop("No channel to import into RAVE. Please check if `electrode_subset` is accidentally set to a wrong list")
}

# ---- Import LFP signals into RAVE --------------------
# Load RAVE 2.0 module import_lfp_native
pipeline_path <- file.path(subject$pipeline_path, "import_lfp_native")
if(dir.exists(pipeline_path)) {
  unlink(pipeline_path, recursive = TRUE)
}
pipeline_importLFP_orig <- raveio::pipeline(pipeline_name = "import_lfp_native")
pipeline_path_importLFP <- raveio::pipeline_fork(
  src = pipeline_importLFP_orig$pipeline_path,
  dest = file.path(subject$pipeline_path, "import_lfp_native")
)
pipeline_importLFP <- raveio::pipeline(pipeline_name = "import_lfp_native", 
                                       paths = subject$pipeline_path)
# pipeline_importLFP settings.yaml:
# force_import: yes
# import_blocks__format: .mat/.h5 file per electrode per block
# import_blocks__session_block:
#   - 008
# import_channels__electrode_file: auto
# import_channels__electrodes: 13-16,24
# import_channels__sample_rate: 2000
# import_channels__unit: NA
# import_setup__project_name: demo
# import_setup__subject_code: DemoSubject
# skip_validation: yes

pipeline_importLFP$set_settings(
  force_import = importLFP_force_import,
  import_channels__electrode_file = "auto",
  import_channels__unit = "NA",
  skip_validation = importLFP_skip_validation,
  
  import_blocks__format = ".mat/.h5 file per electrode per block",
  import_setup__project_name = project_name,
  import_setup__subject_code = subject_code,
  import_blocks__session_block = rave_blocks,
  import_channels__electrodes = dipsaus::deparse_svec(electrodes_to_import),
  import_channels__sample_rate = nsx$header_basic$time_resolution_timestamp/nsx$header_basic$period
)

message("Importing LFP signals using RAVE preprcessing pipeline `import_lfp_native`")
pipeline_importLFP$run(
  async = FALSE,
  as_promise = FALSE,
  scheduler = "none",
  type = "smart"
)

# ---- Import epoch data into RAVE ----------------
block_nev_prefix <- unlist(project_block_prefix)

epoch_table <- lapply(block_nev_prefix, function(prefix) {
  epoch_table <- readNSx::get_event(prefix, "comment")
  if(!is.data.frame(epoch_table)) { return(NULL) }
  data.frame(
    Block = get_block_from_prefix(prefix),
    Trial = seq_len(nrow(epoch_table)),
    Time = epoch_table$time_in_seconds,
    Condition = epoch_table$comment
  )
})
epoch_table <- do.call(rbind, epoch_table)
if(is.data.frame(epoch_table)) {
  raveio::safe_write_csv(
    x = epoch_table,
    file = file.path(subject$meta_path, sprintf("epoch_nev_exports.csv"))
  )
}


# ---- Import electrodes.csv data into RAVE ----------------
# if electrode_coordinate_from_project is not NA, then import
# electrodes.csv from this project

subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code)
if(length(electrode_coordinate_from_project) == 1 &&
   !is.na(electrode_coordinate_from_project)) {
  
  # use try-catch in case the electrodes.csv is bad
  tryCatch({
    subject2 <- raveio::RAVESubject$new(
      project_name = electrode_coordinate_from_project,
      subject_code = subject_code, strict = FALSE
    )
    electrode_table2 <- subject2$get_electrode_table()
    electrode_table1 <- subject$get_electrode_table()
    
    electrode_table <- merge(
      electrode_table1,
      electrode_table2,
      by = "Electrode",
      all.x = TRUE,
      all.y = FALSE,
      sort = TRUE,
      suffixes = c(".x", ""),
      no.dups = TRUE
    )
    electrode_table$SignalType <- electrode_table$SignalType.x
    electrode_table$LocationType[is.na(electrode_table$LocationType)] <- "iEEG"
    electrode_table$Label[is.na(electrode_table$Label)] <- "NoLabel"
    nms <- unique(c(names(electrode_table1), names(electrode_table2)))
    electrode_table <- electrode_table[, nms]
    # fill in the NAs
    for(nm in nms) {
      col_data <- electrode_table[[nm]]
      if(is.numeric(col_data)) {
        col_data[is.na(col_data)] <- 0
        electrode_table[[nm]] <- col_data
      } else if(is.character(col_data)) {
        if(startsWith(nm, "FSLabel")) {
          col_data[is.na(col_data)] <- "Unknown"
        } else {
          col_data[is.na(col_data)] <- ""
        }
        electrode_table[[nm]] <- col_data
      }
    }
    
    raveio::save_meta2(data = electrode_table, meta_type = "electrodes", project_name = subject$project_name, subject_code = subject$subject_code)
  }, error = function(e) {
    e$message <- sprintf("Cannot import electrodes.csv, additional message: \n%s\n\nHowever, this is just a warning, you should be able to proceed ignoring this warning", e$message)
    warning(e)
  })
  
}

# ---- Notch filter ----------------------------------------
# To list all pipelines available
# raveio::pipeline_list()
# Load RAVE 2.0 module notch_filter
pipeline_path <- file.path(subject$pipeline_path, "notch_filter")
if(dir.exists(pipeline_path)) {
  unlink(pipeline_path, recursive = TRUE)
}
pipeline_notchFilter_orig <- raveio::pipeline(pipeline_name = "notch_filter")
pipeline_path_notchFilter <- raveio::pipeline_fork(
  src = pipeline_notchFilter_orig$pipeline_path,
  dest = file.path(subject$pipeline_path, "notch_filter")
)
pipeline_notchFilter <- raveio::pipeline(pipeline_name = "notch_filter", 
                                       paths = subject$pipeline_path)
# pipeline_notchFilter settings.yaml:
# dput(pipeline_notchFilter$get_settings())

pipeline_notchFilter$set_settings(
  project_name = subject$project_name,
  subject_code = subject$subject_code,
  notch_filter_upperbound = c(61, 122, 182),
  notch_filter_lowerbound = c(59, 118, 178),
  diagnostic_plot_params = list(
    path = file.path(
      pipeline_notchFilter$pipeline_path,
      "diagnostic_plot.pdf"
    ),
    window_length = 4000L,
    max_frequency = 300L,
    histogram_bins = 60L,
    background = "#ffffff",
    foreground = "#212529",
    font_size = 2,
    quiet = FALSE
  )
)

message("Applying notch filters via pipeline `notch_filter`")
pipeline_notchFilter$run(
  names = "apply_notch", async = FALSE, as_promise = FALSE,
  scheduler = "none", type = "smart"
)

pipeline_notchFilter$run(names = "diagnostic_plots")

# ---- Wavelet -----------------------------------------------------------
# To list all pipelines available
# raveio::pipeline_list()
# Load RAVE 2.0 module notch_filter
pipeline_path <- file.path(subject$pipeline_path, "wavelet_module")
if(dir.exists(pipeline_path)) {
  unlink(pipeline_path, recursive = TRUE)
}
pipeline_wavelet_orig <- raveio::pipeline(pipeline_name = "wavelet_module")
pipeline_path_wavelet <- raveio::pipeline_fork(
  src = pipeline_wavelet_orig$pipeline_path,
  dest = file.path(subject$pipeline_path, "wavelet_module")
)
pipeline_wavelet <- raveio::pipeline(pipeline_name = "wavelet_module", 
                                         paths = subject$pipeline_path)
# pipeline_wavelet settings.yaml:
# dput(pipeline_wavelet$get_settings())

wavelet_cycles <- ravetools::wavelet_cycles_suggest(wavelet_frequencies)
pipeline_wavelet$set_settings(
  project_name = subject$project_name,
  subject_code = subject$subject_code,
  kernel_table = list(
    Frequency = wavelet_cycles$Frequency,
    Cycles = wavelet_cycles$Cycles
  ),
  pre_downsample = wavelet_predownsample,
  precision = "double",
  target_sample_rate = 100
)

message("Applying wavelet via pipeline `wavelet_module`")
# to find which target to evaluate
# pipeline_wavelet$target_table
pipeline_wavelet$run(
  async = FALSE, as_promise = FALSE,
  scheduler = "none", type = "smart"
)

# ---- Roughly generate and cache bipolar reference channels ----------------
electrode_table <- subject$get_electrode_table()
electrode_table$LabelPrefix %?<-% "Default"

# bipolar by LabelPrefix
reference_table <- lapply(split(electrode_table, electrode_table$LabelPrefix), function(sub) {
  reference <- c(sprintf("ref_%d", sub$Electrode[-1]), "noref")
  data.frame(
    Electrode = sub$Electrode,
    Group = sub$LabelPrefix,
    Reference = reference,
    Type = "Bipolar Reference"
  )
})

reference_table <- do.call("rbind", reference_table)
rownames(reference_table) <- NULL

# save as reference table
raveio::safe_write_csv(
  reference_table,
  file.path(subject$meta_path, "reference_bipolar_initial.csv")
)

# ---- Convert to RAVE 1.0 compatible format --------------------
raveio::rave_subject_format_conversion(subject$subject_id)

# ---- Cache bipolar reference ------------------------------------
# reload subject to get all information up to date
subject <- raveio::as_rave_subject(subject$subject_id)
reference_table <- subject$get_reference("bipolar_initial")
blocks <- subject$blocks

# For each electrode, load referenced data (in parallel)
raveio::with_future_parallel({
  dipsaus::lapply_async2(seq_len(nrow(reference_table)), function(ii) {
    e <- reference_table$Electrode[[ii]]
    ref <- reference_table$Reference[[ii]]
    if(!startsWith(ref, "ref")) {
      # no need to cache as it's noref or bad channel
      return()
    }
    electrode_instance <- raveio::new_electrode(subject = subject, number = e)
    electrode_instance$set_reference(reference = ref)
    
    # get referenced wavelet coefficients
    wavelet_coef <- electrode_instance$load_blocks(
      blocks = blocks, type = "wavelet-coefficient", simplify = FALSE)
    voltage <- electrode_instance$load_blocks(
      blocks = blocks, type = "voltage", simplify = FALSE)
    
    # save cached wavelets
    for(block in blocks) {
      coef <- t(wavelet_coef[[block]])
      volt <- voltage[[block]]
      # power
      raveio::save_h5(x = Mod(coef)^2, file = electrode_instance$power_file, 
                      name = sprintf("ref/power/%s", block))
      raveio::save_h5(x = ref, file = electrode_instance$power_file, name = "reference")
      # phase
      raveio::save_h5(x = Arg(coef), file = electrode_instance$phase_file, 
                      name = sprintf("ref/phase/%s", block))
      raveio::save_h5(x = ref, file = electrode_instance$phase_file, name = "reference")
      # voltage
      raveio::save_h5(x = volt, file = electrode_instance$voltage_file, 
                      name = sprintf("ref/voltage/%s", block))
      raveio::save_h5(x = ref, file = electrode_instance$voltage_file, name = "reference")
    }
  },
  plan = FALSE, callback = function(ii) {
    e <- reference_table$Electrode[[ii]]
    ref <- reference_table$Reference[[ii]]
    sprintf("Cache reference|Electrode %s (%s)", e, ref)
  })
})

# save cached reference table
cached_table <- reference_table[, c("Electrode", "Reference")]
cached_table$Reference[!startsWith(cached_table$Reference, "ref")] <- "invalid"
raveio::safe_write_csv(cached_table, file.path(subject$cache_path, 'cached_reference.csv'))

message("Done RAVE preprocessing!")

# ---- Launch RAVE session ------------------------------------

if( launch_rave_gui ) {
  rave::rave_prepare(
    subject = subject$subject_id,
    electrodes = subject$electrodes,
    time_range = abs(launch_time_range),
    epoch = "nev_exports",
    reference = "bipolar_initial",
    data_types = "power"
  )
  rave::start_rave(active_module = "power_explorer")
}
