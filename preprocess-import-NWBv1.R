# Experimental script to convert NWB to RAVE

project_name <- "devel"
subject_code <- "MW13"
block <- "sess_5"

nwb_path <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/NWB_Data/MW13_Session_5_filter.nwb"

# Event timestamp from time points to time in seconds
event_time_conversion <- 1e-6

# The signal_path points to
# data.processing.ecephys.data_interfaces.LFP.electrical_series.MacroWireSeries
signal_path <- c("processing", "ecephys", "data_interfaces", "LFP", "electrical_series", "MacroWireSeries")

# Parameters for Notch filter
# c() is length of 0: skip notch filters
notch_filter_frequencies <- c()
notch_filter_bandwidths <- c()
pwelch_plot <- FALSE

# Parameters for Morlet wavelet
wavelet_frequencies <- seq(2, 200, 10)
wavelet_cycles <- ravetools::wavelet_cycles_suggest(
  freqs = wavelet_frequencies,
  frequency_range = c(2, 200),
  cycle_range = c(3, 20)
)$Cycles
# Do not down-sample before wavelet to prevent artifact
wavelet_pre_downsample <- 1
# down-sample after wavelet since power is usually smooth
wavelet_coef_sample_rate <- 100
wavelet_enabled <- TRUE


# Other parameters
# regular expression to get sample rate from deescription
sample_rate_regexp <- "fs[ ]{0,}=[ ]{0,}([0-9]+)[^0-9]"

# ------ Collect information ------------------------------

if(!dipsaus::package_installed("rnwb")) {
  ravemanager:::install_packages("rnwb")
}


container <- rnwb::NWBHDF5IO$new(path = nwb_path, mode = "r")

# DIPSAUS DEBUG START
# handler <- container$.__enclos_env__$private$ensure_file_handler()
# data <- handler$read()

container$with({
  
  # You might have warning when NWB file fail the 2.0 check because the 
  # timestamp does not match with data length
  data <- container$read()
  
  # get signal data
  lfp_data <- data
  for(nm in signal_path) {
    lfp_data <- lfp_data[[nm]]
  }
  
  # get sample rate from the description
  description <- rnwb::to_r(lfp_data$description)
  sample_rate <- stringr::str_match(tolower(description), sample_rate_regexp)[[2]]
  sample_rate <- as.numeric(sample_rate)
  if (is.na(sample_rate)) {
    stop("Cannot derive sample rate from the description below:\n  ", description)
  }
  
  # get int to float conversion
  conversion <- as.numeric(rnwb::to_r(lfp_data$conversion))
  if(!length(conversion) || is.na(conversion)) {
    # no conversion from int to float
    conversion <- 1.0
  } else {
    # V -> uV
    conversion <- conversion * 1e6
  }
  
  # get data dimension
  shape <- unlist(rnwb::to_r(lfp_data$data$shape))
  n_channels <- shape[[2]]
  n_timepoints <- shape[[1]]
  
  # get electrode table
  # This process could be error prone since I don't know about the 
  # content
  rnwb::run_pystring("r.electrode_table = r.lfp_data.electrodes[:]", convert = TRUE)
  chann_id <- electrode_table$channID
  
  group_names <- names(data$electrode_groups)
  group_descriptions <- sapply(group_names, function(gname) {
    rnwb::to_r(data$electrode_groups[[gname]]$description)
  })
  group_table <- data.frame(
    group_names = group_names,
    group_descriptions = group_descriptions
  )
  
  electrode_table$group_descriptions <- vapply(electrode_table$group, function(g) {
    rnwb::to_r(g$description)
  }, "")
  electrode_table <- merge(electrode_table, group_table, by = "group_descriptions", all.x = TRUE)
  electrode_table$group <- NULL  
  
  electrode_table <- merge(electrode_table, data.frame(
    channID = chann_id
  ))
  
  
  lfp_channels <- as.integer(electrode_table$channID)
  if(length(lfp_channels) != n_channels) {
    stop(sprintf("Number of channels are inconsistent in NWB file. Expected [%s] vs. actual [%s]", length(lfp_channels), n_channels))
  }
  
  block_starttime <- lfp_data$timestamps[1, convert = TRUE]
  block_endtime <- lfp_data$timestamps[length(lfp_data$timestamps), convert = TRUE] - block_starttime
  block_endtime <- block_endtime * event_time_conversion
  
  has_events <- FALSE
  event_timestamps <- c()
  event_triggers <- c()
  try({
    events <- data$acquisition$events
    event_timestamps <- events$timestamps[convert = TRUE] - block_starttime
    event_timestamps <- event_timestamps * event_time_conversion
    event_triggers <- events$data[convert = TRUE]
    
    sel <- event_timestamps > 0 & event_timestamps < block_endtime
    event_timestamps <- event_timestamps[sel]
    event_triggers <- event_triggers[sel]
    has_events <- any(sel)
  })
  
})

# ------ Convert channel ------------------------------
# Now let's start RAVE and initialize subjects
subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
preprocessing <- subject$preprocess_settings

# Make sure RAVE subject folder is complete
subject$initialize_paths(include_freesurfer = FALSE)
block_folder <- raveio::dir_create2(file.path(preprocessing$raw_path, block))

# write down epoch information
if( has_events ) {
  raveio::safe_write_csv(
    data.frame(
      Block = block,
      Time = event_timestamps,
      Trial = seq_along(event_triggers),
      Condition = event_triggers
    ),
    row.names = FALSE,
    file.path(subject$meta_path, sprintf("epoch_editme_%s.csv", block))
  )
}

# parallel convert
files <- raveio::lapply_async(seq_along(lfp_channels), function(ii) {
  
  channel <- lfp_channels[[ii]]
  
  container <- rnwb::NWBHDF5IO$new(path = nwb_path, mode = "r")
  container$with({
    suppressWarnings({
      data <- container$read()
    })
    # get signal data
    lfp_data <- data
    for(nm in signal_path) {
      lfp_data <- lfp_data[[nm]]
    }
    fpath <- file.path(block_folder, sprintf("channel_%04d.h5", channel))
    signal <- lfp_data$data[, ii, convert = TRUE] * conversion
    raveio::save_h5(x = signal, file = fpath, name = "data", ctype = "numeric", replace = TRUE, quiet = TRUE)
    meta <- jsonlite::toJSON(auto_unbox = TRUE, list(
      channel = channel,
      sample_rate = sample_rate,
      n_timepoints = n_timepoints
    ))
    raveio::save_h5(x = meta, file = fpath, name = "meta", ctype = "character", replace = TRUE, quiet = TRUE)
  })
  
}, callback = function(channel) {
  sprintf("Converting channel %s", channel)
})


# ------ Standard preprocess pipeline ------------------------------------

working_path <- file.path(subject$pipeline_path, "MWB_import")
pipeline_collection <- raveio::pipeline_collection(root_path = working_path, overwrite = TRUE)

# Step 1: Add pipeline `import_lfp_native` to collection with settings
job_import_signal <- pipeline_collection$add_pipeline(
  x = "import_lfp_native", 
  standalone = FALSE,
  pre_hook = function(settings, shared_path) {
    
    settings$import_setup__subject_code <- subject_code
    settings$import_setup__project_name <- project_name
    settings$import_channels__sample_rate <- sample_rate
    settings$import_channels__electrodes <- dipsaus::deparse_svec(lfp_channels)
    settings$import_blocks__session_block <- block
    settings$import_blocks__format <- ".mat/.h5 file per electrode per block"
    
    # fixed usage
    settings$skip_validation <- FALSE
    settings$import_channels__unit <- "NA"
    settings$force_import <- TRUE
    
  }
)

# Step 2: Notch filter 
job_notch_filter <- pipeline_collection$add_pipeline(
  x = "notch_filter", 
  deps = job_import_signal$id, 
  standalone = FALSE,
  pre_hook = function(settings, shared_path) {
    
    settings$subject_code <- subject_code
    settings$project_name <- project_name
    settings$notch_filter_lowerbound <- notch_filter_frequencies - notch_filter_bandwidths / 2
    settings$notch_filter_upperbound <- notch_filter_frequencies + notch_filter_bandwidths / 2
    settings$diagnostic_plot_params <- list(
      path = file.path(shared_path, "notch-pwelch-plot.pdf"),
      window_length = ceiling(2 * sample_rate),
      max_frequency = floor(max(300, sample_rate / 2)),
      histogram_bins = 60L,
      background = "#ffffff",
      foreground = "#212529",
      font_size = 2,
      quiet = FALSE,
      dry_run = !isTRUE(as.logical(pwelch_plot))
    )
    
  }
)

if( wavelet_enabled ) {
  job_wavelet <- pipeline_collection$add_pipeline(
    x = "wavelet_module", 
    deps = c(
      job_import_signal$id,
      job_notch_filter$id
    ), 
    standalone = FALSE,
    pre_hook = function(settings, shared_path) {
      
      settings$subject_code <- subject_code
      settings$project_name <- project_name
      
      settings$kernel_table <- list(
        Frequency = wavelet_frequencies,
        Cycles = wavelet_cycles)
      
      settings$pre_downsample = wavelet_pre_downsample
      settings$precision = "float"
      settings$target_sample_rate <- wavelet_coef_sample_rate
      
    }
  )
  
}


# pipeline_collection$build_pipelines()


# Run the pipelines
pipeline_collection$run()

# write electrode information
electrode_labels <- do.call(
  "rbind",
  lapply(
    split(electrode_table$shortBAn, electrode_table$shortBAn), 
    function(pf) {
      data.frame(
        Label = sprintf("%s%d", pf, seq_along(pf)),
        Dimension = length(pf)
      )
    }
  )
)
rave_electrode_table <- data.frame(
  Electrode = electrode_table$channID,
  Coord_x = 0,
  Coord_y = 0,
  Coord_z = 0,
  LabelPrefix = electrode_table$shortBAn,
  Hemisphere = ifelse(tolower(electrode_table$hemisph == "l"), "left", "right"),
  LocationType = "iEEG",
  SignalType = "LFP",
  GroupDescription = electrode_table$group_descriptions,
  wireID = electrode_table$wireID,
  WireLabel = electrode_table$label,
  T1R = electrode_table$xcoord, 
  T1A = electrode_table$ycoord, 
  T1S = electrode_table$zcoord
)

rave_electrode_table <- data.table::rbindlist(
  lapply(split(rave_electrode_table, rave_electrode_table$LabelPrefix), function(sub) {
    sub$Label <- sprintf("%s%d", sub$LabelPrefix, seq_len(nrow(sub)))
    sub$Dimension <- nrow(sub)
    sub
  })
)
# re-order by electrode
rave_electrode_table <- rave_electrode_table[order(rave_electrode_table$Electrode), ]


raveio::save_meta2(
  data = rave_electrode_table,
  meta_type = "electrodes",
  project_name = subject$project_name,
  subject_code = subject$subject_code
)

# Fixed usage
subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = TRUE)
# utils::write.csv(tp_tbl, file.path(subject$meta_path, "time_points.csv"))
cache_path <- file.path(subject$data_path, "cache")
dir_create2(cache_path)
utils::write.csv(
  data.frame(Electrode = rave_electrode_table$Electrode, Reference = "invalid"),
  file = file.path(cache_path, "cached_reference.csv")
)
yaml <- subject$preprocess_settings$path
raveio::backup_file(yaml)
preproc_data <- subject$preprocess_settings$data
preproc_data$project_name <- subject$project_name
preproc_data$subject_code <- subject$subject_code
preproc_data$channels <- rave_electrode_table$Electrode
preproc_data$exclchan <- NULL
preproc_data$epichan <- NULL
preproc_data$badchan <- NULL
wavelet_params <- preproc_data$wavelet_params
wavelet_params$channels <- wavelet_params$electrodes
wavelet_params$target_srate <- wavelet_params$downsample_to
wavelet_params$wave_num <- wavelet_params$cycle
preproc_data$wavelet_log <- list(wavelet_params)
preproc_data$checklevel <- 4

electrode <- raveio::new_electrode(subject = subject, number = subject$electrodes[[1]])
block_data <- electrode$load_blocks(blocks = block,
                                    type = "wavelet-coefficient",
                                    simplify = TRUE)
power_size <- dim(block_data)[[1]]
raveio::safe_write_csv(
  data.frame(
    Block = block,
    Time = seq(0, by = 1 / wavelet_coef_sample_rate, length.out = power_size)
  ),
  row.names = FALSE,
  file.path(subject$meta_path, 'time_points.csv')
)
subject$preprocess_settings$save()
message("Done!!! run `rave::start_rave2()` to finalize preprocessing and use `rave::start_rave()` to explore!")
