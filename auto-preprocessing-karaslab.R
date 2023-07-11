#' @author Zhengjia Wang
#' @date July 09, 2023
#' @license Apache-2.0
#' 
#' @title This file is for Karas Lab to auto-preprocess iEEG data 
#' 
#' @param project_name RAVE project name to use
#' @param subject_code Subject code to import
#' @param blocks Session blocks
#' @param electrode_channels electrode channels to import, e.g. "1-10,15-20"
#' @param sample_rate sample rate of raw voltage potentials; default is 1000
#' @param raw_format the raw signal formats, run 
#' `raveio::IMPORT_FORMATS[c(1:4, 7)]` to see the options; default is
#' `"Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block"`
#' @param notch_filter_lowerbound lower-bound for Notch filters; default
#' is `c(59, 118, 178)`
#' @param notch_filter_upperbound lower-bound for Notch filters; must
#' have the same length as `notch_filter_lowerbound`; default is 
#' `c(61, 122, 182)`
#' @param notch_diagnostic_plots whether to generate diagnostic plots for Notch
#' filters
#' @param downsample_before_wavelet Choices are 1, 2, 4, 8, down-sample signals
#' before wavelet; watch out for Nyquist frequency. For best quality, do not
#' down-sample before wavelet. However if you do, then the Nyquist frequency 
#' will be `sample_rate / (2.0 * downsample_before_wavelet)`
#' @param wavelet_precision "float" or "double", no much differences (10^-6 
#' floating differences); default is `"float"`
#' @param target_sample_rate time-frequency amplitude sample rate; default is 
#' `100` (suggested)
#' @param frequencies wavelet frequencies to get; default is 2-200 at step 2
#' @param kernel_table do not change if you don't know what's this
#' @param watch_progress whether to watch the progress
#' @returns A RAVE-preprocessed subject
#' 
#' @examples
#' 
#' auto_preprocessing <- raveio::load_snippet("auto-preprocessing-karaslab", local = FALSE)
#' auto_preprocessing(project_name = "test",
#'                    subject_code = "jh103",
#'                    blocks = "presurgery_ictal_ecog_02",
#'                    electrode_channels = "1-20",
#'                    downsample_before_wavelet = 2,
#'                    frequencies = seq(2, 200, by = 20),
#'                    notch_diagnostic_plots = FALSE, watch_progress = TRUE)
#' 
#' 
#'
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------
# Do NOT remove this line
# Using `%?<-%` meaning if left-hand variable is unset, then use right-hand side 
# variables, otherwise use user-defined
`%?<-%` <- dipsaus::`%?<-%`

# Here are the variables to edit if you run as a script

# DIPSAUS DEBUG START
# project_name <- "test"
# subject_code <- "jh103"
# blocks <- c("presurgery_ictal_ecog_02")
# electrode_channels <- "1-20"

# use whatever is provided
sample_rate  %?<-%  1000
# Check `raveio::IMPORT_FORMATS[c(1:4, 7)]`
raw_format %?<-% "Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block"

# --- Notch filter
notch_filter_lowerbound  %?<-%  c(59, 118, 178)
notch_filter_upperbound  %?<-%  c(61, 122, 182)
notch_diagnostic_plots %?<-% FALSE

# --- Wavelet
# Choices are 1, 2, 4, 8, watch out for new_Nyquist = sample_rate / (2 * downsample_before_wavelet)
# for best quality, do not down-sample
# for speed,
downsample_before_wavelet  %?<-%  1
# float or double, differences are 10^-6
wavelet_precision  %?<-% "float"
# down-sample after wavelet, power sample rate (do NOT change unless you know what you are doing)
target_sample_rate  %?<-%  100
# power/amplitude/phase frequencies
frequencies  %?<-%  seq(2, 200, by = 2)
# Wavelet cycles, do NOT change unless you know what you are doing
kernel_table  %?<-%  ravetools::wavelet_cycles_suggest(
  freqs = frequencies,
  frequency_range = range(c(2, 200, frequencies)),
  cycle_range = c(3, 20)
)

watch_progress %?<-% interactive()

# ---- ENDS: variables ---------------------------------------------------------

force(project_name)
force(subject_code)
force(blocks)
force(electrode_channels)

# create RAVE subject
# `strict = FALSE` to avoid checking subject
subject <- raveio::RAVESubject$new(project_name = project_name,
                                   subject_code = subject_code,
                                   strict = FALSE)

# Initialize preprocess
rave_preprocess <- raveio::pipeline_collection(
  root_path = file.path(subject$pipeline_path, "rave-preprocess-collections"),
  overwrite = TRUE
)

# print out available pipelines
raveio::pipeline_list()

# ==== Add pipelines ===========================================================

# ---- Pipeline to import signal -----------------------------------------------
pipeline_import <- rave_preprocess$add_pipeline(
  "import_lfp_native", pre_hook = function(inputs, ...) {
    dipsaus::list_to_fastmap2(
      map = inputs,
      list(
        skip_validation = FALSE,
        import_setup__project_name = subject$project_name,
        import_setup__subject_code = subject$subject_code,
        import_channels__electrodes = electrode_channels,
        import_blocks__session_block = blocks,
        import_blocks__format = raw_format,
        force_import = TRUE
      )
    )
  },
  post_hook = function(...) {
    # ---- Pipeline for reference --------------------------------------------------
    # TODO: you need to go to rave::start_rave2() to generate your own
    # Do NOT blindly generate CAR reference, bad channels will screw up everything
    # The goal is to generate a CSV file at
    # file.path(subject$meta_path, "reference_CAR.csv")
    
    # ---- Pipeline for epoch ------------------------------------------------------
    try({
      epoch_table <- lapply(blocks, function(block) {
        block_path <- file.path(subject$preprocess_settings$raw_path, block)
        event_file <- list.files(block_path, "events\\.tsv$", full.names = TRUE)
        if(!length(event_file)) { return(NULL) }
        tbl <- read.csv(event_file, header = TRUE, sep = "\t")
        if(!nrow(tbl)) { return() }
        
        # get recording summary
        recoding <- list.files(block_path, "\\.json$", full.names = TRUE)
        if(length(recoding)) {
          duration <- raveio::load_json(recoding)$RecordingDuration
        } else {
          duration <- Inf
        }
        
        sel <- (tbl$onset > 0.1) & (tbl$onset < duration-0.1)
        if(!any(sel)) { return(NULL) }
        
        tbl <- tbl[sel, ]
        
        data.frame(
          Block = block,
          Time = tbl$onset,
          Trial = seq_len(nrow(tbl)),
          Condition = gsub("[^a-zA-Z0-9_\\-\\.]+", "_", tbl$trial_type)
        )
      })
      epoch_table <- epoch_table[vapply(epoch_table, is.data.frame, FALSE)]
      if(length(epoch_table)) {
        epoch_table <- do.call("rbind", epoch_table)
        raveio::safe_write_csv(
          epoch_table,
          file.path(subject$meta_path, "epoch_auto_generated.csv")
        )
      }
    })
    
  }
)

# ---- Pipeline for notch filter -----------------------------------------------

pipeline_notch_filter <- rave_preprocess$add_pipeline(
  "notch_filter",
  # set names to "diagnostic_plots" if you want to generate diagnostic_plots
  names = ifelse(notch_diagnostic_plots, "diagnostic_plots", "apply_notch"),
  deps = pipeline_import$id,
  pre_hook = function(inputs, path) {
    dipsaus::list_to_fastmap2(
      map = inputs,
      list(
        notch_filter_lowerbound = notch_filter_lowerbound,
        notch_filter_upperbound = notch_filter_upperbound,
        project_name = subject$project_name,
        subject_code = subject$subject_code
      )
    )
    path <- raveio::dir_create2(path)
    inputs$diagnostic_plot_params$path <- file.path(path, "diagnostic_plot.pdf")
  }
)

# ---- Pipeline for wavelet ----------------------------------------------------

pipeline_wavelet <- rave_preprocess$add_pipeline(
  "wavelet_module", deps = pipeline_notch_filter$id,
  pre_hook = function(inputs, ...) {
    dipsaus::list_to_fastmap2(
      map = inputs,
      list(
        project_name = subject$project_name,
        subject_code = subject$subject_code,
        kernel_table = kernel_table,
        pre_downsample = as.integer(downsample_before_wavelet),
        precision = wavelet_precision,
        target_sample_rate = target_sample_rate
      )
    )
  }
)

# ==============================================================================

if( watch_progress ) {
  # build (optional)
  rave_preprocess$build_pipelines()
  
  # monitor (optional)
  scheduler <- rave_preprocess$get_scheduler()
  scheduler$with_activated({
    targets::tar_watch(
      targets_only = TRUE,
      display = "graph",
      displays = c("graph", "summary"),
      supervise = TRUE
    )
  })
}

rave_preprocess$run()

