project_name <- "ds005953"
subject_number <- "01"

good_channels <- dipsaus::parse_svec("1-16,18-51,53-76,78-80,82-94,96-115,117")

# ---- Initialize subject ------------------------------------------------------
# In RAVE, the imported subject code will be project + "_" + sub_number
subject_code <- sprintf("%s_%s", project_name, subject_number)

# Create subject, strict = FALSE to ignore checks
rave_subject <- raveio::RAVESubject$new(
  project_name = project_name, subject_code = subject_code, strict = FALSE)

# ---- Download from openneuro -------------------------------------------------
openneuro <- raveio::load_snippet("download-openneuro")
target_path <- openneuro(dataset = project_name)

# ---- Import subject from downloads -------------------------------------------
import_bids <- raveio::load_snippet("import-bids")
import_bids(bids_project_path = target_path, bids_subject_code = subject_number)


# ---- Preprocessing: Notch filter ---------------------------------------------

# Notch filter pipeline
pipeline_notch_filter <- ravepipeline::pipeline("notch_filter")

# Get current settings
# dput(pipeline_notch_filter$get_settings())

# set inputs
pipeline_notch_filter$set_settings(
  subject_code = subject_code,
  project_name = project_name,
  notch_filter_lowerbound = c(59, 118, 178),
  notch_filter_upperbound = c(61, 122, 182)
)

# Run notch filter
pipeline_notch_filter$run("apply_notch")

# ---- Preprocessing: Wavelet --------------------------------------------------
pipeline_wavelet <- ravepipeline::pipeline("wavelet_module")
# dput(pipeline_wavelet$get_settings())

# Get recommended wavelet cycles
kernel_table <- ravetools::wavelet_cycles_suggest(
  freqs = seq(2, 200, by = 4),
  frequency_range = c(2, 200),
  cycle_range = c(3, 20)
)
pipeline_wavelet$set_settings(
  subject_code = subject_code,
  project_name = project_name,
  kernel_table = kernel_table, 
  pre_downsample = 4, 
  precision = "float", 
  target_sample_rate = 100
)

# Run wavelet
pipeline_wavelet$run("wavelet_params")


# ---- Re-reference: generate references from `good_channels` ------------------
raveio::generate_reference(rave_subject, electrodes = good_channels)

# load reference template and set the "Common Average Reference"
reference_table <- rave_subject$get_reference("default")
reference_table$Type <- "Common Average Reference"
reference_table$Reference <- sprintf("ref_%s", dipsaus::deparse_svec(good_channels))

# Save to default reference
raveio::safe_write_csv(
  x = reference_table,
  file = file.path(rave_subject$meta_path, "reference_default.csv")
)


# ---- Load repository ---------------------------------------------------------

repository <- raveio::prepare_subject_power(
  subject = rave_subject,
  electrodes = 114, 
  reference_name = "default", 
  epoch_name = "status_good", 
  time_windows = c(-0.5, 1)
)

# baseline correction
raveio::power_baseline(repository, baseline_windows = c(0.75, 1), method = "decibel")

# obtain baseline power spectrogram
baselined <- repository$power$baselined

# Frequency x Time x Trial x Electrode
dim(baselined)

# Subset by epoch
epoch_table <- repository$epoch$table

# generate color pallete
pal <- ravebuiltins::get_heatmap_palette("BlueGrayRed")
pal <- colorRampPalette(pal)(255)

# Plot average baselined power spectrogram from condition 
plot_cond <- function(strimuli, zlim = c(-15, 15)) {
  selected_trials <- epoch_table$Trial[epoch_table$Condition %in% strimuli]
  
  # subset power spectrogram with selected trials
  baselined_subset <- subset(baselined, Trial ~ Trial %in% selected_trials)
  
  # calculate mean spectrogram over electrodes and trials
  # result is time x frequency
  power_over_freq_time <- ravetools::collapse(baselined_subset, keep = c(2, 1))
  
  # get axis
  time_points <- repository$time_points
  frequencies <- repository$frequency
  # zlim <- max(abs(range(power_over_freq_time))) * c(-1, 1)
  power_over_freq_time[power_over_freq_time < zlim[[1]]] <- zlim[[1]]
  power_over_freq_time[power_over_freq_time >= zlim[[2]]] <- zlim[[2]]
  
  image(x = time_points, y = frequencies, z = power_over_freq_time, col = pal, zlim = zlim)
}

plot_cond("1")

par(mfrow = c(2, 4), mar = c(2.1, 2.1, 2.1, 0.1))
zlim <- c(-15, 15)

# Noises
plot_cond("1", zlim = zlim)
plot_cond("2", zlim = zlim)
plot_cond("3", zlim = zlim)

legend_y <- seq(zlim[[1]], zlim[[2]], length.out = length(pal))
image(x = 0, y = legend_y, matrix(legend_y, nrow = 1), col = pal, axes = FALSE, asp = 1)
axis(2, c(zlim, 0))

# Bars
plot_cond("4")
plot_cond("5")
plot_cond("6")
plot_cond("7")


# export repository for future analysis
raveio::rave_export(repository, '~/rave_data/export_dir/')
