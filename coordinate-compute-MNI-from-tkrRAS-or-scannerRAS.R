#' This file imports electrode table (in csv format) and outputs MNI coordinates
#' @author Zhengjia Wang
#' @date Feb 08, 2023
#' @license Apache-2.0
#'
#' @param subject character representing RAVE project+subject (e.g. `demo/DemoSubject`)
#'
#' @param input_filepath path to input csv file, or `NULL`;
#'   When `input_filepath=NULL`, the electrode coordinates will be derived from
#'     `electrodes.csv` at subject's `meta` folder.
#'   When `input_filepath` is specified, the input table should contain at least
#'     five columns: (see the following example, the column names must EXACTLY
#'     match with the given examples)
#'
#'   (example 1, using tkrRAS coordinates)
#'     Electrode Coord_x Coord_y Coord_z Label
#'     1         -11.2   16.3    13.9    LA1
#'     2 ...
#'
#'   (example 2, using tkrRAS coordinates)
#'     Electrode Coord_x Coord_y Coord_z Label
#'     1         -11.2   16.3    13.9    LA1
#'     2 ...
#'
#' @param output_filepath path where output file should be saved. This could be
#'   either a `.csv` file or a `.h5` file, or simply `NULL`:
#'     * If the file path ends with `.h5`, a HDF5 file will be created
#'     * If `output_filepath=NULL`, the table will not be saved unless
#'       `save_to_subject` is set to `TRUE`
#'     * All other cases will create a `.csv` file
#' @param save_to_subject whether to alter the current subject's electrodes.csv
#'   located at `meta/electrodes.csv`

# ---- Global inputs --------------------------------------------------------

subject <- "demo/DemoSubject"

input_filepath <- NULL
# input_filepath <- "~/Dropbox (PENN Neurotrauma)/RAVE/Samples/data/demo/DemoSubject/rave/meta/electrodes.csv"

output_filepath <- tempfile(fileext = ".csv")

# Do not mess with existing electrodes.csv
save_to_subject <- FALSE

# ---- Code -----------------------------------------------------------------

subject_instance <- raveio::as_rave_subject( subject )

brain <- raveio::rave_brain(subject_instance)

if( is.null(input_filepath) ) {
  electrode_table <- subject_instance$meta_data(meta_type = "electrodes")
  input_filepath <- tempfile(fileext = ".csv", pattern = "electrodes_")
  raveio::safe_write_csv(electrode_table, file = input_filepath, quiet = TRUE)
} else if( !file.exists(input_filepath) ) {
  stop("`input_filepath` does not point to any existing file")
}

# convert coordinate system
electrode_table <- raveio::import_electrode_table(
  subject = subject_instance$subject_id,
  path = input_filepath,
  dry_run = !save_to_subject
)

# write to output_filepath

if( !is.null(output_filepath) ) {
  # backup first
  if( file.exists( output_filepath ) ) {
    raveio::backup_file(output_filepath, remove = TRUE)
  }
  
  # check if `output_filepath` is HDF5
  if( endsWith(tolower(output_filepath), ".h5") ) {
    
    # this is HDF5
    for(nm in names(electrode_table)){
      cat("Writing to HDF5:", nm, "                          \r")
      val <- electrode_table[[nm]]
      if(is.numeric(val)) {
        raveio::save_h5(x = val, file = output_filepath, name = nm,
                        replace = TRUE, ctype = "numeric", quiet = TRUE)
      } else {
        raveio::save_h5(x = as.character(val), file = output_filepath,
                        name = nm, replace = TRUE, ctype = "character", quiet = TRUE)
      }
    }
    
  } else {
    raveio::safe_write_csv(electrode_table, file = output_filepath)
  }
  
} else {
  print( electrode_table )
}


message("Done.")

