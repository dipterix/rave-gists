#' @author Zhengjia Wang
#' @date Feb 08, 2023
#' @license Apache-2.0
#' 
#' @title This file converts RAVE 1.0 Power Explorer exported `.fst` file 
#' to common format `.hdf5` or `.csv`, with the FreeSurfer label hemisphere
#' removed (as requested by Kaitlyn Davis)
#' 
#' @param source_file path to the `.fst` file
#' @param export_path where the export should be saved
#' @param col_name (optional) which column contain the FreeSurfer labels
#' @param export_format (optional) export format; choices are `hdf5` or `csv`
#'
#' END OF DOC
NULL

# ---- variables ---------------------------------------------------------------

# source_file <- '~/Downloads/HighGam_001-20230127-212402.fst'
# export_path <- "~/Desktop/export"

# col_name <- "VAR_IS_ROI_FreeSurferLabel"

# available ones are "csv" or "hdf5"
# export_format <- "hdf5"


# ---- ENDS: variables ---------------------------------------------------------
`%?<-%` <- dipsaus::`%?<-%`
col_name  %?<-% "VAR_IS_ROI_FreeSurferLabel"
export_format %?<-% "hdf5"

# Will change 
# "Left-Cerebral-White-Matter" -> "Cerebral-White-Matter"
# ctx-rh-fusiform -> fusiform
# ctx_lh_S_front_sup -> S_front_sup
# lDMPFC -> DMPFC

# Load FST table
fst_table <- fst::read_fst(source_file, as.data.table = TRUE)

# Load the column
fslabel <- fst_table[[col_name]]

fslabel <- gsub("^(left|right)[\\-]{0,1}", "", fslabel, ignore.case = TRUE)
fslabel <- gsub("^ctx[_\\-][lr]h[_\\-]{0,1}", "", fslabel, ignore.case = TRUE)
fslabel[fslabel %in% c("lVMPFC", "rVMPFC")] <- "VMPFC"
fslabel[fslabel %in% c("lDMPFC", "rDMPFC")] <- "DMPFC"

message("Unique FreeSurfer Labels")
cat(unique(fslabel), sep = ", ")

fst_table$FreeSurferLabel2 <- fslabel

# write to file
use_h5 <- export_format == "hdf5"
export_path <- gsub("\\.[a-zA-Z0-9]+$", "", export_path, ignore.case = TRUE)
export_path <- sprintf("%s.%s", export_path, ifelse(use_h5, "h5", "csv"))

if(file.exists(export_path)) { unlink(export_path) }
if( use_h5 ) {
  for(nm in names(fst_table)) {
    tmp <- fst_table[[nm]]
    is_numeric <- is.numeric(tmp)
    raveio::save_h5(
      x = tmp, file = export_path, name = nm, replace = TRUE, 
      ctype = ifelse(is_numeric, "numeric", "character")
    )
  }
} else {
  data.table::fwrite(x = fst_table, file = export_path)
}


message("Done. Please check your file at: ", export_path)
