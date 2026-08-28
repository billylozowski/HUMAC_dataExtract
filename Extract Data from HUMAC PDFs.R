# this code extracts data from HUMAC .pdf reports

# ------------------------------------------------------------------------------
# metrics & column builders
# ------------------------------------------------------------------------------
# identify report variables
section_patterns <- list(
   peakTorque           = "Peak Torque",
   workPR               = "Work per Repetition",
   averagePowerRep      = "Average Power per Repetition",
   jointAnglePeakTorque = "Joint Angle at Peak Torque",
   ROM                  = "Range of Motion",
   timePeakTorque       = "Time to Peak Torque",
   timePeakTorqueHeld   = "Time Peak Torque Held",
   forceDecay           = "Force Decay Time",
   reciprocalDelay      = "Reciprocal Delay",
   delayTime            = "Delay Time"
)

# metrics that include %BW (only these three)
metrics_with_BW <- c("peakTorque", "workPR", "averagePowerRep")

# build column names for a single metric & speed (BW only for selected metrics)
build_cols_for_metric_speed <- function(metric, speed) {
   has_bw <- metric %in% metrics_with_BW
   if (has_bw) {
      paste0(c(
         paste0("HU_", metric, "_RExt_", speed),
         paste0("HU_", metric, "CV_RExt_", speed),
         paste0("HU_", metric, "BW_RExt_", speed),
         paste0("HU_", metric, "_RFle_", speed),
         paste0("HU_", metric, "CV_RFle_", speed),
         paste0("HU_", metric, "BW_RFle_", speed),
         paste0("HU_", metric, "_LExt_", speed),
         paste0("HU_", metric, "CV_LExt_", speed),
         paste0("HU_", metric, "BW_LExt_", speed),
         paste0("HU_", metric, "_LFle_", speed),
         paste0("HU_", metric, "CV_LFle_", speed),
         paste0("HU_", metric, "BW_LFle_", speed)
      ))
   } else {
      paste0(c(
         paste0("HU_", metric, "_RExt_", speed),
         paste0("HU_", metric, "CV_RExt_", speed),
         paste0("HU_", metric, "_RFle_", speed),
         paste0("HU_", metric, "CV_RFle_", speed),
         paste0("HU_", metric, "_LExt_", speed),
         paste0("HU_", metric, "CV_LExt_", speed),
         paste0("HU_", metric, "_LFle_", speed),
         paste0("HU_", metric, "CV_LFle_", speed)
      ))
   }
}

all_cols_for_speed <- function(speed) {
   # map() + unlist() because each metric may return a different number of column names
   map(names(section_patterns), ~ build_cols_for_metric_speed(.x, speed)) %>% 
      unlist()
}

# ------------------------------------------------------------------------------
# small helpers
# ------------------------------------------------------------------------------
num_tokens <- function(line) {
   str_extract_all(line, "-?\\d+\\.?\\d*")[[1]] %>% 
      as.numeric()
}

# parse the Right/Left nearby lines for one metric (robust to a few layout variants)
parse_block_rl <- function(lines, start_idx) {
   # take following 6 lines (safety)
   block <- lines[(start_idx + 1) : min(length(lines), start_idx + 6)]
   right_line <- block[str_detect(block, regex("^\\s*Right", ignore_case = TRUE))] %>% first()
   left_line  <- block[str_detect(block, regex("^\\s*Left",  ignore_case = TRUE))] %>% first()
   list(right = if (!is.na(right_line)) num_tokens(right_line) else numeric(0),
        left  = if (!is.na(left_line))  num_tokens(left_line)  else numeric(0))
}

# parse metrics within a speed-specific chunk of text
# (respects per-metric column counts; %BW only for metrics_with_BW)
parse_metrics_in_chunk <- function(chunk_text, speed_label) {
   lines <- str_split(chunk_text, "\\n") %>% 
      unlist()
   
   # for each metric, produce a numeric vector whose length matches build_cols_for_metric_speed()
   per_metric_vals <- map(names(section_patterns), function(metric) {
      col_names_metric <- build_cols_for_metric_speed(metric, speed_label)
      ncols <- length(col_names_metric)
      
      pat <- section_patterns[[metric]]
      idx <- which(str_detect(lines, fixed(pat, ignore_case = TRUE)))
      if (length(idx) == 0) return(rep(NA_real_, ncols))
      
      parsed <- parse_block_rl(lines, idx[[1]])
      r <- parsed$right; l <- parsed$left
      
      has_bw <- metric %in% metrics_with_BW
      
      # prepare an output vector of the correct length (NA-filled)
      out <- rep(NA_real_, ncols)
      
      if (has_bw) {
         # indices for BW-including metrics (12 cols):
         # 1: RExt value, 2: RExt CV, 3: RExt BW,
         # 4: RFle value, 5: RFle CV, 6: RFle BW,
         # 7: LExt value, 8: LExt CV, 9: LExt BW,
         # 10: LFle value,11: LFle CV,12: LFle BW
         if (length(r) >= 6) out[1:6] <- r[1:6] else if (length(r) == 4) out[c(1,2,4,5)] <- r else if (length(r) == 3) out[c(1,2,3)] <- r else if (length(r) == 2) out[c(1,4)] <- r
         if (length(l) >= 6) out[7:12] <- l[1:6] else if (length(l) == 4) out[c(7,8,10,11)] <- l else if (length(l) == 3) out[c(7,8,9)] <- l else if (length(l) == 2) out[c(7,10)] <- l
      } else {
         # indices for no-BW metrics (8 cols):
         # 1: RExt value, 2: RExt CV,
         # 3: RFle value, 4: RFle CV,
         # 5: LExt value, 6: LExt CV,
         # 7: LFle value, 8: LFle CV
         if (length(r) >= 4) {
            out[c(1,2,3,4)] <- r[1:4]
         } else if (length(r) == 3) {
            out[c(1,2,3)] <- r
         } else if (length(r) == 2) {
            out[c(1,3)] <- r
         } else if (length(r) == 1) {
            out[1] <- r[1]
         }
         
         if (length(l) >= 4) {
            out[c(5,6,7,8)] <- l[1:4]
         } else if (length(l) == 3) {
            out[c(5,6,7)] <- l
         } else if (length(l) == 2) {
            out[c(5,7)] <- l
         } else if (length(l) == 1) {
            out[5] <- l[1]
         }
      }
      
      out
   })
   
   # flatten values and create final col name vector (variable-length per metric)
   vals <- per_metric_vals %>% flatten_dbl()
   col_names <- map(names(section_patterns), ~ build_cols_for_metric_speed(.x, speed_label)) %>% 
      unlist()
   
   # safety: length check / pad if necessary
   if (length(col_names) != length(vals)) {
      warning("column name / value length mismatch in parse_metrics_in_chunk(): padding with NA")
      L <- max(length(col_names), length(vals))
      if (length(col_names) < L) col_names <- c(col_names, rep(paste0("extra_col_", seq_len(L - length(col_names))), length.out = L - length(col_names)))
      if (length(vals) < L) vals <- c(vals, rep(NA_real_, L - length(vals)))
   }
   
   set_names(as.list(vals), col_names)
}

# ------------------------------------------------------------------------------
# locate speed chunks
# ------------------------------------------------------------------------------
# looks for nearby "Speed" lines or speed numbers.
find_speed_chunks <- function(text_pages) {
   lines <- str_split(text_pages, "\\n") %>%
      unlist()
   # find indices of lines that mention "60" or "180" in a speed context
   speed_idx_60 <- which(str_detect(lines, regex("speed.*60|60\\s*deg|60°|60\\s*d/s|60/60", ignore_case = TRUE)))
   speed_idx_180 <- which(str_detect(lines, regex("speed.*180|180\\s*deg|180°|180\\s*d/s|180/180", ignore_case = TRUE)))
   # fallback: if none found, try any lines containing isolated 60 or 180
   if (length(speed_idx_60) == 0) speed_idx_60 <- which(str_detect(lines, regex("\\b60\\b")))
   if (length(speed_idx_180) == 0) speed_idx_180 <- which(str_detect(lines, regex("\\b180\\b")))
   # build chunks: from first 60 index to before the first 180 index -> 60 chunk
   first60 <- if (length(speed_idx_60)) min(speed_idx_60) else NA_integer_
   first180 <- if (length(speed_idx_180)) min(speed_idx_180) else NA_integer_
   text_full <- paste(lines, collapse = "\n")
   chunk60 <- chunk180 <- ""
   if (!is.na(first60) && !is.na(first180)) {
      if (first60 < first180) {
         # 60 then 180
         chunk60 <- paste(lines[first60:(first180 - 1)], collapse = "\n")
         chunk180 <- paste(lines[first180:length(lines)], collapse = "\n")
      } else {
         # 180 appears before 60
         chunk180 <- paste(lines[first180:(first60 - 1)], collapse = "\n")
         chunk60 <- paste(lines[first60:length(lines)], collapse = "\n")
      }
   } else if (!is.na(first60)) {
      chunk60 <- paste(lines[first60:length(lines)], collapse = "\n")
   } else if (!is.na(first180)) {
      chunk180 <- paste(lines[first180:length(lines)], collapse = "\n")
   } else {
      # no explicit speed markers -> return full text as both (parser will find metrics or NA)
      chunk60  <- text_full
      chunk180 <- text_full
   }
   list(chunk60 = chunk60, chunk180 = chunk180)
}

# ------------------------------------------------------------------------------
# parse single pdf -> one-row tibble
# ------------------------------------------------------------------------------
parse_single_pdf_to_row <- function(pdf_path) {
   raw_pages <- pdf_text(pdf_path) %>% 
      paste(collapse = "\n")
   chunks  <- find_speed_chunks(raw_pages)
   vals60  <- parse_metrics_in_chunk(chunks$chunk60,  "60")
   vals180 <- parse_metrics_in_chunk(chunks$chunk180, "180")
   # combine into one named list, ensure consistent order
   all_cols <- c(all_cols_for_speed("60"), all_cols_for_speed("180"))
   all_vals <- c(vals60, vals180)
   # ensure missing cols are NA
   missing_cols <- setdiff(all_cols, names(all_vals))
   if (length(missing_cols)) all_vals[missing_cols] <- rep(NA_real_, length(missing_cols))
   tibble_row <- as_tibble_row(all_vals[all_cols])
   tibble_row %>% 
      mutate(file_name = basename(pdf_path))
}

# ------------------------------------------------------------------------------
# directory batch parse
# ------------------------------------------------------------------------------
parse_biodex_pdfs_in_dir <- function(pdf_dir) {
   dir(pdf_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE) %>%
      map_dfr(~ parse_single_pdf_to_row(.x))
}

# ------------------------------------------------------------------------------
# call the function
# ------------------------------------------------------------------------------

library(tidyverse)
library(pdftools)

sport   <- "WBB"                            # define the sport
session <- "2026_07"                        # define the session date

# if the .pdf files are not in a uniform folder structure, replace the folder definition with the following:
# folder <- "path/to/your/folder"
folder_HUMAC <- paste0(sport, "/Data/", session, "/HUMAC")
folder_OUT   <- paste0(sport, "/Data/", session)
dir.create(folder_OUT, showWarnings = FALSE, recursive = TRUE)
session.data <- parse_biodex_pdfs_in_dir(folder_HUMAC) %>%
   mutate(session = session) %>%            # create a column for the session date
   select(file_name, session, everything()) # reorder columns so the file name and session are idx1 & idx2
write_csv(session.data, file.path(folder_OUT, paste0("Extracted HUMAC Data for ", sport, " - ", session, ".csv")))
