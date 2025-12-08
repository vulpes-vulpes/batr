# MIT License

# Copyright (c) 2017 David A. Riggs

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Parse ISO 8601 subset timestamps
.parse.timestamp <- function(s) {
  if (is.null(s) || is.na(s) || s == "") {
    return(NA)
  } else if (endsWith(s, "Z")) {
    # UTC
    return(strptime(s, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  } else if (length(gregexpr(":", s)[[1]]) == 3) {
    # UTC offset
    len <- nchar(s)
    utc_offset <- strtoi(substr(s, len - 5, len - 3), base = 10) # UTC offset hours, eg: 4
    tz <- paste("Etc/GMT", sprintf("%+d", utc_offset), sep = "") # timezone, eg: "Etc/GMT+4"
    return(strptime(s, "%Y-%m-%dT%H:%M:%S", tz = tz))
  } else {
    # local
    return(strptime(s, "%Y-%m-%dT%H:%M:%S"))
  }
}


#' Maps metadata keys to a data type coercion function
data.types <- list(
  `Filter HP` = as.double,
  `Filter LP` = as.double,
  Humidity = as.double,
  Length = as.double,
  `Loc Accuracy` = as.integer,
  `Loc Elevation` = as.double,
  Note = function(val) gsub("\\\\n", "\n", val),
  Samplerate = as.integer,
  # `Species Auto ID`=?, `Species Manual ID`=?,  # TODO: comma separated
  # Tags=?,  # TODO: comma separated
  TE = function(val) if (is.na(val) || is.null(val) || val == "") 1 else as.integer(val),
  `Temperature Ext` = as.double, `Temperature Int` = as.double,
  Timestamp = .parse.timestamp
)


#' Read a single GUANO file
#'
#' @param filename The GUANO filename or path
#' @return list of named metadata fields
read.guano <- function(filename) {
  f <- file(filename, "rb")
  # read RIFF header as raw bytes to avoid NUL-byte warnings
  riff_id_raw <- readBin(f, what = "raw", n = 4L)
  if (length(riff_id_raw) == 0) {
    return(NULL)
  }
  riff.id <- rawToChar(riff_id_raw, multiple = FALSE)
  if (length(riff.id) == 0 || riff.id != "RIFF") {
    close(f)
    return(NULL)
  }
  riff.size <- readBin(f, integer(), size = 4, endian = "little")
  wave_id_raw <- readBin(f, what = "raw", n = 4L)
  if (length(wave_id_raw) == 0) {
    close(f)
    return(NULL)
  }
  wave.id <- rawToChar(wave_id_raw, multiple = FALSE)
  if (length(wave.id) == 0 || wave.id != "WAVE") {
    close(f)
    return(NULL)
  }

  read.subchunk <- function() {
    # read chunk ID as raw bytes to avoid NUL-byte warnings
    id_raw <- readBin(f, what = "raw", n = 4L)
    if (length(id_raw) == 0) {
      return(NULL)
    }
    id <- rawToChar(id_raw, multiple = FALSE)
    if (length(id) == 0 || id == "") {
      return(NULL)
    }
    size <- readBin(f, integer(), size = 4, endian = "little")
    list(id = id, size = size)
  }

  skip.subchunk <- function(chunk) {
    # print(sprintf("Skipping subchunk '%s' ...", chunk$id))
    pos <- seek(f, NA)
    seek(f, pos + chunk$size)
  }

  md <- list()

  while (!is.null(chunk <- read.subchunk())) {
    if (chunk$id != "guan") {
      skip.subchunk(chunk)
      next
    }
    md[["File Path"]] <- normalizePath(filename)
    md[["File Name"]] <- basename(filename)
    # read chunk as raw bytes then convert, trimming trailing NULs to avoid warning
    raw_bytes <- readBin(f, what = "raw", n = chunk$size)
    if (length(raw_bytes) == 0) {
      md.txt <- ""
    } else {
      # find last non-NUL byte (0x00) to trim trailing padding
      non_nul_idx <- which(raw_bytes != as.raw(0L))
      if (length(non_nul_idx) == 0) {
        md.txt <- "" # all NULs
      } else {
        last_idx <- max(non_nul_idx)
        md.txt <- rawToChar(raw_bytes[1:last_idx], multiple = FALSE)
        Encoding(md.txt) <- "UTF-8"
      }
    }
    for (line in strsplit(md.txt, "\n")[[1]]) {
      line <- trimws(line)
      if (line == "") {
        next
      }
      toks <- strsplit(sub(":", "\n", line), "\n")
      key <- trimws(toks[[1]][1])
      val <- trimws(toks[[1]][2])
      if (is.na(key) || is.null(key) || key == "") {
        next
      }
      if (!is.null(data.types[[key]])) {
        fn <- data.types[[key]]
        # Defensive: ensure the mapping is a callable function before applying
        if (!is.function(fn)) {
          close(f)
          stop(sprintf("read.guano: expected a function for data.types[['%s']] but found %s", key, typeof(fn)))
        }
        # Apply coercion safely (allow functions to handle NA/empty themselves)
        val <- tryCatch(
          fn(val),
          error = function(e) {
            close(f)
            stop(sprintf("read.guano: error applying data.types[['%s']]: %s", key, conditionMessage(e)), call. = FALSE)
          }
        )
      }
      md[[key]] <- val
    }
    if ("Loc Position" %in% names(md)) {
      md[["Loc Position"]] <- gsub("\t", " ", md[["Loc Position"]])
      coords <- lapply(strsplit(md[["Loc Position"]], " "), as.double)[[1]]
      md[["Loc Position Lat"]] <- coords[1]
      md[["Loc Position Lon"]] <- coords[2]
      md[["Loc Position"]] <- NULL
    }
  }

  close(f)
  return(md)
}
