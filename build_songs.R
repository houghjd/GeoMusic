# =============================================================================
# build_songs.R — run to create/refresh data/songs.csv from your audio files
# -----------------------------------------------------------------------------
# Expects audio files named Region_Country.mp3 (e.g. EasternAfrica_Kenya.mp3).
# - Places each dot at the visual centre of its country (or the Navajo Nation)
# - Downloads the Navajo Nation boundary once, to data/navajo_nation.geojson
# - Fills in song details from data/song_details.csv (your spreadsheet)
# - Keeps any song details already typed into data/songs.csv
#
# First time only:  install.packages("tigris")
# Run in the Console with:  source("build_songs.R")
# =============================================================================

library(sf)
sf_use_s2(FALSE)

audio_dir <- "www/audio"
files <- list.files(audio_dir, pattern = "\\.(mp3|m4a|aac|ogg|wav)$", ignore.case = TRUE)
files <- files[!grepl("^prac_", files, ignore.case = TRUE)]   # practice tracks stay off the map
if (length(files) == 0) stop("No audio files found in ", audio_dir)

stem    <- tools::file_path_sans_ext(files)
region  <- sub("_[^_]*$", "", stem)   # text before the last underscore
country <- sub("^.*_", "", stem)      # text after the last underscore

# "UnitedStatesofAmerica" -> "United States of America"
pretty <- function(x) {
  x <- gsub("([a-z])(and|of)([A-Z])", "\\1 \\2 \\3", x)
  x <- gsub("([a-z])([A-Z])", "\\1 \\2", x)
  gsub("\\+", " + ", x)
}
squash <- function(x) tolower(gsub("[^A-Za-z]", "", x))

# --- Visual centre of a shape -------------------------------------------------
# The point inside the shape's biggest piece that is farthest from its edges. It
# always lands inside the shape and away from its edges and holes (for the
# Navajo Nation, that keeps the dot out of the Hopi Reservation in the middle).
visual_centre <- function(geom, n = 40) {
  parts   <- st_cast(geom, "POLYGON")
  biggest <- parts[which.max(as.numeric(st_area(parts)))]
  # Lay a grid of points over the shape, keep the ones inside it, and pick the
  # one farthest from any edge
  grid   <- st_make_grid(biggest, n = n, what = "centers")
  inside <- grid[lengths(st_intersects(grid, biggest)) > 0]
  if (length(inside) == 0) return(st_point_on_surface(biggest))
  dist_to_edge <- as.numeric(st_distance(inside, st_boundary(biggest)))
  inside[which.max(dist_to_edge)]
}

# c(lng, lat) of a single point
point_xy <- function(pt) {
  xy <- st_coordinates(pt)
  c(unname(xy[1, "X"]), unname(xy[1, "Y"]))
}

# --- Countries ----------------------------------------------------------------
countries <- st_read("data/ne_110m_admin_0_countries.json", quiet = TRUE)
geoms <- st_set_crs(st_geometry(countries), NA)   # treat lat/lng as a flat grid
message("Finding the centre of each country (takes a few seconds)...")
anchor <- t(vapply(seq_along(geoms), function(i) point_xy(visual_centre(geoms[i])), numeric(2)))

name_cols <- intersect(c("NAME", "NAME_LONG", "ADMIN", "FORMAL_EN", "NAME_EN"), names(countries))
lookup <- do.call(rbind, lapply(name_cols, function(col) {
  data.frame(key = squash(countries[[col]]), lng = anchor[, 1], lat = anchor[, 2])
}))

# --- Navajo Nation ------------------------------------------------------------
navajo_file <- "data/navajo_nation.geojson"
if (!file.exists(navajo_file)) {
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop('Run install.packages("tigris") in the Console first, then source this script again.')
  }
  message("Downloading the Navajo Nation boundary from the US Census Bureau...")
  areas  <- tigris::native_areas(cb = TRUE, progress_bar = FALSE)
  navajo <- areas[grepl("^Navajo Nation", areas$NAME), ]
  if (nrow(navajo) == 0) stop("Couldn't find the Navajo Nation in the Census file.")
  navajo <- st_transform(navajo, 4326)
  navajo <- st_sf(NAME = "Navajo Nation", geometry = st_union(st_geometry(navajo)))
  st_write(navajo, navajo_file, delete_dsn = TRUE, quiet = TRUE)
  message("Saved ", navajo_file)
}
navajo <- st_read(navajo_file, quiet = TRUE)

# Work in metres (UTM zone 12N covers the Navajo Nation) for an accurate centre
nn_centre <- visual_centre(st_geometry(st_transform(navajo, 26912)), n = 80)
nn_xy <- point_xy(st_transform(nn_centre, 4326))

manual <- data.frame(key = "navajonation", lng = nn_xy[1], lat = nn_xy[2])
lookup <- rbind(manual, lookup)
lookup <- lookup[!is.na(lookup$key) & lookup$key != "" & !duplicated(lookup$key), ]

# --- Build the song table -----------------------------------------------------
m <- match(squash(country), lookup$key)

songs <- data.frame(
  id              = seq_along(files),
  country         = pretty(country),
  region          = pretty(region),
  track           = "",
  artist          = "",
  genre           = "",
  language        = "",
  year            = "",
  timestamp       = "",
  mirage_event_id = "",
  lat             = round(lookup$lat[m], 4),
  lng             = round(lookup$lng[m], 4),
  audio_file      = files,
  spotify_url     = "",
  youtube_url     = "",
  stringsAsFactors = FALSE
)

# Stop here rather than save a file the map can't use
no_xy <- !is.na(m) & (is.na(songs$lat) | is.na(songs$lng))
if (any(no_xy)) {
  stop("Coordinates came out blank for: ", paste(songs$audio_file[no_xy], collapse = ", "),
       ". songs.csv was not changed.")
}

# --- Carry over details already typed into songs.csv --------------------------
out <- "data/songs.csv"
detail_cols <- c("track", "artist", "genre", "language", "year", "timestamp",
                 "mirage_event_id", "spotify_url", "youtube_url", "description")
if (file.exists(out)) {
  old <- read.csv(out, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, fileEncoding = "UTF-8")
  if (!"track" %in% names(old) && "title" %in% names(old)) {
    names(old)[names(old) == "title"] <- "track"
  }
  if ("audio_file" %in% names(old)) {
    file.copy(out, "data/songs_backup.csv", overwrite = TRUE)
    idx <- match(songs$audio_file, old$audio_file)
    for (col in intersect(detail_cols, names(old))) {
      if (!col %in% names(songs)) songs[[col]] <- ""
      vals <- old[[col]][idx]
      songs[[col]][!is.na(vals)] <- vals[!is.na(vals)]
    }
    # An early version filled "track" with the country name as a placeholder
    songs$track[songs$track == songs$country] <- ""
    message("Kept existing song details (previous file backed up to data/songs_backup.csv).")
  }
}

# --- Fill in details from your spreadsheet (data/song_details.csv) -------------
# Export the Google Sheet as CSV, name it song_details.csv, upload it to data/.
# Rows are matched to audio files by Country. The spreadsheet wins over
# anything already in songs.csv, so to change a detail, edit the sheet,
# re-export, and run this script again.
details_file <- "data/song_details.csv"
if (file.exists(details_file)) {
  details <- read.csv(details_file, colClasses = "character", na.strings = c("", "NA"),
                      check.names = FALSE, fileEncoding = "UTF-8")

  # Spreadsheet header (letters only, lower case) -> songs.csv column
  header_map <- c(region = "region", country = "country",
                  genres = "genre", genre = "genre",
                  artist = "artist", track = "track",
                  languagesung = "language", language = "language",
                  yearreleased = "year", year = "year",
                  excerpttimes = "timestamp", timestamp = "timestamp",
                  mirageeventid = "mirage_event_id",
                  youtubeurl = "youtube_url", spotifyid = "spotify_url", spotifyurl = "spotify_url")
  keys <- squash(names(details))
  names(details)[keys %in% names(header_map)] <- header_map[keys[keys %in% names(header_map)]]
  if (!"country" %in% names(details)) stop(details_file, " needs a Country column.")

  details <- details[!is.na(details$country), ]
  for (col in names(details)) {
    v <- trimws(details[[col]])
    v[v %in% c("...", "\u2026")] <- NA          # "..." placeholders count as blank
    details[[col]] <- v
  }
  if ("timestamp" %in% names(details)) {        # "0:25 -0:40" -> "0:25–0:40"
    details$timestamp <- gsub("\\s*[-\u2013]\\s*", "\u2013", details$timestamp)
  }

  idx <- match(squash(songs$country), squash(details$country))
  fill_cols <- intersect(c("region", "country", detail_cols), names(details))
  for (col in fill_cols) {
    if (!col %in% names(songs)) songs[[col]] <- ""
    vals <- details[[col]][idx]
    songs[[col]][!is.na(vals)] <- vals[!is.na(vals)]
  }

  no_row  <- songs$audio_file[is.na(idx)]
  unused  <- setdiff(squash(details$country), squash(songs$country))
  message("Filled in details for ", sum(!is.na(idx)), " songs from ", details_file, ".")
  if (length(no_row) > 0) message("No spreadsheet row for: ", paste(no_row, collapse = ", "))
  if (length(unused) > 0) {
    message("Spreadsheet countries with no audio file: ",
            paste(details$country[squash(details$country) %in% unused], collapse = ", "))
  }
}

write.csv(songs, out, row.names = FALSE, na = "", fileEncoding = "UTF-8")

missing <- files[is.na(m)]
message("Saved ", nrow(songs), " songs to ", out, ".")
if (length(missing) > 0) {
  message("No coordinates found for: ", paste(missing, collapse = ", "),
          "\nAdd them to the `manual` table in this script and run it again.")
}
