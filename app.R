# =============================================================================
# app.R — Geographic Representations of Music: Stimulus Set
# -----------------------------------------------------------------------------
# Folder layout:
#   app.R
#   build_songs.R           <- run to create/refresh data/songs.csv (coordinates)
#   data/songs.csv          <- one row per song, with map coordinates
#   data/song_details.csv   <- your spreadsheet: artist, track, genres, Exp 2, ...
#   data/ne_110m_admin_0_countries.json   <- country outlines
#   data/navajo_nation.geojson            <- created by build_songs.R
#   www/audio/*.mp3         <- music files, named in the audio_file column
#
# Packages (once):
#   install.packages(c("shiny", "bslib", "leaflet", "htmltools", "sf",
#                      "ggplot2", "svglite"))
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(sf)
library(fontawesome)   # YouTube / Spotify icons (installed along with shiny)

sf_use_s2(FALSE)

app_title <- "Geographic Representations of Music \u2014 Stimulus Set"

# -----------------------------------------------------------------------------
# 1. Regions and colours
# -----------------------------------------------------------------------------
# The 19 regions, coloured by continent family. Change any hex code to restyle.
region_colours <- c(
  # Africa: warm oranges
  "Northern Africa"   = "#f7dc9c",
  "Western Africa"    = "#f2b766",
  "Middle Africa"     = "#e8955a",
  "Eastern Africa"    = "#f4c99c",
  "Southern Africa"   = "#d68a63",
  # Americas: reds and pinks
  "Northern America"  = "#eea3a3",
  "Central America"   = "#dd7a7a",
  "Caribbean"         = "#f3bfd3",
  "South America"     = "#d38bb4",
  # Asia: greens
  "Western Asia"      = "#d4e6a5",
  "Central Asia"      = "#b3d47c",
  "Southern Asia"     = "#8cc79a",
  "Southeastern Asia" = "#5fae86",
  "Eastern Asia"      = "#a6d6c3",
  # Europe: blues
  "Northern Europe"   = "#a9c8ea",
  "Western Europe"    = "#7fa8da",
  "Southern Europe"   = "#c6d8f2",
  "Eastern Europe"    = "#8d9ed0",
  # Oceania: purple
  "Australia, New Zealand & Melanesia" = "#c7b2df"
)

ocean_colour  <- "#eaf2f8"   # set to "#ffffff" for a plain white background
land_colour   <- "#ffffff"   # country fill when region colours are switched off
border_colour <- "#6e6e6e"   # country and Navajo Nation outlines; lower = darker

# Stimulus markers: Exp 1 only = dark circle, Exp 1 & 2 = white diamond
dot_dark <- "#1f2937"
exp_groups <- c("Exp 1 only", "Exp 1 & 2")

# Letters only, lower case: "South-Eastern Asia" -> "southeasternasia"
squash <- function(x) tolower(gsub("[^A-Za-z]", "", x))

# Any spelling of a region (map file, file names, spreadsheet) -> one of the 19
region_key <- setNames(names(region_colours), squash(names(region_colours)))
region_key[c("australiaandnewzealand", "melanesia", "anzmelanesia",
             "australianewzealandmelanesia")] <- "Australia, New Zealand & Melanesia"
normalise_region <- function(x) unname(region_key[squash(x)])

# -----------------------------------------------------------------------------
# 2. Data
# -----------------------------------------------------------------------------
songs <- read.csv("data/songs.csv", colClasses = "character",
                  na.strings = c("", "NA"), check.names = FALSE, fileEncoding = "UTF-8")

# Older versions of songs.csv used "title" and "location"
if (!"track" %in% names(songs) && "title" %in% names(songs)) {
  names(songs)[names(songs) == "title"] <- "track"
}
if (!"country" %in% names(songs) && "location" %in% names(songs)) {
  songs$country <- sub(" \\(.*$", "", songs$location)
}

missing_core <- setdiff(c("id", "lat", "lng"), names(songs))
if (length(missing_core) > 0) {
  stop("songs.csv is missing columns: ", paste(missing_core, collapse = ", "))
}

detail_cols <- c("country", "region", "track", "artist", "genre", "language", "year",
                 "timestamp", "exp2", "audio_file", "spotify_url", "youtube_url")
for (col in setdiff(detail_cols, names(songs))) songs[[col]] <- NA_character_

# --- Song details from your spreadsheet (data/song_details.csv) --------------
# Matched by country. Whatever is in the spreadsheet wins, so to change a
# detail, edit the spreadsheet and restart the app.
details_file <- "data/song_details.csv"
if (file.exists(details_file)) {
  # Read the raw text first: a spreadsheet re-saved by Excel or Numbers may not
  # be UTF-8, which otherwise makes R stop reading partway through the file
  txt <- rawToChar(readBin(details_file, "raw", file.info(details_file)$size))
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, from = "windows-1252", to = "UTF-8")
  details <- read.csv(text = txt, colClasses = "character", check.names = FALSE,
                      na.strings = c("", "NA", "...", "\u2026"))

  # Spreadsheet header (letters and digits, lower case) -> column name used here
  header_map <- c(country = "country", region = "region",
                  genres = "genre", genre = "genre",
                  artist = "artist", track = "track",
                  languagesung = "language", language = "language", lyricslanguage = "language",
                  yearreleased = "year", year = "year",
                  excerpttimes = "timestamp", excerpttime = "timestamp", timestamp = "timestamp",
                  youtubeurl = "youtube_url", youtube = "youtube_url",
                  spotifyurl = "spotify_url", spotifyid = "spotify_url", spotify = "spotify_url",
                  experiment = "experiment", experiments = "experiment")
  keys <- tolower(gsub("[^A-Za-z0-9]", "", names(details)))
  hit  <- keys %in% names(header_map)
  names(details)[hit] <- header_map[keys[hit]]
  # The Exp 2 column, however it ends up being named ("Exp 2", "In Exp 2?", ...)
  exp2_col <- which(grepl("^(in|also|alsoin)?(exp|experiment)2", keys))
  if (length(exp2_col) > 0) names(details)[exp2_col[1]] <- "exp2"

  # With an Experiment column ("1" or "1 & 2"), rows left blank there are notes
  # or candidate songs, not stimuli, so they're ignored
  if ("experiment" %in% names(details)) {
    details <- details[!is.na(details$experiment) & trimws(details$experiment) != "", ]
    details$exp2 <- ifelse(grepl("2", details$experiment), "yes", "no")
  }

  if ("country" %in% names(details)) {
    idx <- match(squash(songs$country), squash(details$country))
    for (col in setdiff(intersect(detail_cols, names(details)), c("country", "region"))) {
      vals <- trimws(details[[col]][idx])
      ok   <- !is.na(vals) & vals != ""
      songs[[col]][ok] <- vals[ok]
    }
    if (any(is.na(idx))) {
      warning("No row in ", details_file, " for: ",
              paste(songs$country[is.na(idx)], collapse = ", "))
    }
  } else {
    warning(details_file, " needs a Country column; song details not loaded.")
  }
} else {
  warning(details_file, " not found; popups will show dashes until it's added.")
}

songs$timestamp <- gsub("\\s*[-\u2013]\\s*", "\u2013", songs$timestamp)   # "0:25 -0:40" -> "0:25–0:40"
songs$lat <- as.numeric(songs$lat)
songs$lng <- as.numeric(songs$lng)

blank <- function(x) is.na(x) | trimws(x) == ""

# Exp 2 membership: yes / y / true / 1 / x in the spreadsheet's Exp 2 column
songs$in_exp2   <- tolower(trimws(songs$exp2)) %in% c("yes", "y", "true", "1", "x", "\u2713")
songs$exp_group <- ifelse(songs$in_exp2, "Exp 1 & 2", "Exp 1 only")

# Region of each song, as one of the 19
songs$region19 <- normalise_region(songs$region)

# Drop rows with no coordinates so one bad row doesn't break the map
bad_rows <- is.na(songs$lat) | is.na(songs$lng)
if (any(bad_rows)) {
  warning(sum(bad_rows), " row(s) skipped for missing coordinates: ",
          paste(songs$audio_file[bad_rows], collapse = ", "))
  songs <- songs[!bad_rows, ]
}

# --- Background shapes ---------------------------------------------------------
countries <- st_read("data/ne_110m_admin_0_countries.json", quiet = TRUE)
countries <- countries[countries$NAME != "Antarctica", ]   # can't be shown in Leaflet's projection
countries$region19 <- normalise_region(countries$SUBREGION)
countries$region19[countries$NAME == "Fr. S. Antarctic Lands"] <- NA
countries$region_fill <- ifelse(is.na(countries$region19), "#eeeeee",
                                region_colours[countries$region19])

navajo <- if (file.exists("data/navajo_nation.geojson")) {
  st_read("data/navajo_nation.geojson", quiet = TRUE)
} else {
  warning("data/navajo_nation.geojson not found: run source(\"build_songs.R\") to create it.")
  NULL
}

# --- Exp 2 versions (www/audio/exp2/<same name>_Full / _Vocal / _Instrumental) --
exp2_dir      <- "www/audio/exp2"
version_names <- c("Full", "Vocal", "Instrumental")
exp2_files    <- if (dir.exists(exp2_dir)) list.files(exp2_dir, pattern = "\\.mp3$", ignore.case = TRUE) else character(0)

# For one song: named vector of version -> file name in exp2/, for the versions that exist
find_versions <- function(audio_file) {
  if (is.na(audio_file) || audio_file == "") return(character(0))
  stem   <- tools::file_path_sans_ext(audio_file)
  wanted <- tolower(paste0(stem, "_", version_names, ".mp3"))
  hit    <- match(wanted, tolower(exp2_files))
  setNames(exp2_files[hit], version_names)[!is.na(hit)]
}
song_versions <- lapply(songs$audio_file, find_versions)

if (length(exp2_files) > 0) {
  used <- unlist(song_versions, use.names = FALSE)
  stray <- setdiff(exp2_files, used)
  if (length(stray)) warning("Exp 2 files that don't match any song's file name: ",
                             paste(stray, collapse = ", "))
  no_versions <- songs$country[songs$in_exp2 & lengths(song_versions) == 0]
  if (length(no_versions)) warning("Exp 2 songs with no Full/Vocal/Instrumental files yet: ",
                                   paste(no_versions, collapse = ", "))
}

# -----------------------------------------------------------------------------
# 3. Text builders
# -----------------------------------------------------------------------------
# Plain text of a cell, with blanks as ""
val <- function(x) if (length(x) == 0 || blank(x)) "" else trimws(as.character(x))

# <artist>, “<track>” (<year>), skipping whichever parts are blank
recording_text <- function(row) {
  who_what <- c(val(row$artist),
                if (val(row$track) != "") paste0("\u201c", val(row$track), "\u201d") else "")
  out <- paste(who_what[who_what != ""], collapse = ", ")
  if (val(row$year) != "") out <- trimws(paste0(out, " (", val(row$year), ")"))
  out
}

# Hover: <artist>, “<track>” (<year>) | <country> | <genre>
hover_text <- function(row) {
  parts <- c(recording_text(row), val(row$country), val(row$genre))
  parts <- parts[parts != ""]
  if (length(parts) == 0) paste("Song", row$id) else paste(parts, collapse = " | ")
}

# Popup line with a bold label; blank values show a dash
popup_line <- function(label, value) {
  shown <- if (value == "") '<span class="song-missing">&mdash;</span>' else htmlEscape(value)
  sprintf('<p class="song-line"><span class="song-label">%s:</span> %s</p>', label, shown)
}

# Clickable icon that opens a link in a new tab; hidden when there's no link
icon_link <- function(url, icon_name, colour, label) {
  if (val(url) == "") return("")
  sprintf('<a class="song-icon" href="%s" target="_blank" rel="noopener" aria-label="%s" title="%s">%s</a>',
          htmlEscape(val(url), attribute = TRUE), label, label,
          as.character(fa(icon_name, fill = colour, height = "28px")))
}

audio_src <- function(path) {
  paste(vapply(strsplit(path, "/", fixed = TRUE)[[1]], URLencode, character(1), reserved = TRUE),
        collapse = "/")
}

# Buttons that switch the playback bar between the Exp 1 excerpt and the Exp 2 versions
version_switch <- function(audio_file, versions) {
  if (length(versions) == 0) return("")
  btn <- function(label, src, active = FALSE) {
    sprintf('<button type="button" class="ver-btn%s" aria-pressed="%s" data-src="%s" onclick="geoSwitchVersion(this)">%s</button>',
            if (active) " active" else "", if (active) "true" else "false",
            htmlEscape(src, attribute = TRUE), label)
  }
  paste0(
    '<div class="version-switch" role="group" aria-label="Recording version">',
    btn("Exp 1", audio_src(file.path("audio", audio_file)), active = TRUE),
    '<span class="ver-label">Exp 2:</span>',
    paste(vapply(names(versions), function(v) btn(v, audio_src(file.path("audio/exp2", versions[[v]]))),
                 character(1)), collapse = ""),
    '</div>'
  )
}

make_popup <- function(row, versions = character(0)) {
  recording <- recording_text(row)
  if (val(row$timestamp) != "") {
    recording <- paste0(recording, if (recording != "") " \u2014 " else "", val(row$timestamp))
  }

  # Playback bar; the map's JavaScript starts it automatically when the popup opens.
  # URLencode keeps characters like & and + in file names from breaking the link.
  audio <- if (val(row$audio_file) != "") {
    paste0(version_switch(val(row$audio_file), versions),
           sprintf('<audio class="song-audio" controls preload="auto" src="%s"></audio>',
                   audio_src(file.path("audio", val(row$audio_file)))))
  } else {
    '<p class="song-missing">No audio file for this song yet.</p>'
  }

  sprintf(
    '<div class="song-popup">
       %s%s%s
       <p class="song-country">%s</p>
       <div class="song-icons">%s%s</div>
       %s
     </div>',
    popup_line("Recording", recording),
    popup_line("Genres", val(row$genre)),
    popup_line("Lyrics Language", val(row$language)),
    htmlEscape(val(row$country)),
    icon_link(row$youtube_url, "youtube", "#ff0000", "Open on YouTube"),
    icon_link(row$spotify_url, "spotify", "#1db954", "Open on Spotify"),
    audio
  )
}

rows <- lapply(seq_len(nrow(songs)), function(i) songs[i, ])
songs$hover <- vapply(rows, hover_text, character(1))
songs$popup <- vapply(seq_along(rows), function(i) make_popup(rows[[i]], song_versions[[i]]),
                      character(1))

# Dropdown entries: country, plus the track once it's filled in
songs$menu <- ifelse(blank(songs$track), songs$country,
                     paste0(songs$country, " \u2014 ", songs$track))

# -----------------------------------------------------------------------------
# 4. Marker shapes
# -----------------------------------------------------------------------------
svg_uri <- function(svg) paste0("data:image/svg+xml;charset=UTF-8,", URLencode(svg, reserved = TRUE))

circle_svg <- sprintf(
  "<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18'><circle cx='9' cy='9' r='7' fill='%s' stroke='#ffffff' stroke-width='2'/></svg>",
  dot_dark)
diamond_svg <- sprintf(
  "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><polygon points='10,1.5 18.5,10 10,18.5 1.5,10' fill='#ffffff' stroke='%s' stroke-width='2.2'/></svg>",
  dot_dark)
marker_uri <- c("Exp 1 only" = svg_uri(circle_svg), "Exp 1 & 2" = svg_uri(diamond_svg))

marker_icons <- function(df) {
  is_diamond <- df$exp_group == "Exp 1 & 2"
  icons(
    iconUrl     = unname(marker_uri[df$exp_group]),
    iconWidth   = ifelse(is_diamond, 20, 18),
    iconHeight  = ifelse(is_diamond, 20, 18),
    iconAnchorX = ifelse(is_diamond, 10, 9),
    iconAnchorY = ifelse(is_diamond, 10, 9),
    className   = "stimulus-dot"
  )
}

legend_html <- sprintf(
  '<div class="map-legend">
     <div><img src="%s" width="16" height="16" alt=""> Exp 1 only</div>
     <div><img src="%s" width="17" height="17" alt=""> Exp 1 &amp; 2</div>
   </div>',
  marker_uri[["Exp 1 only"]], marker_uri[["Exp 1 & 2"]])

# -----------------------------------------------------------------------------
# 5. Static figure (for the SVG / PNG export)
# -----------------------------------------------------------------------------
figure_crs <- "+proj=eqearth"   # Equal Earth world projection; "EPSG:3857" matches the app

make_figure <- function(groups) {
  library(ggplot2)
  show_regions <- "Region colors" %in% groups
  pts <- songs[songs$exp_group %in% groups, ]

  # Ocean: the outline of the whole globe
  edge <- 180; step <- 1
  ring <- rbind(
    cbind(seq(-edge, edge, step), 90),
    cbind(edge, seq(90, -90, -step)),
    cbind(seq(edge, -edge, -step), -90),
    cbind(-edge, seq(-90, 90, step))
  )
  globe <- st_transform(st_sfc(st_polygon(list(ring)), crs = 4326), figure_crs)

  land <- st_transform(countries, figure_crs)
  land$fill_key <- if (show_regions) ifelse(is.na(land$region19), "Other", land$region19) else "Land"
  fills <- c(region_colours, "Other" = "#eeeeee", "Land" = land_colour)

  p <- ggplot() +
    geom_sf(data = globe, fill = ocean_colour, colour = border_colour, linewidth = 0.3) +
    geom_sf(data = land, aes(fill = fill_key), colour = border_colour, linewidth = 0.2,
            show.legend = show_regions) +
    scale_fill_manual(values = fills, breaks = names(region_colours), name = NULL,
                      drop = TRUE)

  if (!is.null(navajo)) {
    nav_fill <- if (show_regions) region_colours[["Northern America"]] else land_colour
    p <- p + geom_sf(data = st_transform(navajo, figure_crs), fill = nav_fill,
                     colour = border_colour, linewidth = 0.25)
  }

  if (nrow(pts) > 0) {
    pts_sf <- st_transform(st_as_sf(pts, coords = c("lng", "lat"), crs = 4326), figure_crs)
    pts_sf$exp_group <- factor(pts_sf$exp_group, levels = exp_groups)
    p <- p +
      geom_sf(data = pts_sf, aes(shape = exp_group), fill = "#ffffff", colour = dot_dark,
              size = 2.4, stroke = 0.8) +
      scale_shape_manual(values = c("Exp 1 only" = 16, "Exp 1 & 2" = 23), name = NULL, drop = FALSE)
  }

  p +
    coord_sf(crs = figure_crs, expand = FALSE) +
    labs(title = app_title) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 15, margin = margin(0, 0, 8, 0)),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      plot.background = element_rect(fill = "#ffffff", colour = NA),
      plot.margin = margin(12, 12, 12, 12)
    ) +
    guides(fill = guide_legend(ncol = 5, order = 2), shape = guide_legend(order = 1))
}

# -----------------------------------------------------------------------------
# 6. Styling and map JavaScript
# -----------------------------------------------------------------------------
app_css <- sprintf("
  html, body { height: 100%%; margin: 0; }
  .map-wrap { position: relative; height: 100vh; }
  .leaflet-container { background: %s; font-family: inherit; }

  .map-title { top: 12px; left: 12px; z-index: 1000; pointer-events: none;
               background: rgba(255,255,255,0.92); padding: 8px 14px; border-radius: 6px;
               box-shadow: 0 1px 4px rgba(0,0,0,0.15); }
  .map-title h1 { font-size: 18px; font-weight: 700; margin: 0; }

  .map-tools { top: 12px; right: 12px; z-index: 1000; display: flex; gap: 8px;
               align-items: flex-start; background: rgba(255,255,255,0.92); padding: 8px;
               border-radius: 6px; box-shadow: 0 1px 4px rgba(0,0,0,0.15); }
  .map-tools .form-group { margin: 0; }
  .map-tools .selectize-control { width: 260px; }
  .map-tools .btn { white-space: nowrap; }

  .map-legend { background: rgba(255,255,255,0.92); padding: 6px 10px; border-radius: 6px;
                box-shadow: 0 1px 4px rgba(0,0,0,0.15); font-size: 13px; line-height: 1.8; }
  .map-legend img { vertical-align: middle; margin-right: 6px; }

  .stimulus-dot { cursor: pointer; }
  .stimulus-dot:hover { filter: drop-shadow(0 0 3px rgba(0,0,0,0.7)); }

  .song-popup { min-width: 270px; max-width: 330px; font-size: 14px; }
  .song-line { margin: 0 0 6px; line-height: 1.4; }
  .song-label { font-weight: 600; color: #444; }
  .song-country { margin: 8px 0 10px; font-weight: 600; font-size: 15px; }
  .song-missing { color: #999; font-style: italic; }
  .song-icons { display: flex; gap: 14px; margin-bottom: 10px; }
  .song-icon { line-height: 0; border-radius: 6px; }
  .song-icon:hover { opacity: 0.8; }
  .song-icon:focus-visible { outline: 2px solid #000; outline-offset: 3px; }
  .song-audio { width: 100%%; }
  .version-switch { display: flex; flex-wrap: wrap; align-items: center; gap: 4px; margin-bottom: 6px; }
  .ver-label { font-size: 12px; font-weight: 600; color: #555; margin: 0 2px 0 6px; }
  .ver-btn { font-size: 12px; padding: 2px 8px; border: 1px solid #bbb; border-radius: 12px;
             background: #fff; color: #333; cursor: pointer; }
  .ver-btn:hover { background: #f1f1f1; }
  .ver-btn.active { background: #1f2937; border-color: #1f2937; color: #fff; }
  .ver-btn:focus-visible { outline: 2px solid #000; outline-offset: 2px; }
", ocean_colour)

# Zoom buttons in the bottom left (the title sits top left); music starts when a
# popup opens and stops when it closes
map_js <- "
  function(el, x) {
    var map = this;
    L.control.zoom({ position: 'bottomleft' }).addTo(map);
    map.on('popupopen', function(e) {
      var audio = e.popup.getElement().querySelector('audio');
      if (audio) {
        var p = audio.play();
        if (p && p.catch) p.catch(function() {});   // browser blocked autoplay: the play button still works
      }
    });
    map.on('popupclose', function(e) {
      var box = e.popup.getElement();
      var audio = box && box.querySelector('audio');
      if (audio) audio.pause();
    });
  }
"

# -----------------------------------------------------------------------------
# 7. UI
# -----------------------------------------------------------------------------
export_menu <- div(
  class = "dropdown",
  tags$button(class = "btn btn-light btn-sm dropdown-toggle", type = "button",
              `data-bs-toggle` = "dropdown", `aria-expanded` = "false", "Export"),
  tags$ul(
    class = "dropdown-menu dropdown-menu-end",
    tags$li(downloadLink("dl_svg", "Figure (SVG)", class = "dropdown-item")),
    tags$li(downloadLink("dl_png", "Figure (PNG)", class = "dropdown-item")),
    tags$li(downloadLink("dl_csv", "Spreadsheet (CSV)", class = "dropdown-item")),
    tags$li(downloadLink("dl_mp3", "Recordings (MP3, zipped, incl. Exp 2 versions)", class = "dropdown-item"))
  )
)

ui <- page_fillable(
  title = app_title,
  theme = bs_theme(version = 5),
  padding = 0, gap = 0,
  tags$head(
    tags$style(HTML(app_css)),
    # Switches a popup's playback bar to the version whose button was clicked
    tags$script(HTML("
      function geoSwitchVersion(btn) {
        var box = btn.closest('.song-popup');
        var audio = box.querySelector('audio');
        box.querySelectorAll('.ver-btn').forEach(function(b) {
          b.classList.remove('active'); b.setAttribute('aria-pressed', 'false');
        });
        btn.classList.add('active'); btn.setAttribute('aria-pressed', 'true');
        audio.src = btn.getAttribute('data-src');
        var p = audio.play();
        if (p && p.catch) p.catch(function() {});
      }
    "))
  ),

  div(
    class = "map-wrap",
    leafletOutput("map", height = "100%"),

    absolutePanel(class = "map-title", h1(app_title)),

    absolutePanel(
      class = "map-tools",
      selectInput("jump_to", NULL, width = "260px",
                  choices = c("Go to a song..." = "",
                              setNames(songs$id, songs$menu)[order(songs$menu)])),
      actionButton("reset_view", "Reset view", class = "btn-light btn-sm"),
      export_menu
    )
  )
)

# -----------------------------------------------------------------------------
# 8. Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  world_view <- function(map) {
    fitBounds(map, min(songs$lng), min(songs$lat), max(songs$lng), max(songs$lat))
  }

  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 5, zoomControl = FALSE)) |>
      # Plain land (always on) with region colours as a layer on top
      addPolygons(data = countries, fillColor = land_colour, fillOpacity = 1,
                  color = border_colour, weight = 0.9,
                  options = pathOptions(interactive = FALSE)) |>
      addPolygons(data = countries, fillColor = ~region_fill, fillOpacity = 1,
                  color = border_colour, weight = 0.9, group = "Region colors",
                  options = pathOptions(interactive = FALSE))

    if (!is.null(navajo)) {
      m <- m |>
        addPolygons(data = navajo, fillColor = land_colour, fillOpacity = 1,
                    color = border_colour, weight = 0.9,
                    options = pathOptions(interactive = FALSE)) |>
        addPolygons(data = navajo, fillColor = region_colours[["Northern America"]],
                    fillOpacity = 1, color = border_colour, weight = 0.9,
                    group = "Region colors", options = pathOptions(interactive = FALSE))
    }

    # One layer per experiment group so each can be switched on and off
    for (g in exp_groups) {
      df <- songs[songs$exp_group == g, ]
      if (nrow(df) == 0) next
      m <- m |>
        addMarkers(data = df, lng = ~lng, lat = ~lat, layerId = ~id, group = g,
                   icon = marker_icons(df), label = ~hover, popup = ~popup,
                   popupOptions = popupOptions(maxWidth = 350))
    }

    m |>
      addLayersControl(overlayGroups = c(exp_groups, "Region colors"),
                       position = "bottomright",
                       options = layersControlOptions(collapsed = FALSE)) |>
      addControl(HTML(legend_html), position = "bottomright") |>
      world_view() |>
      htmlwidgets::onRender(map_js)
  })

  # Jump to a song chosen from the dropdown and open its popup
  observeEvent(input$jump_to, {
    req(input$jump_to != "")
    row <- songs[songs$id == input$jump_to, ][1, ]
    leafletProxy("map") |>
      setView(row$lng, row$lat, zoom = 4) |>
      clearPopups() |>
      addPopups(row$lng, row$lat, row$popup, options = popupOptions(maxWidth = 350))
  })

  observeEvent(input$reset_view, {
    updateSelectInput(session, "jump_to", selected = "")
    leafletProxy("map") |> clearPopups() |> world_view()
  })

  # --- Exports ----------------------------------------------------------------
  # Layers currently switched on (all of them until the map reports otherwise)
  visible_groups <- reactive({
    if (is.null(input$map_groups)) c(exp_groups, "Region colors") else input$map_groups
  })

  output$dl_svg <- downloadHandler(
    filename = "GeoMusic_stimulus_map.svg",
    content = function(file) {
      ggplot2::ggsave(file, make_figure(visible_groups()), device = "svg",
                      width = 12, height = 7.5)
    }
  )

  output$dl_png <- downloadHandler(
    filename = "GeoMusic_stimulus_map.png",
    content = function(file) {
      ggplot2::ggsave(file, make_figure(visible_groups()), device = "png",
                      width = 12, height = 7.5, dpi = 300, bg = "white")
    }
  )

  # The fields shown in the app, one row per song
  output$dl_csv <- downloadHandler(
    filename = "GeoMusic_stimuli.csv",
    content = function(file) {
      out <- data.frame(
        Region            = songs$region19,
        Country           = songs$country,
        Artist            = songs$artist,
        Track             = songs$track,
        `Year Released`   = songs$year,
        Excerpt           = songs$timestamp,
        Genres            = songs$genre,
        `Lyrics Language` = songs$language,
        Experiments       = songs$exp_group,
        `YouTube URL`     = songs$youtube_url,
        `Spotify URL`     = songs$spotify_url,
        check.names = FALSE
      )
      out <- out[order(out$Region, out$Country), ]
      con <- file(file, open = "w", encoding = "UTF-8")
      writeChar("\ufeff", con, eos = NULL)   # lets Excel read accents and non-Latin scripts
      write.csv(out, con, row.names = FALSE, na = "")
      close(con)
    }
  )

  output$dl_mp3 <- downloadHandler(
    filename = function() {
      if (nzchar(Sys.which("zip"))) "GeoMusic_recordings.zip" else "GeoMusic_recordings.tar.gz"
    },
    content = function(file) {
      audio <- file.path("www/audio", songs$audio_file[!blank(songs$audio_file)])
      audio <- c(audio, file.path(exp2_dir, unlist(song_versions, use.names = FALSE)))
      audio <- audio[file.exists(audio)]
      if (nzchar(Sys.which("zip"))) {
        utils::zip(file, audio, flags = "-j -q")
      } else {
        old <- setwd("www/audio"); on.exit(setwd(old))
        utils::tar(file, sub("^www/audio/", "", audio), compression = "gzip")
      }
    }
  )
}

shinyApp(ui, server)
