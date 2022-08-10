<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# overedge 0.0.0.9001 (2022-06-07)

- feat: add col parameter to get_location_data
- refactor: simplify flow since st_bbox_ext now allows NULL x values
- refactor: split read_sf_query off from read_sf_path
- refactor: improve error handling for read_sf_pkg and read_sf_ext
- fix: correct handling of filenames w/ filetypes for write_sf_ext
- refactor: rename str_pad_digit to str_pad_digits and str_extract_digit to str_extract_digits
- refactor: create new str_fix function and split off str_prefix function
- refactor: add str_extract_filetype helper function
- refactor: rename x arg for str_remove_filetype to string
- feat: add width parameter to make_filename
- feat: add digit padding support to make_filename using new str_pad_digit utility function
- docs: fill in missing parameter names
- fix: correct issue w/ rename_sf_col
- refactor: add set_join_by_geom_type utility function for bind_boundary_col and write_exif_keywords
- feat: Add join parameter to bind_boundary_col
- docs: update function docs
- feat: split str_trim_squish from format_data
- feat: add sf_col support to format_data
- fix: correct issue w/ is_state_name + is_county_name
- refactor: simplify get_admin_data functions
- refactor: change format_data parameter replace_na to replace_na_with
- feat: create rename_with_xwalk based on code shared to Twitter by Shannon Pileggi
- fix: correct bug in as_sf w/ bbox objects
- fix: correct typo in write_sf_cache
- fix: correct issue w/ address_to_sf
- refactor: rename df_to_sf helper functions for consistency
- feat: add address parameter to df_to_sf
- feat: add show_col_types param to read_sf_csv
- feat: update address_to_sf to work w/ data frames
- feat: add geo parameter to allow address data in csv, Excel, and Google Sheet data
- fix: correct issue w/ misnamed parameter (path instead of file) for readr::read_csv
- refactor: drop url + path params from get_location_data
- feat: allow as_sf to work w/ character vectors (e.g. state names, addresses)
- feat: add support for character locations to get_location_data
- refactor: add crs parameter to internal df_wkt_to_sf function
- feat: add internal address_to_sf function
- feat: make get_states and get_counties accept GeoID, name, and abb values to location parameter
- docs: update docs/pkgdown site
- refactor: move rappdirs from Imports to Suggests and add mapbaltimore to Suggests
- refactor: remove drop parameter from sort_features
- docs: update missing parameter definitions
- refactor: add url parameter to read_sf_csv
- feat: add warning if read_sf_gist has more than one file
- feat: add read_sf_geojson function
- feat: add sheet parameter to read_sf_gsheet
- refactor: revise parameters for read_sf_gist and read_sf_gsheet
- feat: add read_sf_gist function
- docs: reorganize function order for read_sf_ext and write_sf_ext
- feat: add internal is_gist_url function
- docs: update pkgdown site
- refactor: update NAMESPACE
- refactor: add gistr to Suggests
- fix: issue w/ layer_numbers and layer_markers
- refactor: move early return for geom_fn parameter
- fix: correct handling of geom parameter for layer_numbers + layer_markers when called by layer_location_data
- refactor: remove is_ggrepel_geom indicator variable
- refactor: add get_path_filetype helper function + simplify logic for renaming variables
- feat: add support for multiple paths to write_exif
- refactor: add is_state_name and is_county_name helper functions for is_sf_or_what
- refactor: update dist functions to work w/ area
- feat: add is_diff_area function
- feat: update dist_unit_options and add area_unit_options
- refactor: remove area_units object
- docs: fill missing parameter definitions for make_location_grid, get_data_dir, and as_points
- docs: update pkgdown site
- fix: correctly export dist functions
- refactor: move convert_dist_scale and is_dist_units into separate scripts
- docs: add missing documentation to is_dist_units
- docs: add dist function family tag
- feat: rebuild make_location_grid to support multiple styles
- test: update make_location_grid style
- fix: stop importing mapboxapi and knitr (packages in Suggests)
- fix: add missing features parameter to get_osm_data_features
- fix: correct sf_bbox_diagdist calls w/ old units = TRUE parameter (replaced w/ drop = FALSE)
- style: fix spacing
- feat: add .id parameter for st_dist + number_features
- feat: add alert to sort_features if sort and to parameters are both provided
- feat: add has_same_name_col utility
- refactor: move relocate_sf_col
- refactor: add drop param to sf_bbox_dist functions  and allow units to be used to define new units for conversion
- feat: add sf_bbox_point function
- refactor: adopt sf::st_is for is_geom_type
- fix: correct typo making is_multistring broken
- refactor: rename is_string to is_line
- feat: add is_sfg function
- refactor: add len to is_sf_or_what and allow numeric values to be checked against GeoIDs
- feat: add as_point and as_points functions
- refactor: rebuild as_sf functions to follow early return, case_when, switch convention
- feat: add has_coords function
- fix: correct handling of upper/lower case df col name variation w/ check_coords
- feat: add create_data_dir function
- fix: correct issue w/ NULL path values for make_filename
- feat: add is_sf_or_what
- refactor: rename is_named param for is_sf_list to named
- feat: add ggsave_social function
- fix: correct issue w/ NULL filename and filetype values
- docs: update pkgdown site
- refactor: add scales, mapboxapi, and paletteer to Suggests
- feat: add bbox parameter to read_sf_gsheet
- refactor: clean up bbox_filter and sf_filter utility functions
- refactor: split up utils.R
- feat: add modify_fn_fmls function
- feat: join_sf_gsheet function
- refactor: add ls_pkg helpers
- refactor: relocate theme_method helper to theme_ext.R
- feat: add layer_markers and layer_numbers
- refactor: move tidygeocoder to Suggests
- feat: add num_style param to number_features
- refactor: separate sort_features
- feat: add read_sf_gsheet and write_sf_gsheet functions
- refactor: move tidygeocoder + googlesheets4 to Suggests
- refactor: switch from ui functions to cli
- fix: correct issue w/ nonexported gsheet_to_sf
- feat: add coords option to check_coords
- fix: correct issues w/ location_filter
- refactor: create sf_filter and bbox_filter helper functions
- refactor: coords_crs to df_crs
- feat: update df_to_sf to support a dataframe with a wkt column
- refactor: rename multiple utility functions
- feat: add ui_ask utility function
- feat: add gsheet_to_sf utility function
- feat: add support for unzip parameter (untested)
- docs: update pkgdown website
- feat: add layer_frame and make_frame
- refactor: rename geom_sf_icon to layer_icon
- feat: add diff parameter to is_same_dist
- docs: update pkgdown site
- feat: add st_square and st_circle
- refactor: rename st_geom_type to is_geom_type and add variants
- docs: move more examples into example folder
- docs: update read_sf_ext documentation
- refactor: rename eval_data to use_eval_parse
- refactor: create new use_fn utility function
- feat: expand flexibility of get_admin_data (add ... params and class param)
- refactor: rename valid_dist_units to dist_units
- feat: add location_filter function
- feat: add coords_crs and remove_coords parameters to df_to_sf
- docs: update pkgdown site
- feat: add name_col support to read_sf_path
- fix: continuing to refine list checks
- feat: add number_features and make_markers
- refactor: reorganize examples
- refactor: rename locationname_col to name_col and locationname to name
- docs: update vignettes and examples
- fix: extensive updates on how basic functions handle checks for sf lists
- fix: correct regression in st_bbox_ext
- feat: update get_paper to support custom paper sizes
- test: add tests for is_sf and is_gg functions (remove blank test for st_bearing)
- fix: multiple issues w/ get_location and get_margin
- fix: correct issue w/ is_gg_sf_layer
- feat: add mapview_col function (first of planned mapview_ext)
- refactor: Add mapview as a suggested package
- feat: add make_location_grid function
- feat: add support for geometry column name to sf_bbox_to_sf and as_sf
- refactor: Add draft (not exported) make_location_layers function
- refactor: Create examples folder (not incorporated into docs yet)
- feat: add draft read_sf_any function
- refactor: get layer_location_data to work w/ rlang exec
- refactor: name pkg parameter for check_pkg_installed
- feat: extensive additions to support sf lists
- feat: add map_location_data function
- refactor: add as_sf_class utility function
- refactor: update st_bbox_ext, st_buffer_ext and other functions to work w/ sf lists
- refactor: update is_sf_list to work w/ bbox and sfc lists
- feat: add class parameter to get_location
- docs: update pkgdown site
- feat: add make_location_data_list and make_sf_list
- feat: add is_gg utility check functions
- feat: add bbox parameter to get_paper and update get_paper to allow passing custom data frames
- fix: correct issue w/ misleading portrait/landscape orientation for get_paper results (still need to correct data frame)
- feat: add plot_background to layer_neatline and suppress warning by setting default = TRUE for ggplot2::coord_sf
- feat: add nonexported group_by_col utility function
- docs: update pkgdown site
- fix: get as_bbox to work w/ MULTIPOINT geometry (even if <4 points)
- style: minor formatting updates
- feat: add title param to theme_legend and implement better control over justification for legends that are not inset
- refactor: remove warning from st_geom_type
- feat: add gutter and margin params to get_paper
- test: expand tests for get_paper to include margin and cols params
- fix: finish rename to_unit to to and from_unit to from
- refactor: change modify_label_mapping to modify_mapping
- fix: correct old function name in write_sf_cache
- feat: add simplify parameter to st_cast_ext
- feat: Add gtsave_ext function
- docs: minor updates to function titles
- refactor: move multiple packages to Suggests from Imports
- refactor: use units::drop_units function instead of as.numeric
- feat: add modify_label_mapping function
- docs: reorganize reference for pkgdown site + revise function titles
- feat: add st_cast_ext function
- fix: correctly apply sf::st_zm before sf::st_point_on_surface function for st_coords
- fix: correct issue w/ read_sf_pkg
- refactor: rename read_sf_package to read_sf_pkg
- fix: correct issue w/ st_coords
- feat: add scale parameter to st_inscribed square
- fix: correct issues w/ st_coords trying to convert POINT data
- refactor: rename check_package_exists to check_pkg_installed (and switch to using rlang for check/install)
- refactor: rename get_standard_scale to get_scale
- style: move get_data_dir and make_filename into separate R file
- docs: reorganize pkgdown reference page and fill in missing parameter names
- feat: add get_states and get_counties functions
- refactor: add return parameter to st_transform_ext
- refactor: switch state/county data from 20m to 5m
- feat: add us_states and us_counties data
- feat: export get_data_dir function
- feat: add read_sf_download function
- feat: add sf_bbox_to_sfc function
- refactor: draft sf_bbox_to_npc function
- refactor: add cache parameter to make_filename function
- feat: add st_circumscribed_circle
- refactor: update read_sf_exif to pull in more data
- fix: update ggsave_exif to better handle missing filetypes
- feat: add keywords parameter to ggsave_exif
- feat: add hjust and vjust parameters to theme_text
- refactor: swap exifr for exiftoolr in package Imports
- fix: avoid get_location error if location is NULL
- feat: add support for label parameter even if location is NULL
- feat: add vignette on working w/ sf and bbox objects
- refactor: add packages to Remotes in DESCRIPTION
- feat: export all check functions (oops)
- docs: add new vignette and rebuild pkgdown site
- feat: export as_sf + as_bbox functions
- refactor: modify crs parameter for as_bbox and add to as_sf
- test: drop tests for get_osm_data + get_us_boundaries (temporarily)
- test: fix test to df_to_sf (drop use of ggspatial) and set_bearing (drop call to get_osm_data)
- refactor: switch esri2sf and ggsvg to Suggested to support R version <= 4
- refactor: add gt and birdseyeview to Suggested
- feat: drop get_us_counties (temporarily)
- docs: correct pkg links and add missing param defintions
- refactor: change check_coords to use x param instead of data
- fix: update check_coords to use setequal when comparing coordinates
- docs: update pkgdown site
- refactor: update st_clip to separate make_clip and make_pts utility functions and add st_erase
- refactor: rename check_to_sf -> as_sf and check_to_bbox -> as_bbox
- refactor: improve layer_location_data handling of aes mapping
- feat: add sf parameter to sf_bbox_asp
- refactor: add st_erase to layer_mask
- refactor: revert name of bind parameter to keep_all in st_coords
- feat: add st_coords function based on sf_to_df function (which now calls st_coords)
- docs: Add osm_building_tags to pkgdown site
- feat: Add ... params to check_to_sf + check_to_bbox
- fix: correct how check_to_sf converted raster data
- feat: Add check_coords function
- refactor: incorporate check_coords function into df_to_sf and read_sf_url (for Google Sheets)
- feat: Add rev parameter to df_to_sf to make auto-reversing coordinates to lon/lat order optional
- feat: Add support to more classes to st_bbox_ext (including sfc, raster, and sp objects)
- refactor: add by_geometry parameter to st_geom_type + use st_geom_type in st_bbox_asp
- feat: add st_geom_type function
- feat: add check_sp, check_raster, and check_sfc
- fix: get_flickr_photos correct handling of sort param and make sure image width/height column names match results from read_sf_exif
- feat: Add gsheet support to read_sf_url
- fix: update write_sf_ext to support NULL path
- refactor: Add missing imports from ggplot2
- feat: add draft for read_sf_exif
- refactor: migrate layer_show_location to new {birdseyeview} package
- feat: add st_clip (modified version of {mapbaltimore} clip_area function)
- fix: partial fix for issue with get_osm_data
- fix: additional fix for support for position = "none" on theme_legend
- docs: update pkgdown site and minor change for ggsave_ext vignette
- docs: reorganize reference page
- fix: update theme_legend to support position = "none"
- refactor: minor improvement to st_center
- feat: Add `get_osm_boundaries` function and implement enclosing parameter for get_osm_data
- style: auto-style white space on several files
- fix: correct make_basemap use of nonexistent layer_sf_data function (replaced by layer_location_data)
- feat: add convert_dist_scale + convert_dist_units functions
- refactor: remove convert = TRUE parameter from get_standard_scale
- refactor: update layer_scaled to work w/ convert_dist_scale instead of get_standard_scale
- feat: Add cm + km to valid_dist_units
- refactor: Add dist_unit_options as system data
- feat: Add `get_wiki_data` function
- docs: Update title for st_transform_ext to show it supports bounding boxes
- refactor: incorporate sf_bbox_shift into st_bbox_asp
- feat: add sf parameter to st_bbox_ext
- feat: add units parameter to sf_bbox dist functions
- feat: add sf_bbox_shift, sf_bbox_contract, and sf_bbox_expand functions
- refactor: consolidate st_scale_rotate  and st_inscribed_square to a single file st_misc w/ st_center
- refactor: Rename get_socrata_data to get_open_data (Socrata parent firm is planning rebrand)
- feat: Add get_us_counties function based on USAboundariesData package
- docs: reorganize documentation to consolidate  st_bbox_ext, st_bbox_asp and relocate get_paper, get_margin, and get_asp
- feat: add standard map, architectural, and engineering scales
- feat: Add get_socrata_data
- feat: export check_sf functions
- docs: expand vignettes
- refactor: rename st_bbox_adj > st_bbox_ext
- feat: export labs_ext
- feat: Add `get_flickr_photos` function
- docs: add get_remote_data draft vignette and update pkgdown site
- refactor: rename `st_bbox_adj` to `st_bbox_ext` for consistency with other modified versions of sf functions
-refactor: import rlang .data function to reduce undefined global functions or variables warnings
- refactor: move required packages back into Imports from Suggests
- test: fix test for st_bearing
- feat: Add support for different types of margin to `get_asp` (allowing numeric margin values to be passed to distance or accepting margin class objects as is)
- test: Add test for `get_asp`
- feat: Add package checks for geomtextpath and ggrepel (necessary to keep packages as suggests instead of imports)
- feat: Add partial support for px units to get_margin
- feat: Add support for bbox location parameter to get_location
- fix: update how layer_mask handles neatline checks
- feat: add support for ggrepel and geomtextpath geoms to layer_location_data
- refactor: add check_geom utility function
- feat: Add layer_show_location function
- refactor: move less critical packages from Imports to Suggests
- docs: update pkgdown site
- refactor: minor reorganization for layer_neatline
- feat: Update get_location_data to better handle mismatched crs
- refactor: Add utility functions check_sf_same_crs, check_class (generic), check_bbox, and update check_sf w/ ext parameter
- docs: Add titles and parameter definitions to utility functions
- test: Add test for `st_transform_ext`
- feat: add bgcolor param to `layer_neatline`
- docs: update pkgdown site
- feat: Add postcard, print, and social image sizes to `paper_sizes`
- refactor: update `get_paper` to ensure compatibility w/ updated image sizes
- test: Add test for `get_paper`
- refactor: create write_sf_types helper function to create feature parity for write_sf_ext and write_sf_cache to optionally save both CSV and RDS files.
- docs: clean-up documentation for read_sf_ext functions
- docs: Update pkgdown site
- refactor: rename cache_sf to write_sf_cache (and combine docs w/ write_sf_ext)
- feat: drop support for lists of sf objects (at least temporarily)
- feat: Add support for creating missing directories to make_filename
- docs: Update pkgdown site
- remove parameter check from get_location that was causing issues
- remove latlon parameter from get_esri_data
- fix test for get_location
- multiple functions update parameter definitions
- `get_location`: modify tidygeocoder:geo params to support multiple addresses and improve checks.
- `ggsave_ext`: update exifr code to handle dates better for PNG and other filetypes and integrate make_filename  function
- Add check_bbox, check_sf_list, and theme_method to non-exported utility functions
- Add `cache_sf` function
- refactor: update geom_sf_icon and st_bearing to use sf_to_df
- refactor: drop latlon parameter from sf_bbox_to_lonlat_query
- feat: st_bearing updates to handle multiple lines
- feat: Add sf_bbox_dist function
- test: Add test for sf_to_df
- refactor: rename check_sf_list to check_sf
- docs: correct minor documentation issues in several functions
- feat: Add theme_legend function
- feat: add `valid_dist_units` for internal use w/ st_buffer_ext and possible other uses
- fix: error for get_osm_data when selecting geometry type
- fix: issue with list of building tags (added new sysdata for osm_building_tags)
- fix: make get_asp use orientation if block_asp = TRUE
- fix: remove extra code at end of layer_location_data
- docs: update pkgdown site w/ draft ggsave_exif vignette
- feat: add theme_margin function
- feat: add color parameter to theme_text function
- fix: correct defaults for layer_location_data
- refactor: add new utility functions sf_bbox_asp and move sf_bbox_xdist and sf_bbox_ydist
- fix: small issues w/ ggsave_exif
- fix: issue w/ axis.ticks in layer_neatline
- docs: update pkgdown site reference
- feat: Add support for "nautical mile" unit to `st_buffer_ext()`
- docs: update pkgdown site w/ updated param definition for unit
- test: Add tests for geom_sf_icon, layer_mask, and st_bearing
- test: Fix test for df_to_sf and get_location
- refactor: Add support for unit parameter to get_location_data(), layer_location_data(), and other functions
- docs: Additional minor updates to parameter definitions + update pkgdown site
- refactor: rename write_sf.R to write_sf_ext.R
- refactor: add `check_sf_list()` utility function
- feat: Add support for paper to `ggsave_ext()`
- fix: `get_location_data()` not loading data from global environment as expected
- feat: Add size column to `paper_sizes`
- feat: Add `write_sf_ext()` function (cache currently not supported)
- refactor: Update `read_sf_package()` to use `check_package_exists()`
- feat: Add `ggsave_ext()` function
- refactor: Add `check_package_exists()` utility function
- feat: add support for adjusting bbox objects to `st_bbox_asp()`
- feat: add ... parameters to `st_buffer_ext()` to pass to `sf::st_buffer()`
- feat: Export `layer_location_data()`
- docs: Add `layer_location_data()` to docs and pkgdown site
- feat: Add neatline parameter to layer_location_data function
- refactor: update layer_mask to work w/ layer_location_data
- feat: Add get_location_data and layer_location_data functions
- feat: Add `layer_mask()`
- test: Add test for `get_osm_data()`
- feat: Add `read_sf_path()` (for file paths), `read_sf_url()` (for URLs including ArcGIS FeatureServers and MapServers), and `read_sf_package()` for reading cached package data
- feat: Add `get_esri_data()` wrapping esri2sf function calls
- feat: Add `get_osm_data()` wrapping osmdata function call
- Add new utility functions: `check_url()`, `check_esri_url()`, and `eval_data_label()`
- Move `sf_bbox_to_sf()` to sf_bbox.R and add new functions `sf_bbox_to_sf()` and `sf_bbox_to_lonlat_query())`
- feat: Add `theme_text()` function for updating ggplot2 themes
- fix: Update `layer_neatline()` to remove axis
- docs: Update README w/ `layer_neatline()` example
- docs: Update pkgdown site
- feat: Add `iconname_col` parameter to support use of any data column as source of icon names and add support for fixed px and source values in combination with iconname_col
- refactor: Reorder conditional to use `icon` parameter if provided and ignore `svg` unless `icon` is NULL
- docs: Update `geom_sf_icon()` documentation w/ info re: new parameter
- refactor: Move utility functions to utils.R
- refactor: Rename `check_asp()` to `get_asp()`
- feat: Add OSM icons to map_icons
- refactor: Blank NA values in map_icons style column
- docs: Clean up documentation for new functions
- docs: Update pkgdown site
- refactor: Minor changes to `st_diag_dist()` utility function
- feat: Add `st_bbox_adj()`, st_buffer_ext()`, and `st_bbox_asp()` functions adapted from `adjust_bbox()`, `buffer_area()`, and `adjust_bbox_asp()` functions in {mapbaltimore} package
- feat: Add `layer_neatline()` function adapted from `set_map_limits()` functions in {mapbaltimore} package
- feat: Add `df_to_sf()` function for converting lat/lon dataframes into sf objects
- refactor: Rename `get_point_matrix()` utility function used by `st_bearing()` to `get_coord_df()`
- test: Use {testhat} to create tests for `st_bbox_adj()`
- test: Use {covr} to check coverage
- fix: Add geosphere, lwgeom, and purrr to Imports
- feat: Add `st_bearing` to append a bearing column to sf objects w/ LINESTRING geometry
- feat: Add `st_inscribed_square` that wraps `sf::st_inscribed_circle()` to make a close approximation of an inscribed square
- feat: Add `st_scale_rotate` function
- feat: Update `geom_sf_icon` to support custom svg path/url
- docs: Clarify definition of `source` parameter for `geom_sf_icon`
- docs: Add a pkgdown website and update the README
- fix: Update DESCRIPTION with R version dependency and LazyData
- feat: Add a `geom_sf_icon` function that wraps `ggsvg::geom_point_svg()` function with support for sf objects with an icon column or set icon name
- docs: Add README
- feat: Add map_icons object with icon index

