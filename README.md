<img src="inst/PuffR.png" width="75%">

PuffR is all about helping you conduct dispersion modelling using the [CALPUFF modelling system](http://www.epa.gov/scram001/dispersion_prefrec.htm).

### Air Quality Modelling and CALPUFF

Air quality modelling is a great tool for describing the causal relationship between emissions, meteorology, atmospheric concentrations, deposition, and other factors. Air pollution measurements give useful quantitative information about ambient concentrations and deposition, however, such measurements can only describe air quality at specific locations and times. Moreover, monitoring usually doesn't provide very good information concerning the causes of the air quality problem. AQ modelling can instead provide a more complete deterministic description of the air quality problem, including an analysis of factors and causes (e.g., emission sources, meteorological processes, physical changes, and chemistry). Thus air quality models play an important role in science, because of their capability to assess the relative importance of the relevant processes. Air quality modelling is also an important tool for developing and evaluating air quality policy. Model outputs provide a wide assessment of the state of air quality across a given jurisdiction both in terms of airborne concentrations and potential human exposure and the deposition of acidifying and eutrophying pollutants. 

The CALPUFF integrated modelling system consists of three main components and a set of preprocessing and postprocessing programs. The main components of the modelling system are CALMET (a diagnostic 3-dimensional meteorological model), CALPUFF (an air quality dispersion model), and CALPOST (a postprocessing package). It's a great system.
 
### The Goals of the PuffR R Package Project

While CALPUFF is indeed great, the workflow for atmospheric dispersion modelling with CALPUFF needs to be reconsidered, both in the interest of saving time and also for ensuring that the quality of inputs is higher. Here are some ways that the PuffR package might provide some value:

— allow the user to provide a minimal selection of parameters (relying on a very sensible selection of defaults); the PuffR package will then collect, analyze, and prepare model inputs with the best publicly available data

— include the ability to store presets (e.g., for model domains, receptors, emissions sources, etc.) that can be shared across projects

— have a useful help system and documentation library (with copious examples) available to aid in the understanding of every option/switch/setting in CALMET/CALPUFF/CALSUM/CALPOST

— include functions for a wide range of statistical analyses will for both the input and the output data

— put an emphasis on data visualization and data exploration; this will allow for greater understanding for both experts and laypersons

— allow visualizations and data to be easily shared on the web, and, be exported in a wide range of useful formats

### How to go about this

The project is starting small. Hell, it's got to start somewhere. But we've got a great foundation! First off, we are using R. It's got everything we need to gather and organize datasets, do spatial/temporal tasks, produce beautiful visualizations, and publish on the web. Secondly, we have the CALPUFF code base to perform the numerical modelling. Nothing really has to be rewritten there, there just needs to be an interface. Perhaps some compiling from source will be done but *that's it*.

Dispersion modelling can be a complex process and, as with all models, the results are only as useful as the model itself and how it is used. Furthermore, such models need good data. Well, we have have an embarrassment of riches when it comes to data. It's very easily accessible now and the relevant data products are of exceptional quality (often taking years of work from a large number of contributors). Here are some suitable candidates for datasets that can be incorporated into a PuffR workflow:

| Type of Data | Description | Provider |
|--------------|-------------|----------|
| Surface station meteorology | 1-hourly global dataset for global met stations | National Climatic Data Center (NCDC) |
| Upper air data | global datasets spanning decades | RAOBS global archive of radiosonde/rawinsonde upper air data |
| Surface elevation | U.S. National Elevation Data (NED) | U.S. Geological Survey (USGS) |
|  | Canadian Digital Elevation Data (CDED) | GeoBase.ca |
|  | global SRTM V4 GeoTIFF archive | U.S. Geological Survey (USGS) |
| Landuse and land cover | U.S. National Land Cover Data (NLCD) 2011 | U.S. Geological Survey (USGS) / Multi-Resolution Land Characteristics Consortium (MRLC) |
|  | GeoBase Land Cover Product (Canada) | GeoBase.ca |
|  | GlobCover 2009 (Global Land Cover Map) | European Space Agency (ESA) data user element (due) |
|  | MODIS gridded land cover data (global) | MODIS data |
| Industrial emissions sources | a database of point and area emissions sources | US EPA National Emissions Inventory |
|  |  | Canadian NPRI Emissions Database | 
| Marine emissions sources | AIS ship positions + ship master data | various freely available sources |

Aside from the aforementioned data, there are also additional datasets available on a regional basis that may prove valuable in many instances (e.g., regionally-managed met stations, road activity data, etc.). For this reason, you can bring your own datasets (BYOD) and add those to the model inputs.

### Installation

Install PuffR from GitHub using the `devtools` package:

```R
require(devtools)
install_github('PuffR', 'rich-iannone')
```

It's early days so the package will be changing rapidly. I invite you to send me questions and comments about this. If you'd like to contribute, let me know and we can talk collaboration. Yeah, let's work together!

### Package Progress

You can already do one or two useful things with this package. For instance, the geophysical input file for CALMET (GEO.DAT) can be produced.

Let's generate a geophysical input file for a portion of the Vancouver area. This will consist of a grid centered (use `lat_lon_grid_loc = 1` for a center reference) on 49.250117ºN and 123.076122ºW (`lat_dec_deg = 49.5` and `lon_dec_deg = -125.30364`). The width (E-W distance) of the grid will be 8000 m, and the height (N-S distance) will be the same (`domain_width_m = 8000` and `domain_height_m = 8000`. We will elect to download SRTM terrain height data from "http://gis-lab.info/data/srtm-tif/" (with `download_SRTM = TRUE`).

```R
calmet_define_geophys(lat_dec_deg = 49.250117,
                      lon_dec_deg = -123.076122,
                      lat_lon_grid_loc = 1,
                      domain_width_m = 8000,
                      domain_height_m = 8000,
                      download_SRTM = TRUE)
```

Alternatively, if you elected to download the whole 11 GB set of SRTM V4 GeoTIFF zip files, you can reference the local folder that contains that archive:

```R
calmet_define_geophys(location_name = "the_city"
                      lat_dec_deg = 49.250117,
                      lon_dec_deg = -123.076122,
                      lat_lon_grid_loc = 1,
                      domain_width_m = 8000,
                      domain_height_m = 8000,
                      download_SRTM = FALSE,
                      SRTM_file_path = "/Volumes/Big HD/SRTM V4 GeoTIFF/")
```

This function currently defaults to generating seasonal GEO.DAT files:

- `geo--the_city-32x32x250--1-winter.txt`
- `geo--the_city-32x32x250--2-spring.txt`
- `geo--the_city-32x32x250--3-summer.txt`
- `geo--the_city-32x32x250--4-fall.txt`
- `geo--the_city-32x32x250--5-winter.txt`

The naming of these files is handled by PuffR. Functions for setting up the CALMET input file will rely on consistent naming of the files for file handling and for parsing the metadata that is stored within. This scheme allows for data persistence and minimal repetition of basic parameters.

How about surface meteorology? We can produce a SURF.DAT file using the `calmet_surface_met` function. In the following example, we can obtain a SURF.DAT file from the same domain, specifying the beginning and ending years:

```R
calmet_surface_met(start_year = 2011,
                   end_year = 2011,
                   lat_dec_deg = 49.250117,
                   lon_dec_deg = -123.076122,
                   lat_lon_grid_loc = 1,
                   domain_width_m = 30000,
                   domain_height_m = 30000,
                   time_offset = -8,
                   output_file = "surf.dat")
```

This function currently requires that you supply a `time_offset` value, which is the time difference from UTC+0000. The output file can be named by supplying a filename for the `output_file' argument.

Creating functional CALMET and CALPUFF input files occur through a stepwise process. After creating the basic CALMET input data files (e.g., GEO.DAT, SURF.DAT, etc.), the next logical step forward is to initialize a template of the CALMET.INP file using the `calmet_inp_generate_template` function.

```R
calmet_inp_generate_template()
```

This creates an effectively empty CALMET input file in the working directory (called 'calmet_template.txt`). While this file is readable plaintext, it really shouldn't be modified by hand. Rather, a group of functions will serve to programmatically populate that input file with parameter values. In this way, validation of inputs can be performed at every step.

The CALMET input file can be built up using a series of functions that address each of the input file's main sections:

```R
calmet_01_temporal_params()

calmet_02_grid_levels()

calmet_03_output_opts()

calmet_04_met_data_opts()

calmet_05_wind_field_opts_params()

calmet_06_mixhgt_temp_precip_params()

calmet_07_station_params()

calmet_inp_finalize()
```

While each of the above functions has a long list of arguments, sensible defaults for each parameter value are included. Furthermore, some functions will take data from input files (e.g., GEO.DAT, SURF.DAT, etc.) residing in the working folder. This strategy avoids possible errors from attempting to supply the same basic information multiple times. Of course, you will want to (and often need to) use specific options within each of the CALMET input section and that's entirely possible. For instance, the following function call will provide beginning and ending dates/times that do not run for the full course of what is supplied in the SURF.DAT input file:

```R
calmet_01_temporal_params(read_data_from_surf_dat = FALSE,
                          ibyr = 2005,
                          ibmo = 1,
                          ibdy = 1,
                          ibhr = 0,
                          ibsec = 0,
                          ieyr = 2005,
                          iemo = 3,
                          iedy = 15,
                          iehr = 0,
                          iesec = 0,
                          abtz = "UTC-0800")
```

### In the Works

- greater selection of data sources for generating geophysical input files
- simple functions for producing an upper air input file
- method for specifying emissions sources and creating time-and-space-varying emissions for point, line, area, and volume sources
- method for computing building downwash for point sources near large structures
- output/visualization of concentrations at receptors