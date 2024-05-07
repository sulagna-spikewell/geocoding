<center><h1>PCGC Social Determinants of Health and Geocoding Manual</h1></center>
<center><h3>April 11, 2024</h3></center>
<!-- # geocoder <a href='https://degauss.org'><img src='https://github.com/degauss-org/degauss_hex_logo/raw/main/PNG/degauss_hex.png' align='right' height='138.5' /></a> -->

<!-- [![](https://img.shields.io/github/v/release/degauss-org/geocoder?color=469FC2&label=version&sort=semver)](https://github.com/degauss-org/geocoder/releases) -->
<!-- [![container build status](https://github.com/degauss-org/geocoder/workflows/build-deploy-release/badge.svg)](https://github.com/degauss-org/geocoder/actions/workflows/build-deploy-release.yaml) -->


<!-- ## Preparation -->

## Introduction
The PCGC geocoding docker container provides means of geocoding a given list of PCGC participant addresses and augmenting it with additional location-derived information (geomarkers) as well as determining the driving distance (in minutes) to a PCGC center.
After installation, the software runs on a local computer without requiring an internet connection, thus maintaining the security and privacy of the participant information. The underlying software is based on [Cole Brokamp's](https://colebrokamp.com/) [deGAUSS package](https://degauss.org).

## Requirements
* Operating System:
    + MacOS
    + Windows
* RAM: 8GB
* Disk Space: 20GB (docker container is 10GB)
* administrator privileges (initially only, to install the 'docker' software)

## Step 0: Install Docker

See the [Installing Docker](https://degauss.org/using_degauss.html#Installing_Docker) webpage.

> <font size="3.5"> **_Note about Docker Settings:_** </font> <br> <font size="2.75"> After installing Docker, but before running containers, go to **Docker Settings > Advanced** and change **memory** to greater than 4000 MB (or 4 GiB) <br> 
 <center> <img width=75% src="figs/docker_settings_memory.png"> </center> <br> 
If you are using a Windows computer, also set **CPUs** to 1. <br> 
<center> <img width=75% src="figs/docker_settings_cpu.png">
</center> Click **Apply** and wait for Docker to restart. </font>


## Step 1: Preparing Your Address File

The address file must be a CSV file with either a column titled `address` containing all address components or columns titled `lat` and `lon` with the participant's latitude and longitude, respectively. Other columns may be present - in particular a `participant ID` column and an `address_date` column are recommended. However, the software will ignore (but preserve) all additional columns besides `address`, `lat` and `lon`.

An example address CSV file might look like the following `address-sample-date-UTAH.csv` file from the docker container:

<center><img src="figs/example_data.png" width="75%"></center>


Example address CSV files are [my_address_file.csv](tests/my_address_file.csv), [address-sample.csv](tests/address-sample.csv) or [address-sample-date-UTAH.csv](tests/address-sample-date-UTAH.csv), all located in the tests folder of the docker container source.

**_Note_**: Please make sure to enclose the information in the `address` column in quotation marks (e.g., "") if it contains commas.



## Step 2: Running the PCGC container (the short version)

If `my_address_file.csv` is an address file in the current working directory with an address column named `address`, then the command to process it through the PCGC geocoding container is:

  - macOS:
  
    ```sh
    docker run --rm -v $PWD:/tmp ghcr.io/pcgcid/geocoder_pcgc:0.0.1 \
    -s PCGC_UTAH -i my_address-file.csv -o UTAH_output 
    ```
  
  - Windows (CMD):
  
    ```sh
    docker run --rm -v "%cd%":/tmp ghcr.io/pcgcid/geocoder_pcgc:0.0.1 ^
    -s PCGC_UTAH -i my_address-file.csv -o UTAH_output 
    ```

will produce 3 output files:

- `UTAH_output.csv`: This file has full output data, __including PII data__. Do NOT sent this to the ACC.
- `UTAH_output-deid.csv`: This file contains de-identified fields specified by the user as well as location-derived information. By default, the list of de-identified fields contain "id","address_date","matched_state","precision","geocode_result","fraction_assisted_income", "fraction_high_school_edu","median_income","fraction_no_health_ins","fraction_poverty","fraction_vacant_housing", "dep_index","drivetime_selected_center","nearest_center_pcgc","drivetime_pcgc","version". “id” and “address_date” are copied verbatim from the input address file; it is the user's responsibility to ensure they don’t contain PHI
- `UTAH_output-log.txt`: This file is an output log of the processing.

**_Note_**: The example above uses `-s PCGC_UTAH` for PCGC center in Utah. To change the care center, replace `-s PCGC_UTAH` with one of the site abbrevations below (e.g., `-s PCGC_YALE`):

| **Abbreviation** |  **Name** |
|--------------------|-------------------|
PCGC_YALE | `Yale`
PCGC_BOSTON | `Boston Childrens`
PCGC_MTSINAI | `Mt. Sinai`
PCGC_COLUMBIA | `Columbia`
PCGC_CHOP | `CHOP`
PCGC_UTAH | `Utah`
PCGC_CHLA | `Childrens of LA`

**_Note_**: On Windows computers you may need to give Docker explicit permissions to access the folder containing the address file (and possibly restart the Docker daemon after you have done so). Please check the Docker documentation for details.

**_Note_**: The first time this process is run, docker will download the latest container from the ACC, which takes a few minutes of time. Later runs will not require internet connections (unless the container is to be updated with the latest version).

**_Note_**: After processing, __please__ inspect the output files and fix obvious formatting problems with the address file should they arise (see also the section below on input address data formatting). The `*-deid.csv` file is safe to be sent to the ACC via secure upload to AWS (similar to the EMR data uploads).

### Running the PCGC deGAUSS container (the longer version)

Command line parameters to show help, version and site list are as follows:

- `-h` or `--help`: Show available parameters. For example, users can use this command:

  ```sh
  docker run ghcr.io/pcgcid/geocoder_pcgc:0.0.1 -h
  ```
or 
  ```sh
  docker run ghcr.io/pcgcid/geocoder_pcgc:0.0.1 --help
  ```

- `-v` or `--version`: Show the current version of Docker container with this command:
  ```sh
  docker run ghcr.io/pcgcid/geocoder_pcgc:0.0.1 -v
  ```
  or 
  ```sh
  docker run ghcr.io/pcgcid/geocoder_pcgc:0.0.1 --version
  ```

- `--site-list`: Print all available sites with this command:
  ```sh
  docker run ghcr.io/pcgcid/geocoder_pcgc:0.0.1 --site-list
  ```

This container __requires__ both of the following arguments:

- `-i` to specify the path to the input address CSV file
- `-s` or `--site` to specify theabbreviation of the PCGC center of interest

| **Abbreviation** |  **Name** |
|--------------------|-------------------|
PCGC_YALE | `Yale`
PCGC_BOSTON | `Boston Childrens`
PCGC_MTSINAI | `Mt. Sinai`
PCGC_COLUMBIA | `Columbia`
PCGC_CHOP | `CHOP`
PCGC_UTAH | `Utah`
PCGC_CHLA | `Childrens of LA`


This container takes the following optional arguments:

-   `-o` or `--output-file-prefix` to specify prefix of output files. By default, the prefix is `output`, which will generate output.log, output-phi.csv, output-deid.csv
-   `--f` or `--include-deid-fields` to specify list of fields to include in output. Default fields:
    - id, address_date, precision, geocode_result, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, dep_index, drivetime_selected_center, nearest_center_pcgc, drivetime_pcgc, version
-   `--force` to force the container to overwrite output files if one of the output files already exists. By default, the program would exit if one of the output files already exists

### Running the PCGC  container (additional details)
This Docker image does the following:

1. perform geocoding on addresses (if not geocoded already, i.e., if `lat` and `lon` are not specified in the input), adding the following columns:

  - **`matched_street`**, **`matched_city`**, **`matched_state`**, **`matched_zip`**: matched address componets (e.g., `matched_street` is the street the geocoder matched with the input address); can be used to investigate input address misspellings, typos, etc.
  - **`precision`**: The method/precision of the geocode. The value will be one of:
    - `range`: interpolated based on address ranges from street segments
    - `street`:  center of the matched street
    - `intersection`: intersection of two streets
    - `zip`: centroid of the matched zip code
    - `city`: centroid of the matched city
  - **`score`**: The percentage of text match between the given address and the geocoded result, expressed as a number between 0 and 1. A higher score indicates a closer match. Note that each score is relative within a precision method (i.e. a `score` of `0.8` with a `precision` of `range `is not the same as a `score` of `0.8` with a `precision` of `street`). 
  - **`lat`** and **`lon`**: geocoded coordinates for matched address
  - **`geocode_result`**: A character string summarizing the geocoding result. The value will be one of
    + `geocoded`: the address was geocoded with a `precision` of either `range` or `street` and a `score` of `0.5` or greater.
    + `imprecise_geocode`: the address was geocoded, but results were suppressed because the `precision` was `intersection`, `zip`, or `city` and/or the `score` was less than `0.5`.
    + `po_box`: the address was not geocoded because it is a PO Box
    + `non_address_text`: the address was not geocoded because it was blank or listed as "foreign", "verify", or "unknown" 
    
    
- then join with tract-level deprivation index data derived from the 2018 American Community Survey (ACS), adding the following columns:

  - **`fips_tract_id`**: 2010 census tract identifier
  
  - 2018 American Community Survey variables:
    
    + **`fraction_assisted_income`**: fraction of households receiving public assistance income or food stamps or SNAP in the past 12 months
    + **`fraction_high_school_edu`**: fraction of population 25 and older with educational attainment of at least high school graduation (includes GED equivalency)
    + **`median_income`**: median household income in the past 12 months in 2018 inflation-adjusted dollars
    + **`fraction_no_health_ins`**: fraction of population with no health insurance coverage
    + **`fraction_poverty`**: fraction of population with income in past 12 months below poverty level
    + **`fraction_vacant_housing`**: fraction of houses that are vacant
  
  - **`dep_index`**: composite measure of the 6 variables above
  

- then compute drive time to a Pediatric Cardiac Genomics Consortium (PCGC) specified by user, adding the following columns:
  - **`drivetime_selected_center`**: computed estimated drive time to center specified by user
  - **`nearest_center_pcgc`**: Nearest PCGC center as computed by the Docker image 
  - **`distance_pcgc`**: Distance to the nearest PCGC center as computed by the Docker image 

# Details on the processing steps contained in the software

#### 1. Geocoding
##### Input address data formatting

- Other columns may be present, but it is recommended to only include `address`, an optional identifier column (e.g., `id`) and an optional `address_date` column.
- Address data must be in one column called `address`. 
- Separate the different address components with a space
- Do not include apartment numbers or "second address line" (but its okay if you can't remove them)
- ZIP codes must be five digits (i.e. `32709`) and not "plus four" (i.e. `32709-0000`)
- Do not try to geocode addresses without a valid 5 digit zip code; this is used by the geocoder to complete its initial searches and, if missing, will likely return incorrect matches
- Spelling should be as accurate as possible, but the program does complete "fuzzy matching" so an exact match is not striclty necessary
- Capitalization does not affect results
- Abbreviations may be used (i.e. `St.` instead of `Street` or `OH` instead of `Ohio`)
- Use Arabic numerals instead of written numbers (i.e. `13` instead of `thirteen`)
- Address strings with out of order items could return NA (i.e. `3333 Burnet Ave Cincinnati 45229 OH`)
- Geomarker data used was prepared following the instructions [here](https://degauss.org/manual_install.html) using the 2021 TIGER/Line Street Range Address files from the Census

#### 2. Deprivation index
This container overlays the input latitude and longitude coordinates with 2010 census tracts, then joins with tract-level deprivation index data derived from the 2018 American Community Survey (ACS).

For more information on the deprivation index, please see the [deprivation index page](https://geomarker.io/dep_index/).

<!--
##### Geomarker Data

- 2010 tract shape files are stored at: [`s3://geomarker/geometries/tracts_2010_sf_5072.rds`](https://geomarker.s3.us-east-2.amazonaws.com/geometries/tracts_2010_sf_5072.rds).
- 2018 deprivation index data is stored at: [`s3://geomarker/tract_dep_index_2018.rds`](https://geomarker.s3.us-east-2.amazonaws.com/tract_dep_index_2018.rds) and is also available for download at [https://geomarker.io/dep_index/](https://geomarker.io/dep_index/).
-->
#### 3. Drive time

This container uses isochrones to assign drive time to care center for each input address. Drive time isochrones are concentric polygons, in which each point inside a polygon has (roughly) the same drive time to the care center. Below is an example of drive time isochrones around the PCGC center in Utah

![](figs/PCGC_UTAH_isochrones_fig.png)

Drive time isochrones were obtained using a [self-hosted openroute service](https://maps.openrouteservice.org/reach?n1=38.393339&n2=-95.339355&n3=5&b=0&i=0&j1=30&j2=15&k1=en-US&k2=km) in order to overcome the time limitations of the publicly available API.

<!-- Drive time is computed based on distance between input address and care center: -->
<!-- <center>drive_time = distance/60</center> -->

We defined 24 levels of isochrones with driving distances up to 960 minutes (16 hours):  15  30  45  60  75  90  105 120 135 150 165 180 195 210 225 240 300 360 420 480 600 720 840 960 minutes


<!-- ##### Geomarker Data -->

<!-- - `download_isochrones.R` was used to download and prepare drive time isochrones -->
<!-- - Isochrone shape files are stored at [`s3://geomarker/drivetime/isochrones/`](https://geomarker.s3-us-east-2.amazonaws.com/drivetime/isochrones) -->
<!-- - A list of available care center addresses is also stored at [`s3://geomarker/drivetime/center_addresses.csv`](https://geomarker.s3-us-east-2.amazonaws.com/drivetime/center_addresses.csv) -->


## DeGAUSS Details

 For detailed documentation on DeGAUSS, including general usage and installation, please see the [DeGAUSS homepage](https://degauss.org).
