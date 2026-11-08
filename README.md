# RSGIF
RSGIF — Portuguese Rural Fire Database (PRFD), 1980 to the present

R scripts to download, harmonize, enrich and quality-assess the Portuguese Rural Fire Database (PRFD): the official record of rural fires in mainland Portugal kept by the Institute for Nature Conservation and Forests (ICNF), with more than 820,000 fire records since 1980.

Raw data (updated regularly by ICNF): https://fogos.icnf.pt/download/ExportarDadosSGIF/
Rebuilt database, 1980–2025 (CSV and GeoPackage): https://doi.org/10.5281/zenodo.23165487
About the database

The PRFD records the date, time, location, cause and burned area of every rural fire reported in mainland Portugal. Its history follows that of the forest service and of civil protection, and of the information systems they used. The records come in five data structures, which fall into two main periods:

1980–2000. Records were processed manually. Fires were located by toponym and parish, without coordinates, and from 1980 to 1993 most records also lack the district and municipality. Burned area was recorded by land type (forest stands, shrubland, agriculture).
2001 to the present. The Wildfire Information System (SGIF) introduced digital records and GIS validation. The ignition point has coordinates, first derived from the nearest place name of the Carta Militar de Portugal (series M888, 1:25,000) and, increasingly, recorded by GPS through the radio communication systems. Burned-area perimeters are mapped from satellite imagery (Landsat, and Sentinel-2 in recent years), which allows the records to be checked against other geographic sources.

Because the two periods were collected in different ways, the raw records contain missing locations, missing or impossible end times, zero burned areas, duplicates and changing administrative names. This repository corrects these problems and brings the whole series to a common structure.

What the pipeline does:
Harmonization. Consolidates the five data structures, standardizes administrative names, removes duplicates, normalizes cause codes and places records without coordinates at their toponym, with an estimated location uncertainty.
Correction and imputation. Corrects missing, negative and anomalous end times with explicit rules, and fills missing or zero burned areas and durations by multiple imputation (MICE). Original values are kept; imputed values are stored in separate fields and flagged.
Linkage to burned-area perimeters. Links the fire records to the national burned-area perimeters (PBA), with a confidence flag for each link.
Enrichment. Adds variables in eight information categories:
Meteorological: hourly ERA5 weather and the Fire Weather Index and its sub-indices at the ignition point;
Physical: altitude, slope, terrain ruggedness, solar exposure, land use and fuel model, structural fire danger;
Vegetation state: MODIS GPP, FAPAR, LAI, NDVI, EVI and NDII7 before ignition;
Fire behaviour and effects: Fire Radiative Energy (SEVIRI), burn severity (EFFIS and the Portuguese Burn Severity Atlas), link to PT-FireSprd;
Social: population and building density, road density;
Operational: nearest fire station and simultaneous fires, as a proxy for pressure on suppression resources.
Quality assessment. Scores every record on four quality indicators (location, area agreement, imputation, independent confirmation) and assigns a quality tier (A, B, U, C), so that users can choose the records that suit their analysis.
External validation. Compares the database with EFFIS burned-area perimeters and NASA FIRMS active fires.

The enriched database supports analyses of fire occurrence, causes and burned area over four decades, the calibration of fire-danger indices, studies of human ignitions and exposure, and the development of models for fire prevention and management.
