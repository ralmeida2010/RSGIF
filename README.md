# RSGIF
Portuguese National Database of Forest Fires 1880-until now - R access and script

The Portuguese Rural Fire Database (PRFD) is a valuable resource that provides detailed insights into wildfire dynamics in Portugal, offering comprehensive data on fire occurrences, causes, and locations. Understanding the processes involved in creating and managing these records is crucial for leveraging the PRFD for effective wildfire prevention and management strategies. 

Was building upon the research presented by using the PRFD as a pivotal resource for comprehending and managing wildfires in Portugal. 
Crossing from 1980 to the present, the PRFD provides comprehensive data on wildfire incidents, encompassing fire counts, locations, causes, and extents. 

The database has a history intertwined with the evolution of the forest service and civil protection, shaped by advancements in information technology.
Two main periods have been established (1980-2000; 2001-now) in relation to methods of information collection and then presented as uniform dataset. 

The first period is characterized by limited data; for instance, there was a lack of precise information regarding the exact starting points of fires. During this time, information relied on toponymic data without real coordinates, primarily referencing parish names. Data collection only included information on burned areas in terms of shrubs and stands.
In the second period, information became more detailed. For example, when a toponym was identified, the name of the nearest location was registered using Carta Militar de Portugal Série M888, a 1:25,000 scale map. Recently, GPS coordinates have been integrated into new radio communication systems for accurate location recording.
Initially, during the second period, burned areas were classified by vegetation type. Today, fires larger than 10 hectares are associated with geographic perimeters, allowing for cross-validation with other geographic data sources.
Here, new data acquisition methods and presented like functions developed to correct data inaccuracies and to approximate the structure of the two periods. We present the new dataset with associated time-stamped coordinates, facilitating the generation of new insights through cross-referencing with other datasets. This new information is categorized into three groups: Social indicator, including density people and buildings; Physical indicators, including topographic features, land use, and adaptation to fuel models; Meteorological indicators, utilizing location-specific meteorological and fire-related data; Fire database reanalysis, identifying patterns simultaneous fires.

New information is the base to develop new models that can forecast fire behavior or can help the decision in fire prevention. A description of most relevant variables is presented to decode some insights. We also establish a way to new users uses daily data series with some indicators associated with. The concept of fire regime is review with analyses that take in account the distribution of several variables stratified by district and decade. This new approach highlights that fire regimes are not static but have accentuated variability over time.
All data can be downloaded from the following link: ICNF Data Download.
All analyses can be performed using the R scripts available at: SGIF GitHub Repository.

