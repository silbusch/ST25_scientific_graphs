Summer Term 2025 | Scientific Graphs | 04-GEO-SOS13-1

These graphics were created as part of the Scientific Graphs course at Julius Maximilian University of Würzburg.
The used data set is not freely accessible.
*Data Source: Henri Debray, German Aerospace Center (DLR)*

## Neighbouring building

**Note:** Buildings are considered neighbours if at least 50% of their area lies within the buffer zone of the target building.
The greyed-out buildings were included in the determination of the neighbours, but were not analysed themselves.

<img width="3543" height="4251" alt="cairo_neighbours_buffer50_without_edge" src="https://github.com/user-attachments/assets/17a4aa8f-f966-4db5-9094-66e47e138bc9" />

## Building patches

**Note:** Connected buildings were grouped together and their number and area were visualised in a binary representation. In this version, equal class intervals were used.
*Possible source of error: Inaccurate spatial building classification can lead to an increase in individual buildings.*

<img width="5669" height="4251" alt="cairo_patches" src="https://github.com/user-attachments/assets/c78dcf03-7b15-413c-bc58-ab7d93d34b9b" />

**Note:** In the following version, quantiles were used to determine the class boundaries.

<img width="5669" height="4251" alt="cairo_patches_quantiles" src="https://github.com/user-attachments/assets/fe9850ee-3bac-42d1-8742-8240826b60ef" />

## Average building size

**Note:** The average building size in the area surrounding a building is determined by the size of the building itself and the size of neighbouring buildings.
The edge buildings were removed, but still included in the analysis for the remaining buildings.

<img width="3543" height="4251" alt="cairo_avg_building_area" src="https://github.com/user-attachments/assets/52ef6826-f1b0-46c0-91e4-bfc0f8af01d3" />

# Building dominance

**Note:** The dominance of a building is described by its surface area in comparison to whole neighbouring buildings within a radius of 50 metres.
*Disclaimer: X-axis scaling problem due to discrete scaling.*

<img width="3543" height="4251" alt="cairo_building_area_dominance" src="https://github.com/user-attachments/assets/e62420f4-fbf2-41af-a678-34231b05d7ad" />
