BH 2/20
Preparation of monitoring sites consists of these steps:

1) Download the available sites for your HUC8 watershed of interest.
(If running a project area that covers multiple HUC8s, download them individuall them merge prior to loading to GIS).

2). Load into GIS as a comma-delimmited file.

3). Load the WQCD 2018 (or most recent) 305b/303d segmentation line files into the map

4). Use nearest-neighbor join and join the segment attributes to each monitoring site

5). Manually check monitoring sites that are close to stream junctions to make sure they joined the right segment data.  Easiest way to do that is to turn labels on for the monitoring site points and the stream segments, (use the AUID attribute for both) and compare.  Also, coloring the monitoring sites categorically by AUID will show you the groups of sites associated with each stream segment AUID, the colored site clusters should correspond to the stream reaches.

6). Delete the extra attributes to clean up the monitoring site file.  Keep at least the segment attributes of WBID, AUID, Portion Description, Cat, Aq Life tier, Imp analyte, etc and the various use statuses. 

7). Once the joined attribute have been checked and seem correct, download the attribute table as a .csv and save it in the report_card/ProgramFiles directory; this will be used by the program as the key to link each
site with a 305(b) segment.




