# validation data for getDataByHUC8
getDataByHUC8_validate(HUC8, StartDate, EndDate)

# save datasheet to data frame for comparison
validation_data <- read.csv(file="./Data/HUC_14010005_dataSheet.2020-02-26.csv",header=T,sep = "",stringsAsFactors=F,strip.white=T)

# test data for getDataBYHUC8
getDataByHUC8(HUC8, StartDate, EndDate)

# save test datasheet 
test_data <- read.csv(file="./Data/HUC_14010005_dataSheet.2020-02-26.csv",header=T,sep = "",stringsAsFactors=F,strip.white=T)

# check if data are identical
all.equal(validation_data, test_data)

#######################

# validation data for orgList
Organization = orgList_validate(CharacteristicNames, Units, valueType)

# test data for orgList
Organization_test = orgList(CharacteristicNames, Units, valueType)

# check if lists are identical
org_output = identical(Organization, Organization_test)
print(paste("orgList output identical", org_output, sep= " "))

#######################

# validation data for standardize_units
data_standardized = standardize_units_validate(Data)

# test data for standardize_units
data_standardized_test = standardize_units(Data)

# check if data frames are identical 
all.equal(data_standardized, data_standardized_test)

#######################

# validation data for retrieveNumericalData
temperatureData = retrieveNumericalData_validate(Data, c("Temperature", "Temperature, water"), "deg C", "")
pHData = retrieveNumericalData_validate(Data, "pH", "Std. Units", "")
hardnessData = retrieveNumericalData_validate(Data, c("Hardness, Ca, Mg as CaCO3","Hardness, Ca, Mg", "Total hardness -- SDWA NPDWR", "Hardness"), c("mg/l", "mg/l CaCO3"), "")

# test data for retrieveNumericalData
temperatureData_test = retrieveNumericalData(Data, c("Temperature", "Temperature, water"), "deg C", "")
pHData_test = retrieveNumericalData(Data, "pH", "Std. Units", "")
hardnessData_test = retrieveNumericalData(Data, c("Hardness, Ca, Mg as CaCO3","Hardness, Ca, Mg", "Total hardness -- SDWA NPDWR", "Hardness"), c("mg/l", "mg/l CaCO3"), "")

# check if data frames are identical
all.equal(temperatureData, temperatureData_test)
all.equal(pHData, pHData_test)
all.equal(hardnessData, hardnessData_test)

#######################

# validation data for countSamples
sample_count = countSamples_validate(monitoringStation, Data)

# test data for countSamples
sample_count_test = countSamples(monitoringStation, Data)

#check if data frames are identical
all.equal(sample_count, sample_count_test)

#######################

# validation data for findSampleLocations
sample_locations = findSampleLocations_validate(segmentName, Data)

# test data for findSampleLocations
sample_locations_test = findSampleLocations(segmentName, Data)

# check if data frames are identical
all.equal(sample_locations, sample_locations_test)

#######################

# validation data for summarizeSegments
segment_summary = summarizeSegments_validate(segmentName, monitoringStation)

# test data for summarizeSegments
segment_summary_test = summarizeSegments(segmentName, monitoringStation)

# check if data frames are identical
all.equal(segment_summary, segment_summary_test)

#######################

# validation data for retrieveNumericalData
numerical_data = retrieveNumericalData_validate(Data, CharacteristicNames, Units, valueType)

# test data for retrieveNumericalData
numerical_data_test = retrieveNumericalData(Data, CharacteristicNames, Units, valueType)

# check if data frames are identical
all.equal(numerical_data, numerical_data_test)

#######################

# validation data for retrieveCategoricalData
Categorical_data = retrieveCategoricalData_validate(Data, CharacteristicNames, Units, valueType)

# test data for retrieveNumericalData
Categorical_data_test = retrieveCategoricalData(Data, CharacteristicNames, Units, valueType)

# check if data frames are identical
all.equal(Categorical_data, Categorical_data_test)

#######################
invertebrate_char = "Invertebrates;"
# validation data for retrieveInvertebrateData
Invertebrate_data = retrieveInvertebrateData_validate(Data, invertebrate_char, Units, valueType)

# test data for retrieveNumericalData
Invertebrate_data_test = retrieveInvertebrateData(Data, invertebrate_char, Units, valueType)

# check if data frames are identical
all.equal(Invertebrate_data, Invertebrate_data_test)

#######################

# validation data for findSeverity
severity = findSeverity_validate(Categorical_data$ResultMeasureValue)

# test data for findSeverity
severity_test = findSeverity(Categorical_data_test$ResultMeasureValue)

# check if outputs are identical
identical(severity, severity_test)

#######################
Maximum = max(numerical_data$ResultMeasureValue, na.rm=TRUE)

# validation data for maxDate
datemax = maxDate_validate(CharacteristicNames, Units, valueType, Maximum)

# test data for maxDate
datemax_test = maxDate(CharacteristicNames, Units, valueType, Maximum)

# check if outputs are identical
identical(datemax, datemax_test)

#######################

# validation data for calculateTrend
trend = calculateTrend_validate(numerical_data)

# test data for calculateTrend
trend_test = calculateTrend(numerical_data)

# check if outputs are identical
identical(trend, trend_test)

#######################
ammonia_standard = "((0.0577/(1+10^7.688-pH))+(2.487/(1+10^(pH-7.688)))*min(2.85, 1.45*10^(0.028*(25-temp))))"

# validation data for ammonia_eval
ammonia_calc = ammonia_calc_eval_validate(numerical_data, ammonia_standard)
 
# test data for ammonia_eval
ammonia_test = ammonia_calc_eval(numerical_data, ammonia_standard)

# check if outputs are identical
identical(ammonia_calc, ammonia_test)

#######################
annual_standard = 1250

# validation data for annual_median_eval
annual_eval = annual_median_eval_validate(numerical_data, annual_standard)

# test data for annual_median_eval
annual_test = annual_median_eval(numerical_data, annual_standard)

# check if outputs are identical
identical(annual_eval, annual_test)

#######################
DO_standard = 6

# validation data for DO_eval
DO_validate = DO_eval_validate(numerical_data, DO_standard)

# test data for DO_eval
DO_test = DO_eval(numerical_data, DO_standard)

# check if outputs are identical
identical(DO_validate, DO_test)

#######################
hardness_standard = "exp(0.819*(log(hardness))-0.5340)"

# validation data for hardness_calc_eval
hard_validate = hardness_calc_eval_validate(numerical_data, hardness_standard)

# validation data for hardness_calc_eval
hard_test = hardness_calc_eval(numerical_data, hardness_standard)

# check if outputs are identical
identical(hard_validate, hard_test)

#######################
# validation data for mmi_eval
mmi_validate = mmi_eval_validate(Invertebrate_data, dummy)

# test data for mmi_eval
mmi_test = mmi_eval(Invertebrate_data, dummy)

# check if outputs are identical
identical(mmi_validate, mmi_test)

#######################
one_day_standard = 0.2 

# validation data for one_day_eval
one_validate = one_day_eval_validate(numerical_data, one_day_standard)

# test data for one_day_eval
one_test = one_day_eval(numerical_data, one_day_standard)

# check if outputs are identical
identical(one_validate, one_test)

#######################
range_standard = "5.0, 9.0"

# validation data for range_eval
range_validate = range_eval_validate(numerical_data, range_standard)

# test data for range_eval
range_test = range_eval(numerical_data, range_standard)

# check if outputs are identical
identical(range_validate, range_test)

####################### PROBLEMS WITH SUMMER_MAX_EVAL...no rolling mean object for either function put rollingMean function in test function 2/25/20 WW
summer_standard = 150

# validation data for summer_max_eval
summer_validate = summer_max_eval_validate(numerical_data, summer_standard)

# test data for summer_max_eval
summer_test = summer_max_eval(numerical_data, summer_standard)

# check if outputs are identical
identical(summer_validate, summer_test)

####################### PROBLEMS TESTING TEMP_EVAL...NO TEMPERATURE STANDARD FOR TEST SEGMENT DATA 2/25/20 WW
# validation data for temp_eval
temp_validate = temp_eval_validate(numerical_data, standard)

# test data for temp_eval
temp_test = temp_eval(numerical_data, standard)

# check if outputs are identical
identical(temp_validate, temp_test)

#######################
thirty_standard = 20

# validation data for thirty_day_eval
thirty_day_validate = thirty_day_eval_validate(numerical_data, thirty_standard)

# test data for thirty_day_eval
thirty_day_test = thirty_day_eval(numerical_data, thirty_standard)

# check if outputs are identical
identical(thirty_day_validate, thirty_day_test)

#######################
two_month_standard = 126

# validation data for two_month
two_month_validate = two_month_geometric_mean_eval_validate(numerical_data, two_month_standard)

# test data for two_month
two_month_test = two_month_geometric_mean_eval(numerical_data, two_month_standard)

# check if outputs are identical
identical(two_month_validate, two_month_test)

#######################
mean_data = c(1,5,2,8,2,2.5, 1000, 304, 2,4,5,6,7,7)

# validation data for geometric mean
gm_validate = geometricMean_validate(mean_data)

# test data for geometric mean
gm_test = geometricMean(mean_data)

# check if outputs are identical
identical(gm_validate, gm_test)

#######################FIXED ISSUE, COMMENTED OUT TROUBLESHOOTING CODE FOR THESE TO RUN 2/25/20 WW

# validation data for summaryCalcs
sum_validate = summaryCalcs_validate(UseClass, Category, Indicator, CharacteristicNames, Units, valueType, standardType, standard)

# test data for summaryCalcs
sum_test = summaryCalcs(UseClass, Category, Indicator, CharacteristicNames, Units, valueType, standardType, standard)

# check if outputs are identical 
all.equal(sum_validate, sum_test)

#######################
## must manually compare function output as they are pngs
# validation data for plotDischarge
discharge_plot = plotDischarge_validate(segmentName, Data)

# test data for plotDischarge
discharge_plot_test = plotDischarge(segmentName, Data)

#######################
# validation data for summaryBarCharts
bar_chart = summaryBarCharts_validate(segmentName, Data)

# test data for summaryBarCharts
bar_chart_test = summaryBarCharts(segmentName, Data)

#######################
# validationn data for mapSiteData
map_validate = mapSiteData_validate(watershedData, monitoringStations)

# test data for mapSite Data
map_test = mapSiteData(watershedData, monitoringStations)
