source('helper_functions.R',local = TRUE)

# Read in the shapefile
shapefile <- readOGR("Neighborhoods/WGS84/", "Neighborhoods")

# Read in object ID mapping
id_mapping <- read.csv('object_id_mapping.csv', stringsAsFactors = FALSE) %>%
  mutate(object.ID = as.character(object.ID))

# Get neighborhood data
neighborhoods <- read.csv('seattle_overview.csv') %>%
  distinct() %>%
  spread(Measure.Names, Measure.Values) %>%
  rename(Year = Year.of.Period.End) %>%
  rename(Month = Month.of.Period.End) %>%
  arrange(Region, Year, Month) %>%
  separate(Region, c("City","Neighborhood"), sep=' - ') %>%
  mutate(common_name = tolower(Neighborhood)) %>%
  filter(common_name %in% unique(id_mapping$common_name)) %>%
  filter(is.na(Neighborhood)==FALSE) %>%
  mutate(ones = '1') %>%
  unite('date_string',one_of(c('Year','Month', 'ones')), sep=' ') %>%
  mutate(date = ymd(date_string)) %>%
  mutate(Year = year(date)) %>%
  mutate(Month = months(date))

# Map shape file ID to common ID
shapefile@data <- shapefile@data %>%
  mutate(OBJECTID = as.character(OBJECTID)) %>%
  left_join(id_mapping[c('object.ID','common_name')], by=c("OBJECTID"='object.ID')) %>%
  mutate(common_name=ifelse(common_name=="",'UNK',common_name)) %>%
  mutate(common_name=ifelse(is.na(common_name),'NA',common_name))

# Filter neighborhoods to only have Feb 2017 data (most recent) and join with shapefile
neighborhoods_2017 <- neighborhoods %>% 
  filter(Year==2017) %>%
  select(one_of(c('common_name','Neighborhood',"Days on Market","Median Sale Price","Homes Sold",
                  "Inventory","Median Ppsf"))) %>%
  group_by(common_name, Neighborhood) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))
shapefile@data <- shapefile@data %>%
  left_join(neighborhoods_2017, by='common_name')

# Normalize data for par coords plot
neighborhoods_group <- neighborhoods %>%select(one_of(c('common_name','date','Neighborhood')))
neighborhoods_vars <- neighborhoods %>% select(one_of(c("Days on Market","Median Sale Price","Homes Sold",
                                                        "Inventory","Median Ppsf"))) %>%
  mutate_each(funs(normalize))
neighborhoods_norm <- df_par <- bind_cols(neighborhoods_group, neighborhoods_vars)

