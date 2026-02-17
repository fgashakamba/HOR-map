# Check the avocado data and clean it for analysis
#-----------------------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, dplyr, stringr,  sf, 
               googlesheets4, RPostgres, stringr, tidyr, janitor)

# Load district boundaries and CWS locations
con <- dbConnect(drv = RPostgres::Postgres(),
                 dbname = 'main_db',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'Mapengu333?')

sectors <- st_read(con, query = "SELECT district, sector, geom FROM nisr.sectors")
sectors_2   <- st_read(file.path(getwd(), "data_wgs84/RW_sectors.gpkg"), quiet = TRUE)
sectors_2 %<>% rename(sector = name)
sectors_2 %<>% mutate(across(c(district, sector), ~str_squish(str_to_sentence(.))))  


#===============Load avocado coverage data===============================
#Authenticate googlesheets4
if(!gs4_has_token()){gs4_auth(email = "faustin.gashakamba@oneacrefund.org")}
url <- "1KBCj6bKPaBUPysBCy0DsgEkliN0VkaIvJSNlLF3eq1I"
avo_25A <- range_read(url, sheet = "Avocado_DB", col_names = T)

#----------cleaning the data---------------------
# sanitize column names
avo_25A %<>% clean_names()

# fill down district and sector names
avo_25A %<>% fill(district, .direction = "down")
avo_25A %<>% fill(sector, .direction = "down")
# convert number of trees to numeric, and filter out rows with missing or zero values
avo_25A %<>% mutate(number_of_trees = as.numeric(number_of_trees)) %>%
  filter_out(if_any(c(district, sector, number_of_trees), ~is.na(.))) %>%
  filter_out(number_of_trees == 0)

# sanitize district/sector names
avo_25A %<>% mutate(across(c(district, sector), ~str_squish(str_to_sentence(.))))  

# check how many unique district/sector are or are not matching
avo_25A %<>%
  group_by(district, sector) %>%
  summarise(number_of_trees = sum(number_of_trees, na.rm = T), .groups = "drop")

# check how many sector names are mis-spelled
View(avo_25A %>%  anti_join(sectors, by = c("district", "sector")))
                        