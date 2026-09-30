### Dataframe overview figure 1

#--- All cleaned up data used for models ---------------------------------------

# "int" signifies intervention groups comparison data (young/old intervention)
# "con" signifies intervention vs. control comparison data (old intervention/old control)


#--- Humac dynamometer data ----------------------------------------------------

iso_isom <- readRDS("./data/data-gen/iso_isom.rds")
isom_int <- readRDS("./data/data-gen/iso_isom_int.RDS")
isom_con <- readRDS("./data/data-gen/iso_isom_con.rds")

iso_isok <- readRDS("./data/data-gen/iso_isok.rds")
pt_60_int <- readRDS("./data/data-gen/pt_60.rds")
pt_60_con <- readRDS("./data/data-gen/pt_60_con.rds")

pt_120_int <- readRDS("./data/data-gen/pt_120.rds")
pt_120_con <- readRDS("./data/data-gen/pt_120_con.rds")

pt_240_int <- readRDS("./data/data-gen/pt_240.rds")
pt_240_con <- readRDS("./data/data-gen/pt_240_con.rds")

#--- Keiser Legpress -----------------------------------------------------------

keiser_dat <- readRDS("./data/data-gen/keiser_dat.rds")
keiser_int <- readRDS("./data/data-gen/keiser_int.rds")
keiser_con <- readRDS("./data/data-gen/keiser_con.rds")

#--- Bicep curl MVC ------------------------------------------------------------

bc_dat <- readRDS("./data/data-gen/bc_dat.rds")
bc_int <- readRDS("./data/data-gen/bc_int.rds")
bc_con <- readRDS("./data/data-gen/bc_con.rds")

#--- Mucsle Volume -------------------------------------------------------------

muscle_volume <- readRDS("./data/data-gen/muscle_volume.RDS")

#--- Muscle Thicknes -----------------------------------------------------------

muscle_thickness <- readRDS("./data/data-gen/muscle_thickness.rds")

#--- Lean mass Arms ------------------------------------------------------------

lean_dat <- readRDS("./data/data-gen/lean_dat.rds")

#--- Exercise Volume -----------------------------------------------------------

exercise_dat <- readRDS("./data/data-gen/exercise_volume.rds")









