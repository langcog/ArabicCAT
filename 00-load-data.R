




## new data from Haifa Alroqi June 7, 2022 - 82 WG subjects
sheets_wg <- readxl::excel_sheets(path="data/Alroqi et al. 2020 - Saudi CDI - WG.xlsx")
raw_wg <- read_xlsx(path="data/Alroqi et al. 2020 - Saudi CDI - WG.xlsx", sheet="Saudi CDI - WG")
notes_wg <- read_xlsx(path="data/Alroqi et al. 2020 - Saudi CDI - WG.xlsx", sheet="Notes")

wg_cols <- names(raw_wg)
colnames(raw_wg) = raw_wg[1,]
raw_wg <- raw_wg[3:nrow(raw_wg),]
wg_dem <- raw_wg[,c(1:23,574:576)] %>% # ToDo: calculate age from Timestamp - DOB ?
  rename(child_id = )
wg_voc <- raw_wg[,24:573]

# intersect(names(wg_voc), names(raw2))

new_wg_names = paste0(names(wg_voc), ".p")

intersect(new_wg_names, names(raw2)) # 511
setdiff(new_wg_names, names(raw2)) # 
# "puppy.p"              "ant.p"                "fly.insect.p"         "whistle.p"           
# "iPad.p"               "Nutella.p"            "coat.p"               "hair.clip.p"         
# "candle.p"             "knife.p"              "mat.p"                "mountain.p"          
# "swing.noun.p"         "dada.child.nanny.p"   "finger.game.p"        "sneeze.noun.1.p"     
# "say.u.p"              "fall.down.p"          "blow.nose.p"          "crash.p"             
# "get.sth.out.p"        "throw.p"              "go.down.p"            "black.p"             
# "old.in.age.p"         "cough.noun.p"         "thirsty.p"            "that.masculine.p"    
# "finished.no.more.1.p" "i.dont.want.p"        "finished.p"           "finished.no.more.2.p"
# "another.p"            "some.p"               "if.p"   

# ToDo: fix difficult format -- looks like each row (child) lists the items they know in each category, but 
# NOT in the same columns...need to either iterate over each row and try to match the columns, or reformat by hand..
sheets_ws <- readxl::excel_sheets(path="data/Alroqi et al. 2020 - Saudi CDI - WS.xlsx")
raw_ws <- read_xlsx(path="data/Alroqi et al. 2020 - Saudi CDI - WS.xlsx", sheet="Saudi CDI - WS") # 243 subjects
notes_ws <- read_xlsx(path="data/Alroqi et al. 2020 - Saudi CDI - WS.xlsx", sheet="Notes")

intersect(raw_wg[1,], raw_ws[1,])