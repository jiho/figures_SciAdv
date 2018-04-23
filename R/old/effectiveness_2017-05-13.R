library(tidyverse)
library(readxl)
library(knitr)       # kable : prettier data.frame output
library(viridis)

# prepare data ####
dat <- read_xlsx("../../interim_documents/backups_google_docs/Ocean Solutions Tables_2017-05-13d.xlsx",
                 skip=17, n_max = 75, trim_ws = TRUE,
                 sheet = "T3_Systems-Ocean", col_names = TRUE,
                 col_types = "text") %>%
  dplyr::select(-`Assessment guidance`) %>%
  dplyr::filter(grepl("Effectiveness to mimise impacts from", Criteria)) %>%
  mutate_each(funs(as.integer), 3:18)
colnames(dat) <- c("Category", "Criteria", "Renewables", "Vegetation (g)", "Productivity",
                   "Alkalinity (g)","Others", "Cloud", "Albedo", "Alkalinity (l)",
                   "Vegetation (l)", "Pollutants", "Hydrology", "Overexploitation",
                   "Protection", "Invasion", "Evolution",
                   "Relocation & Restoration")
dat
dat$Criteria[dat$Criteria == "Effectiveness to mimise impacts from warming (T1a or T1b with T2)"] <-
  "OW"
dat$Criteria[dat$Criteria == "Effectiveness to mimise impacts from ocean acidification (T1a or T1b with T2)"] <-
  "OA"
dat$Criteria[dat$Criteria == "Effectiveness to mimise impacts from sea level rise (T1a or T1b with T2)"] <-
  "SLR"

dat_long <- gather(data = dat, key = Cat, value = score, 3:18)


gg <- ggplot(data = dat_long, 
             aes(y=Cat, 
                 x=Criteria, fill=score)) +
  labs(x = "Driver", y = NULL, title = "Effectiveness of each solution to minimize risks of impacts from\nocean warming (OW) and acidification (OA), and sea level rise (SLR)") +
  geom_tile(color="white", size=0.1) +
  #coord_equal() +
  scale_fill_viridis(name="Score") +
  facet_wrap(~Category, ncol=3) +
  coord_fixed(ratio = 0.3)
gg
