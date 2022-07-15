#adult child population

library(dplyr)

# population

# 2019
dir.create("tmp")
unzip("data/population/sape22dt2mid2019lsoasyoaestimatesunformatted.zip",
      exdir = "tmp")
pop19 <- readxl::read_excel("tmp/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx",
                            sheet = "Mid-2019 Persons")
unlink("tmp", recursive = TRUE)
pop19 <- as.data.frame(pop19)
names(pop19) <- pop19[4,]
pop19 <- pop19[5:nrow(pop19),]
#pop19 <- pop19[,c(1,7)]

# 2018
dir.create("tmp")
unzip("data/population/pop2018.zip",
      exdir = "tmp")
pop18 <- readxl::read_excel("tmp/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx",
                            sheet = "Mid-2018 Persons")
unlink("tmp", recursive = TRUE)
pop18 <- as.data.frame(pop18)
names(pop18) <- pop18[4,]
pop18 <- pop18[5:nrow(pop18),]
#pop18 <- pop18[,c(1,4)]


# 2017
dir.create("tmp")
unzip("data/population/pop2017.zip",
      exdir = "tmp")
pop17 <- readxl::read_excel("tmp/SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS",
                            sheet = "Mid-2017 Persons")
unlink("tmp", recursive = TRUE)
pop17 <- as.data.frame(pop17)
names(pop17) <- pop17[4,]
pop17 <- pop17[5:nrow(pop17),]
#pop17 <- pop17[,c(1,4)]


# 2016
dir.create("tmp")
unzip("data/population/pop2016.zip",
      exdir = "tmp")
pop16 <- readxl::read_excel("tmp/SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.XLS",
                            sheet = "Mid-2016 Persons")
unlink("tmp", recursive = TRUE)
pop16 <- as.data.frame(pop16)
names(pop16) <- pop16[4,]
pop16 <- pop16[5:nrow(pop16),]
#pop16 <- pop16[,c(1,4)]

# 2015
dir.create("tmp")
unzip("data/population/pop2015.zip",
      exdir = "tmp")
pop15 <- readxl::read_excel("tmp/SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.XLS",
                            sheet = "Mid-2015 Persons")
unlink("tmp", recursive = TRUE)
pop15 <- as.data.frame(pop15)
names(pop15) <- pop15[4,]
pop15 <- pop15[5:nrow(pop15),]
#pop15 <- pop15[,c(1,4)]

# 2014
dir.create("tmp")
unzip("data/population/pop2014.zip",
      exdir = "tmp")
pop14 <- readxl::read_excel("tmp/SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.XLS",
                            sheet = "Mid-2014 Persons")
unlink("tmp", recursive = TRUE)
pop14 <- as.data.frame(pop14)
names(pop14) <- pop14[4,]
pop14 <- pop14[5:nrow(pop14),]
#pop14 <- pop14[,c(1,4)]

# 2013
dir.create("tmp")
unzip("data/population/pop2013.zip",
      exdir = "tmp")
pop13 <- readxl::read_excel("tmp/SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.XLS",
                            sheet = "Mid-2013 Persons")
unlink("tmp", recursive = TRUE)
pop13 <- as.data.frame(pop13)
names(pop13) <- pop13[4,]
pop13 <- pop13[5:nrow(pop13),]
#pop13 <- pop13[,c(1,4)]

# 2012
dir.create("tmp")
unzip("data/population/pop2012.zip",
      exdir = "tmp")
pop12 <- readxl::read_excel("tmp/SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.XLS",
                            sheet = "Mid-2012 Persons")
unlink("tmp", recursive = TRUE)
pop12 <- as.data.frame(pop12)
names(pop12) <- pop12[4,]
pop12 <- pop12[5:nrow(pop12),]
#pop12 <- pop12[,c(1,4)]

# 2011
dir.create("tmp")
unzip("data/population/pop2011.zip",
      exdir = "tmp")
pop11 <- readxl::read_excel("tmp/mid-2011-lsoa-quinary-estimates.xls",
                            sheet = "Mid-2011 Persons")
unlink("tmp", recursive = TRUE)
pop11 <- as.data.frame(pop11)
names(pop11) <- pop11[3,]
pop11 <- pop11[4:nrow(pop11),]
#pop11 <- pop11[,c(1,4)]

# 2002 - 2011
dir.create("tmp")
unzip("data/population/pop2002-2011.zip",
      exdir = "tmp")
pop02 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                            sheet = "Mid-2002")
pop03 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                            sheet = "Mid-2003")
pop04 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                            sheet = "Mid-2004")
pop05 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                            sheet = "Mid-2005")
pop06 <- readxl::read_excel("tmp/SAPE8DT1a-LSOA-syoa-unformatted-persons-mid2002-to-mid2006.xls",
                            sheet = "Mid-2006")
pop07 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                            sheet = "Mid-2007")
pop08 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                            sheet = "Mid-2008")
pop09 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                            sheet = "Mid-2009")
pop10 <- readxl::read_excel("tmp/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls",
                            sheet = "Mid-2010")
unlink("tmp", recursive = TRUE)
pop02 <- as.data.frame(pop02)
pop03 <- as.data.frame(pop03)
pop04 <- as.data.frame(pop04)
pop05 <- as.data.frame(pop05)
pop06 <- as.data.frame(pop06)
pop07 <- as.data.frame(pop07)
pop08 <- as.data.frame(pop08)
pop09 <- as.data.frame(pop09)
pop10 <- as.data.frame(pop10)

pop_02_10 <- list(pop02, pop03, pop04, pop05, pop06, pop07, pop08, pop09, pop10)
names(pop_02_10) <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
pop_02_10 <- bind_rows(pop_02_10, .id = "year")
names(pop_02_10)[96] = c("90+")
names(pop_02_10)[6:95] <- gsub("p","",names(pop_02_10)[6:95] )

pop_2012_2018 <- list(pop12, pop13, pop14, pop15, pop16, pop17, pop18)
names(pop_2012_2018) <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
pop_2012_2018 <- bind_rows(pop_2012_2018, .id = "year")

pop_02_10 <- pop_02_10[,c("year","LSOA11CD","all_ages",as.character(0:89),"90+")]
pop_2012_2018 <- pop_2012_2018[,c("year","Area Codes" ,"All Ages",as.character(0:89),"90+")]
names(pop_2012_2018)[1:3] <- c("year","LSOA11CD","all_ages")

pop_non_2011 <- rbind(pop_02_10, pop_2012_2018)

pop_non_2011 <- pop_non_2011[substr(pop_non_2011$LSOA11CD,1,3) %in% c("E01","W01"),]
pop_non_2011$`90+` <- as.numeric(pop_non_2011$`90+`)

# Add age bands
pop_non_2011$`0-4` <- rowSums(pop_non_2011[,as.character(0:4)])
pop_non_2011$`5-9`<- rowSums(pop_non_2011[,as.character(5:9)])
pop_non_2011$`10-14`<- rowSums(pop_non_2011[,as.character(10:14)])
pop_non_2011$`15-19`<- rowSums(pop_non_2011[,as.character(15:19)])
pop_non_2011$`20-24`<- rowSums(pop_non_2011[,as.character(20:24)])
pop_non_2011$`25-29`<- rowSums(pop_non_2011[,as.character(25:29)])
pop_non_2011$`30-34`<- rowSums(pop_non_2011[,as.character(30:34)])
pop_non_2011$`35-39`<- rowSums(pop_non_2011[,as.character(35:39)])
pop_non_2011$`40-44`<- rowSums(pop_non_2011[,as.character(40:44)])
pop_non_2011$`45-49`<- rowSums(pop_non_2011[,as.character(45:49)])
pop_non_2011$`50-54`<- rowSums(pop_non_2011[,as.character(50:54)])
pop_non_2011$`55-59`<- rowSums(pop_non_2011[,as.character(55:59)])
pop_non_2011$`60-64`<- rowSums(pop_non_2011[,as.character(60:64)])
pop_non_2011$`65-69`<- rowSums(pop_non_2011[,as.character(65:69)])
pop_non_2011$`70-74`<- rowSums(pop_non_2011[,as.character(70:74)])
pop_non_2011$`75-79`<- rowSums(pop_non_2011[,as.character(75:79)])
pop_non_2011$`80-84`<- rowSums(pop_non_2011[,as.character(80:84)])
pop_non_2011$`85-89`<- rowSums(pop_non_2011[,as.character(85:89)])


pop_all <- pop_non_2011[,c("year","LSOA11CD","all_ages",
                           "0-4","5-9",
                           "10-14","15-19","20-24","25-29",
                           "30-34","35-39","40-44","45-49",
                           "50-54","55-59","60-64","65-69",
                           "70-74","75-79","80-84","85-89","90+")]
pop_all$all_ages <- as.numeric(pop_all$all_ages)

pop11 <- pop11[,c(1,4:23)]
pop11$year <- "2011"
pop11[2:21] <- lapply(pop11[2:21], as.numeric)#
names(pop11)[1:2] <- c("LSOA11CD","all_ages")
pop11 <- pop11[substr(pop11$LSOA11CD,1,3) %in% c("E01","W01"),]


pop_final <- bind_rows(list(pop_all, pop11))
pop_final$year <- as.numeric(pop_final$year)

pop_final <- pop_final[order(pop_final$LSOA11CD,pop_final$year),]

saveRDS(pop_final, "data/population/LSOA_population_2002_2018_agebands.Rds")

# test E01033173 
library(tidyr)
library(ggplot2)

foo <- pop_final[pop_final$LSOA11CD == "E01033173",]

foo <- pivot_longer(foo, cols = c("all_ages","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                                  "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+"))
foo <- foo[foo$name != "all_ages",]
foo$name <- factor(foo$name, levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                   "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+"))
ggplot(foo, aes(x = year, y = value, fill = name)) +
  geom_area()

foo <- pop_final[pop_final$LSOA11CD == "E01033173",]
foo$adults <- rowSums(foo[,c(8:22)])
foo$children <- rowSums(foo[,c(4:7)])
foo <- foo[,c("year","adults","children")]

foo <- pivot_longer(foo, cols = c("adults","children"))
foo$name <- factor(foo$name, levels = c("children","adults"))

ggplot(foo, aes(x = year, y = value, fill = name)) +
  geom_area() +
  scale_x_continuous(breaks = seq(2002,2018,1)) +
  ylab("population") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )
ggsave("plots/E01033173_children.png")


