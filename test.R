#test.




barplotly_statistics = function(bike_data = parsed_bike_data[[3]],
                                datelim="",
                                plot_colors = default_colors(default = F)
){
  
  p2022<-ggplot(data=bike_data, aes(x=sum2022, y=reorder(Nom, sum2022),fill = Nom)) +
    geom_bar(stat="identity") + scale_fill_manual(values=plot_colors) + theme(legend.position = "none")
  p2021<-ggplot(data=bike_data, aes(x=sum2021, y=reorder(Nom, sum2021),fill = Nom)) +
    geom_bar(stat="identity") + scale_fill_manual(values=plot_colors) + theme(legend.position = "none")
  p2020<-ggplot(data=bike_data, aes(x=sum2020, y=reorder(Nom, sum2020),fill = Nom)) +
    geom_bar(stat="identity") + scale_fill_manual(values=plot_colors) + theme(legend.position = "none")
  p2019<-ggplot(data=bike_data, aes(x=sum2019, y=reorder(Nom, sum2019),fill = Nom)) +
    geom_bar(stat="identity") + scale_fill_manual(values=plot_colors) + theme(legend.position = "none")
  
  
  
  
  
  
  
  library(tidyverse)
  library(janitor)
  gdp <- read_csv("~/Desktop/GDP_Data.csv") #select required columns
  gdp <- gdp %>% select(3:15) #filter only country rows
  gdp <- gdp[1:217,]
  gdp_tidy <- gdp %>%
    mutate_at(vars(contains("YR")),as.numeric) %>%
    gather(year,value,3:13) %>%
    janitor::clean_names() %>%
    mutate(year = as.numeric(stringr::str_sub(year,1,4)))
  
  write_csv(gdp_tidy,"~/Desktop/gdp_tidy.csv")
  
  
  
  comptage_tidy = read_csv("data/comptage_summarised_plotly_2019_2022.csv")
  comptage_formatted = comptage_tidy %>%
    group_by(year) %>%
    mutate(rank = rank(-counts),
           Value_rel = value/value[rank==1],
           Value_lbl = paste0(" ",round(value/1e9))) %>%
    group_by(country_name) %>%
    filter(rank <=10) %>%
    ungroup()
  
  staticplot = ggplot(gdp_formatted, aes(rank, group = country_name,
                                         fill = as.factor(country_name), color = as.factor(country_name))) +
    geom_tile(aes(y = value/2,
                  height = value,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey" ),
          panel.grid.minor.x = element_line( size=.1, color="grey" ),
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
          plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
          plot.background=element_blank(),
          plot.margin = margin(2,2, 2, 4, "cm"))
  
  
  anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = TRUE)  +
    labs(title = 'GDP per Year : {closest_state}',
         subtitle  =  "Top 10 Countries",
         caption  = "GDP in Billions USD | Data Source: World Bank Data")
  
  
  
  animate(anim, 200, fps = 20,  width = 1200, height = 1000,
          renderer = gifski_renderer("gganim.gif"))
