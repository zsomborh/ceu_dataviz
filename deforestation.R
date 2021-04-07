# TODO: remove unused packages

library(tidyverse)
library(scales)
library(leaflet)
library(sf)
library(ggthemes)
library(gganimate)
library(showtext)
library(magick)
library(tidyverse)
library(data.table)
library(glue)
library(countrycode)


# TODO in markdown add data introduction part with reference
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-04-06
tuesdata <- tidytuesdayR::tt_load('2021-04-06')


# Loading in data ---------------------------------------------------------



forest <- tuesdata$forest %>% as.data.table()
forest_area <- tuesdata$forest_area
brazil <- tuesdata$brazil_loss

forest[,sum(net_forest_conversion),by = list(year,entity)]



# Import world map plot + transformations ---------------------------------

# make a world map
world <- map_data("world")

forest <- forest %>%
    mutate(region = 
           recode(entity,
                  "Congo" = "Republic of Congo",
                  "French Guyana" = "French Guiana",
                  "United Kingdom" = "UK",
                  "United States" = "USA"
           ),
           
           # cut at specific point for mapping
           net_forest_conversion_f = cut(net_forest_conversion, breaks = c(-1500000, -400000, -200000, -100000, 0, 100000, 200000, 400000, 2000000))
    )

# Define function that creates maps
# TODO: do animation

create_map <- function(world, forest, year_of_plot) {
    
    # merge the datasets
    merged <- left_join(world, forest[year == year_of_plot,], by = "region")
    
    chart <- 
    merged %>%
        ggplot() +
        
        #Adding world map as polygon
        geom_polygon(data = merged, 
                     aes(x = long, 
                         y = lat, 
                         group = group, 
                         fill = net_forest_conversion_f), 
                     color = "black", 
                     size = .1) +
        
        # formatting labs so that nothing is shown but the title
        labs(title = glue("Net change in forest cover in {year_of_plot}"), x='', y='') + 
        
        # colouring and 
        scale_fill_brewer(palette = 'PRGn', name = '',
                          labels = c("-400,000 hectares", 
                                     "-200,000 hectares",
                                     "-100,000 hectares",
                                     "0 hectares",
                                     "+100,000 hectares",
                                     "+200,000 hectares",
                                     "+400,000 hectares",
                                     "+600,000 hectares")) + 
        theme(
            legend.position = "top",
            plot.title = element_text(hjust = 0.5, size = unit(24, "mm")),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank()
        ) +
        transition_states(year, transition_length = 2, state_length = 2) + #setting animation properties
        ease_aes('cubic-in-out')

    return(chart)

}


create_map(world,forest,2000)

# This won't work, animation is not created

library(animation)
ani.options(interval = 2)
saveGIF({
    for (i in c(1990,2000,2010,2015)) {
        create_map(world,forest,i)
        Sys.sleep(2)
    }
})

create_map(world,forest,2015)

chart

animate(chart, nframes = 300, fps = 20, width = 600, height = 600) 


# Create bar chart with top 10 absolute differences


#forest_area[forest_area$year == 2015,] 

#forar[order(forar$forest_area,decreasing= T),]


forest <- cbind(forest, countrycode(sourcevar = forest[,entity],
                                    origin = "country.name",
                                    destination = "continent"))
setnames(forest, 'V2', 'continent')


forest_area['continent'] <- countrycode(sourcevar = forest_area[["entity"]],
            origin = "country.name",
            destination = "continent")



# Check if we need records where countrycode() didn't find continent
View(unique(forest_area[is.na(forest_area$continent),'entity']))
# not really these are entities we don't care about 


agg_forest <- forest[!is.na(continent) ,list('net_forest_conversion'=sum(net_forest_conversion)),by=.(continent,year)]
agg_forest$positive<- ifelse(agg_forest$net_forest_conversion<0,'no','yes')
agg_forest$net_forest_conversion <- agg_forest$net_forest_conversion / 1000000
    
ggplot(data= agg_forest , aes(x = reorder(continent,net_forest_conversion), y = net_forest_conversion)) + 
    geom_bar(stat='identity', aes(fill=positive)) + 
    coord_flip() + 
    labs(title= 'Aggregate forest conversion for continents (in mm hectar)', x= '', y='') + 
    scale_fill_manual(values = c('darkorchid4','seagreen4')) + 
    theme(
        plot.title = element_text(hjust = 0.5, size = unit(15, "mm")),
        legend.position = 'none',
        # axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
    ) + facet_wrap(~year)


agg_forar <- forest_area[!is.na(continent) & year == 2015,
                    list('forest_area'=sum(forest_area)),
                    by=continent]

forest_area

agg_forar <- forest_area %>% filter(!is.na(continent)) %>% 
    group_by(continent, year) %>% 
    summarise(forest_area = sum(forest_area)) 

# let's try this one out 

ggplot(data = agg_forar, aes(x = year, y= forest_area, fill = continent)) + 
    geom_area() +  scale_fill_brewer(palette = 'PRGn') + 
    theme(
        plot.title = element_text(hjust = 0.5, size = unit(15, "mm")),
        legend.title = element_blank(),
        legend.position = 'top',
        # axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
    ) + labs(title ='Forest areas of continents between 1990 and 2020' , x='', y='')




# Try to do sankey chart - IT WONT WORK

library(networkD3)
library(stringr)

sth <- t(brazil[brazil$year == 2001,4:14])

temp_df <- t(brazil[brazil$year == 2001,4:14]) %>% as.data.frame( )
temp_df$node <- rownames(temp_df)
temp_df <- temp_df[order(temp_df$V1, decreasing = T),]
source = temp_df$node


temp_df_new <- t(brazil[brazil$year == 2002,4:14]) %>% as.data.frame( )
temp_df_new$node <- rownames(temp_df_new)
temp_df_new <- temp_df_new[order(temp_df_new$V1, decreasing = T),]
target = paste0(temp_df_new$node, '_new_year') 
value = temp_df_new$V1

nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
    source=source, 
    target=target, 
    value=value
)


match(str_sub(links$target,end = -10),nodes$name)-1



#links$IDsource <- match(str_sub(links$target,end = -10),nodes$name)-1 
links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(paste0(links$source,'_new_year'), nodes$name)-1
match(str_sub(links$target,end = -10),nodes$name)-1 
links$IDtarget

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=TRUE, fontSize = 12, nodeWidth = 30)
p



## GGanimate - IT WONNT WORK FOR SOME REASON

year <- data.frame(year = c(1990,2000,2010,2015))
merged <- merge(world,year)

merged <- left_join(merged, forest, by = c('year',"region"))

chart <- 
    merged %>%
    ggplot() +
    
    #Adding world map as polygon
    geom_polygon(data = merged, 
                 aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = net_forest_conversion_f), 
                 color = "black", 
                 size = .1) +
    
    # formatting labs so that nothing is shown but the title
    
    
    # colouring and 
    scale_fill_brewer(palette = 'PRGn', name = '',
                      labels = c("-400,000 hectares", 
                                 "-200,000 hectares",
                                 "-100,000 hectares",
                                 "0 hectares",
                                 "+100,000 hectares",
                                 "+200,000 hectares",
                                 "+400,000 hectares",
                                 "+600,000 hectares")) + 
    theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = unit(24, "mm")),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
    ) +
    transition_states(year, transition_length = 2, state_length = 2) + #setting animation properties
    ease_aes('cubic-in-out')


chart + transition_time(year) +labs(title = "Year: {frame_time}")

## GGiraph 

brazil_use<- brazil %>% select(!c(entity,code))
brazil_use <- melt(brazil_use, 'year')

brazil_use$tooltip <- c(paste0('Driver is: ', 
                               brazil_use$variable, 
                               " it's contribution to Brazil's deforestation is: ",
                               round(brazil_use$value/1000000,2),'mm in the year ',
                               brazil_use$year))

p1<- ggplot(data = brazil_use, aes(x = year, y = value, group = variable, tooltip = variable)) +
    geom_line_interactive(aes(color = variable, tooltip = variable, data_id = variable)) +
    geom_point_interactive(aes(color = variable, tooltip = tooltip, data_id = variable), size = 2) +
    scale_color_brewer(palette = 'PRGn') + labs(x = '', y = '') +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = unit(24, "mm")),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
p1

girafe(ggobj =  p1, 
       options = list(
           opts_hover_inv(css = "opacity:0.1;"), 
           opts_hover(css = "stroke-width:5;stroke:#2c7fb8;"), 
           opts_tooltip( 
               #offx = 120,
               #offy = 400, 
               use_cursor_pos = FALSE,
               opacity = 0.9)
       ))#, 
#width_svg = 8, height_svg = 8)
