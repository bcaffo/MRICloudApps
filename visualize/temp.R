dat = "~/sandboxes/MRIcloudT1Volumetrics/inst/extdata/kirby127a_3_1_ax_283Labels_M2_corrected_stats.txt" %>%
    readSubject() %>%
    subject2df() %>%
    addSubjectICV() %>%
    addSubjectTBV()

hierarchy = "~/sandboxes/MRIcloudT1Volumetrics/inst/extdata/multilevel_lookup_table.txt" %>%
    read.table(header = TRUE, 
               stringsAsFactors = FALSE, 
               na.strings = "xxxx",
               fill = TRUE) %>% 
    select(-modify.)

names(hierarchy) = c("index", "roi", 
                     "alevel4", "alevel3", "alevel2", "alevel1",
                     "blevel4", "blevel3", "blevel2", "blevel1")

## There as some blank lines and NAs where the whole row has to be 
## Deleted
hierarchy = hierarchy %>% 
    filter(!is.na(alevel3)) %>% 
    filter(alevel3 != "")

dat %>% filter(type == 1, level == 5) %>%
        left_join(hierarchy, by = "roi") %>%
    mutate(
        v1 = paste(alevel1, alevel2, alevel3, alevel4, roi, sep = "-"),
        v3 = volume / tbv * 100
    ) %>% 
    select(v1, v3) %>%
    sunburst()


smry = dat %>% filter(type == 1, level == 5) %>%
    left_join(hierarchy, by = "roi") %>%
    mutate(
        normalizedVolume = volume / tbv * 100
    ) 

treemap(smry, #Your data frame object
        index=c("alevel1","alevel2","alevel3","alevel4", "roi"),  #A list of your categorical variables
        vSize = "normalizedVolume",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Treemap of average", #Customize your title
        fontsize.title = 14 #Change the font size of the titles
)        


##    filter(alevel2 == "CerebralCortex_R") %>%
##    filter(alevel4 != "Insula_R") %>%
##    select(roi, meanComp, alevel4, alevel3) 

## Do the tree map

#pdf("treemap.pdf")
treemap(smry, #Your data frame object
        index=c("alevel3","alevel4", "roi"),  #A list of your categorical variables
        vSize = "meanComp",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Treemap of average composition", #Customize your title
        fontsize.title = 14 #Change the font size of the titles
)

smry = dat %>% filter(type == 1, level == 5) %>%
    left_join(hierarchy, by = "roi") %>%
    mutate(
        normalizedVolume = volume / tbv * 100
    ) %>%
    filter(!is.na(alevel1))

color_list = rep(brewer.pal(12, 'Set3'), 40)
#names(color_list) = unique(smry$roi)


D3partitionR() %>%
    add_data(smry,
             count = 'normalizedVolume',
             steps=c('alevel1','alevel2', 'alevel3', 'alevel4', 'roi'),
             tooltip = c('name', 'normalizedVolume'),
             aggregate_fun = list(normalizedVolume = sum)) %>%
#    set_chart_type('circle_treemap') %>%
    set_discrete_color_scale(color_list) %>%
#    add_title('Normalized volume as a percent') %>%
    set_legend_parameters(visible = FALSE) %>%
    plot()



## Selecting variables
var_names = c('Sex','Embarked','Pclass','Survived')

## Merging steps data and data with ages
data_plot = merge(titanic_data[,.N, by = c(var_names)], titanic_data[,.(mean_age=mean(Age,na.rm =TRUE), Survived_num=Survived), by=c(var_names)], by=var_names)

##Improving steps naming
data_plot[,(var_names):=lapply(var_names,function(x){data_plot[[x]]=paste0(x,' ',data_plot[[x]])
})]

D3partitionR()%>%
    add_data(data_plot,
             count = 'N',
             steps=c('Sex','Embarked','Pclass','Survived'),
             tooltip=c('name','N','mean_age'),
                label='name',
                color='Survived_num',
                aggregate_fun = list(mean_age=weighted.mean,
                                     Survived_num=weighted.mean)
                ) %>%
    set_chart_type('treemap') %>%
    set_labels_parameters(cut_off=10) %>%
    plot()

        
        
