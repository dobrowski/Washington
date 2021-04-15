

mutate(lacrt = if_else(lacrt == 0, litcrt, lacrt),
       lagrades = if_else(lagrades == 0, litgrades, lagrades),
       lawrite = if_else(lawrite == 0, writetest, lawrite)
)




### Load Libraries -----

library(tidyverse)
library(here)
library(readxl)
library(ggthemes)
library(janitor)
library(ggrepel)
library(scales)
library(htmlwidgets)
library(DT)
library(plotly)
library(viridis)
library(glue)

###  Load data ----


scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d


loc <- "testClass"  #"ClassHTMLfiles" 
loc2 <- "testStudent"  #"StudentGraphsWrite"


dir.create(here(loc))

dir.create(here(loc2))

slash <- "/"

all.data <- read_xls(here("data","mmdata","MM 18-19" , "toro tri1.xls")) %>%
    mutate(trimester = "1819-1")


all.data <- all.data %>%
    filter(trimester == "xxxx")

for( k in c(#"15-16","16-17","17-18","18-19", "19-20",
            "20-21"))
for( j in c("toro","wus","sb"))
for( i in 1:3){
    
    skip_to_next <- FALSE #
    
    tryCatch({
    
    
    temp <- read_xls(here("data","mmdata",paste0("MM ",k)  , paste0(j," tri",i,".xls"))) %>%
        mutate(trimester = paste0(k,"-",i)  )    #readline(prompt = "Enter year and trimester (e.g. 1819-1): "))
    
print(paste(k,j,i))
    
        
    all.data <- all.data %>%
        bind_rows(temp)
    }, error=function(e){
        cat("ERROR :",conditionMessage(e), "\n")
        skip_to_next <<- TRUE  #
        })
    
    if(skip_to_next) { next }     #
    
}



all.data <- all.data %>%
    mutate_at(vars(ends_with("pl")), list( ~na_if(.,0)) ) %>% 
    mutate(teacher.last = sub( ",.*", "", teacher ),
           student = paste0(sub( ".*,", "", stuname )," ",  sub( ",.*", "", stuname ))) %>%
  
  mutate(lacrt = if_else(lacrt == 0, litcrt, lacrt),
         lagrades = if_else(lagrades == 0, litgrades, lagrades),
         lawrite = if_else(lawrite == 0, writetest, lawrite)
  )

###  Student Performance Levels  Over time ----

# Run for every student once a trimester.  Save all the graphs and put them up on shared drive.  

#  Redo for all students ;  Get them structured into current teachers

current <- all.data %>%
    filter(trimester == max(trimester)) %>%
    select(stuid, student, cougrade, teacher.last) 
    
graph.history <- function(stud){
    
all.data %>%
    filter(stuid == stud) %>%
    select(trimester, ends_with("pl")) %>%
    gather(key = "key", value = "value", -trimester) %>%
        ggplot( aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
    geom_line() + 
    theme_hc()  +
    geom_text_repel(data = graphthis %>% filter(trimester == max(trimester)) ,
                    aes(label = key) , 
                    hjust = "right", 
                    segment.size = .2,
                    segment.color = "grey",
                    fontface = "bold", 
                    size = 3, 
                    nudge_x = .5, 
                    direction = "y") +
    geom_label(aes(label = value), 
               size = 2.5, 
               label.padding = unit(0.05, "lines"), 
               label.size = 0.0) +
    theme(legend.position = "none") +
    ylim(0,4) + 
    labs(title = paste0("Performance Levels over time for Student ", stud),
         x = "Trimester",
         y= "Performance Level")



} 

# graph.history("3919")


current.nest <- current %>%
    mutate(student.id = stuid) %>%
    group_by(student) %>%
    nest() %>%
    mutate(graphthese = data %>%
               map(~ all.data %>%
                       filter(stuid == .x$stuid) %>%
                       select(trimester, ends_with("pl")) %>%
                       gather(key = "key", value = "value", -trimester)),
            real.graph = graphthese %>%
               map2(student,~ ggplot(data = .x, aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
                         geom_line() + 
                         theme_hc()  +
                         geom_text_repel(data = .x %>% filter(trimester == max(trimester)) ,
                                         aes(label = key) , 
                                         hjust = "right", 
                                         segment.size = .2,
                                         segment.color = "grey",
                                         fontface = "bold", 
                                         size = 3, 
                                         nudge_x = .5, 
                                         direction = "y") +
                         geom_label(aes(label = value), 
                                    size = 2.5, 
                                    label.padding = unit(0.05, "lines"), 
                                    label.size = 0.0) +
                         theme(legend.position = "none") +
                         ylim(0,4) + 
                         labs(title = paste0("Performance Levels over time for ", .y),
                              x = "Trimester",
                              y= "Performance Level")
               )
    ) %>%
    unnest( cols = c(data)) %>%
    mutate(stufile = paste0( cougrade, slash,  teacher.last, slash,    student, ".png"))  # to create path and filename




dir.create(here(loc2))
# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc2, .x)))
# Makes the teacher folders
walk2(current.nest$cougrade, current.nest$teacher.last, ~ dir.create(here(loc2, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest$stufile, current.nest$real.graph , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 


######  Now for district write  srclacrt  srcmathcrt  srclawrite  -----


graph.history.write <- function(stud){
    
    all.data %>%
        filter(stuid == stud) %>%
        select(trimester, srclawrite ) %>%
        gather(key = "key", value = "value", -trimester) %>%
        ggplot( aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
        geom_line() + 
        theme_hc()  +
        geom_text_repel(data = graphthis %>% filter(trimester == max(trimester)) ,
                        aes(label = key) , 
                        hjust = "right", 
                        segment.size = .2,
                        segment.color = "grey",
                        fontface = "bold", 
                        size = 3, 
                        nudge_x = .5, 
                        direction = "y") +
        geom_label(aes(label = value), 
                   size = 2.5, 
                   label.padding = unit(0.05, "lines"), 
                   label.size = 0.0) +
        theme(legend.position = "none") +
        ylim(0,4) + 
        labs(title = paste0("Performance Levels over time for Student ", stud),
             x = "Trimester",
             y= "Performance Level")
    
    
    
} 

# graph.history.write("3919")


current.nest.write <- current %>%
    mutate(student.id = stuid) %>%
    group_by(student) %>%
    nest() %>%
    mutate(graphthese = data %>%
               map(~ all.data %>%
                       filter(stuid == .x$stuid) %>%
                       select(trimester, srclawrite) %>%
                       mutate(srclawrite = as.numeric(srclawrite)) %>%
                       gather(key = "key", value = "value", -trimester)),
           real.graph = graphthese %>%
               map2(student,~ ggplot(data = .x, aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
                        geom_line() + 
                        theme_hc()  +
                        geom_text_repel(data = .x %>% filter(trimester == max(trimester)) ,
                                        aes(label = key) , 
                                        hjust = "right", 
                                        segment.size = .2,
                                        segment.color = "grey",
                                        fontface = "bold", 
                                        size = 3, 
                                        nudge_x = .5, 
                                        direction = "y") +
                        geom_label(aes(label = value), 
                                   size = 2.5, 
                                   label.padding = unit(0.05, "lines"), 
                                   label.size = 0.0) +
                        theme(legend.position = "none") +
                        ylim(0,4) + 
                        labs(title = paste0("District Write over time for ", .y),
                             x = "Trimester",
                             y= "Performance Level")
               )
    ) %>%
    unnest( cols = c(data)) %>%
    mutate(stufile = paste0( cougrade, slash,  teacher.last, slash,    student, "District Write", ".png"))  # to create path and filename




dir.create(here(loc2))
# Makes the grade level folders
walk(current.nest.write$cougrade, ~ dir.create(here(loc2, .x)))
# Makes the teacher folders
walk2(current.nest.write$cougrade, current.nest.write$teacher.last, ~ dir.create(here(loc2, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest.write$stufile, current.nest.write$real.graph , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 




###### Simplifying District Write code and expanding to other requested measures -----

testplot <- function(data1, student1){ 
  data1 %>% map2(student1,~ ggplot(data = .x, aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
                                    geom_line() + 
                                    theme_hc()  +
                                    geom_text_repel(data = .x %>% filter(trimester == max(trimester)) ,
                                                    aes(label = key) , 
                                                    hjust = "right", 
                                                    segment.size = .2,
                                                    segment.color = "grey",
                                                    fontface = "bold", 
                                                    size = 3, 
                                                    nudge_x = .5, 
                                                    direction = "y") +
                                    geom_label(aes(label = value), 
                                               size = 2.5, 
                                               label.padding = unit(0.05, "lines"), 
                                               label.size = 0.0) +
                                    theme(legend.position = "none") +
                                    ylim(0,NA) + 
                                    labs(title = glue("{.x$key[1]} over time for {.y}"),
                                         x = "Trimester",
                                         y= "Performance Level")
)
}



current.nest.write <- current %>%
    mutate(student.id = stuid) %>%
    group_by(student) %>%
    nest() %>%
    mutate(graphthese.write = data %>%
               map(~ all.data %>%
                       filter(stuid == .x$stuid) %>%
                       select(trimester, srclawrite) %>%
                       mutate(srclawrite = as.numeric(srclawrite)) %>%
                       gather(key = "key", value = "value", -trimester)),
           real.graph.write = graphthese.write %>%   testplot(student),
           
           graphthese.lacart = data %>%
               map(~ all.data %>%
                       filter(stuid == .x$stuid) %>%
                       select(trimester, srclacrt) %>%
                       mutate(srclacrt = as.numeric(srclacrt)) %>%
                       gather(key = "key", value = "value", -trimester)),
           real.graph.lacrt = graphthese.lacart %>%  testplot(student),

           graphthese.mathcart = data %>%
               map(~ all.data %>%
                       filter(stuid == .x$stuid) %>%
                       select(trimester, srcmathcrt) %>%
                       mutate(srcmathcrt = as.numeric(srcmathcrt)) %>%
                       gather(key = "key", value = "value", -trimester)),
           real.graph.mathcrt = graphthese.mathcart %>%  testplot(student)

    ) %>%
    unnest( cols = c(data)) %>%
    mutate(stufile.write = paste0( cougrade, slash,  teacher.last, slash,    student, " - District Write", ".png"))  %>% # to create path and filename
mutate(stufile.lacrt = paste0( cougrade, slash,  teacher.last, slash,    student, " - LA CRT", ".png"))  %>%  # to create path and filename
mutate(stufile.mathcrt = paste0( cougrade, slash,  teacher.last, slash,    student, " - Math CRT", ".png"))  # to create path and filename




dir.create(here(loc2))
# Makes the grade level folders
walk(current.nest.write$cougrade, ~ dir.create(here(loc2, .x)))
# Makes the teacher folders
walk2(current.nest.write$cougrade, current.nest.write$teacher.last, ~ dir.create(here(loc2, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest.write$stufile.write, current.nest.write$real.graph.write , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 
walk2(current.nest.write$stufile.lacrt, current.nest.write$real.graph.lacrt , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 
walk2(current.nest.write$stufile.mathcrt, current.nest.write$real.graph.mathcrt , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 


#####   Test multiple columns -------


testplot_group <- function(data1, student1){ 
  data1 %>% map2(student1,~ ggplot(data = .x, aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
                   geom_line() + 
                   theme_hc()  +
                   geom_text_repel(data = .x %>% filter(trimester == max(trimester)) ,
                                   aes(label = key) ,
                                   hjust = "right",
                                   segment.size = .2,
                                   segment.color = "grey",
                                   fontface = "bold",
                                   size = 3,
                                   nudge_x = .5,
                                   direction = "y") +
                   geom_label(aes(label = value),
                              size = 2.5,
                              label.padding = unit(0.05, "lines"),
                              label.size = 0.0) +
                   theme(legend.position = "none") +
                   ylim(0,4) + # ylim(0,NA) +
                   labs(title = glue("{.y}"),#glue("{.x$key[1]} over time for {.y}"),
                        x = "Trimester",
                        y= "Performance Level")
  )
}


current.nest.multi <- current %>%
  mutate(student.id = stuid) %>%
  group_by(student) %>%
  nest() %>%
  mutate(graphthese.multi = data %>%
           map(~ all.data %>%
                 filter(stuid == .x$stuid) %>%
                 select(trimester, 
                        lacrt,
                        lagrades,
                        lawrite ,
                        mathcrt ,
                        mathgrades ,
                        writetest
                        ) %>%
                 mutate(across(.cols = -trimester, .fns =  as.numeric)) %>%
                 gather(key = "key", value = "value", -trimester)),
         real.graph.multi = graphthese.multi %>%   testplot_group(student),
         

  ) %>%
  unnest( cols = c(data)) %>%
  # mutate(stufile.write = paste0( cougrade, slash,  teacher.last, slash,    student, " - District Write", ".png"))  %>% # to create path and filename
  # mutate(stufile.lacrt = paste0( cougrade, slash,  teacher.last, slash,    student, " - LA CRT", ".png"))  %>%  # to create path and filename
  mutate(stufile.multi = paste0( cougrade, slash,  teacher.last, slash,    student, " - Multi", ".png"))  # to create path and filename




dir.create(here(loc2))
# Makes the grade level folders
walk(current.nest.multi$cougrade, ~ dir.create(here(loc2, .x)))
# Makes the teacher folders
walk2(current.nest.multi$cougrade, current.nest.multi$teacher.last, ~ dir.create(here(loc2, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest.multi$stufile.multi, current.nest.multi$real.graph.multi , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 
#walk2(current.nest.write$stufile.lacrt, current.nest.write$real.graph.lacrt , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 












#### History by cohort ---------

cohort <- all.data %>% 
    mutate(grade.numeric = if_else(cougrade == "KN",0,as.numeric(cougrade)),
           trimester.numeric =  sub( "-.*", "", trimester ) %>% as.numeric(),
           cohort.class =  trimester.numeric - grade.numeric ,
           cohort.class2 = cohort.class) # take first two digits from trimester, subtract the grade level (with KN changed to 0) 

cohort.copy <- cohort

# cohort2 <- cohort %>%
#     filter(cohort.class == 15) %>%
#     select(trimester, ends_with("pl")) %>%
#     gather(key = "key", value = "value", -trimester) %>%
#     group_by(trimester,key) %>%
#     transmute(cohort.average = mean(value, na.rm = TRUE)) %>%
#     distinct() %>%
#     ungroup()
# 
#  ggplot(cohort2, aes(x = trimester, y = cohort.average, group = key, color = key, label = cohort.average), size = 1) +
#     geom_line() + 
#     theme_hc()  +
#     geom_text_repel(data = cohort2 %>% filter(trimester == max(trimester)) ,
#                     aes(label = key) , 
#                     hjust = "right", 
#                     segment.size = .2,
#                     segment.color = "grey",
#                     fontface = "bold", 
#                     size = 3, 
#                     nudge_x = .5, 
#                     direction = "y") +
#     geom_label(aes(label = round(cohort.average,2)), 
#                size = 2.5, 
#                label.padding = unit(0.05, "lines"), 
#                label.size = 0.0) +
#     theme(legend.position = "none") +
#     ylim(0,4) + 
#     labs(title = paste0("Performance Levels over time  "),
#          x = "Trimester",
#          y= "Performance Level")
# 


class.to.cohort <- cohort %>%
    select(trimester, cohort.class2, cougrade) %>%
    group_by(cohort.class2) %>%
    filter(trimester == max(trimester)) %>%
    distinct()

cohort.nest <- cohort  %>%
    group_by(cohort.class2) %>%
    nest() %>%
    mutate(graphthese = data %>%
               map(~ cohort.copy %>% 
                       filter(cohort.class == .x$cohort.class ) %>% 
                       select(trimester, ends_with("pl"))   %>%
                       gather(key = "key", value = "value", -trimester) %>%
                       group_by(trimester,key) %>%
                       transmute(cohort.average = mean(value, na.rm = TRUE)) %>%
                       distinct() %>%
                       ungroup()),
           real.graph = graphthese %>%
               map2(cohort.class2 ,~  ggplot(.x, aes(x = trimester, y = cohort.average, group = key, color = key, label = cohort.average), size = 1) +
                       geom_line() + 
                       theme_hc()  +
                       geom_text_repel(data = .x %>% filter(trimester == max(trimester)) ,
                                       aes(label = key) ,
                                       hjust = "right",
                                       segment.size = .2,
                                       segment.color = "grey",
                                       fontface = "bold",
                                       size = 3,
                                       nudge_x = .5,
                                       direction = "y") +
                       geom_label(aes(label = round(cohort.average,2)),
                                  size = 2.5,
                                  label.padding = unit(0.05, "lines"),
                                  label.size = 0.0) +
                       theme(legend.position = "none") +
                       ylim(0,4) +
                       labs(title = paste0("Performance Levels over time for Class of 20", .y + 12, "-",.y + 13),
                            x = "Trimester",
                            y= "Performance Level")
               )
    ) %>% 
    left_join(class.to.cohort) %>% 
    mutate(filename = paste0( cougrade, slash,  "Performance Levels over time for Class of 20", cohort.class2 + 12, "-",cohort.class2 + 13, ".png"))


dir.create(here(loc2))
# Makes the grade level folders
walk(cohort.nest$cougrade, ~ dir.create(here(loc2, .x)))
#  Saves the graphs in the current teachers folder
walk2(cohort.nest$filename, cohort.nest$real.graph , ~ggsave(filename = here(loc2, .x), plot = .y, height = 7, width = 7) ) 





###  Big change between Trimester -----

#  This shows the student ID of all students that moved more than 1 point on an PL in the last trimester

# # Need to show which direction + / - 
# test <- all.data %>% 
#     filter(trimester %in% tail(unique(all.data$trimester), 2)) %>%
#     group_by(stuid) %>%
#     mutate(la.range = range(lapl)[2] - range(lapl)[1],
#            math.range = range(mathpl)[2] - range(mathpl)[1],
#            sci.range = range(scipl)[2] - range(scipl)[1],
#            ss.range = range(sspl)[2] - range(sspl)[1]) %>% 
#     select(stuid, stuname, teacher, ends_with("range")) %>%
#     gather(-stuid, -stuname, -teacher ,key = "PL", value = "value") %>%
#     filter(value >= 1) %>%
#     distinct() %>%
#     arrange(teacher, stuname)


teacher.match <- all.data %>% 
    filter(trimester == max(trimester)) %>%
    select(student, teacher.last, cougrade)

bigchange <- all.data %>% 
    filter(trimester %in% tail(unique(all.data$trimester), 2)) %>%
    group_by(student) %>%
    mutate(tri = case_when( 
        trimester == tail(unique(all.data$trimester), 2)[2] ~ "new" ,
        trimester == tail(unique(all.data$trimester), 2)[1] ~ "old" )
        ) %>%
#    select(student, teacher.last, cougrade, tri, ends_with("pl")) %>%
    select(student,  tri, ends_with("pl")) %>%
#    gather(key = "key", value = "value", -tri, -cougrade, -teacher.last,  -student ) %>%
    gather(key = "key", value = "value", -tri,   -student ) %>%
        pivot_wider(names_from = c(tri,key), values_from = value) %>%
    mutate(change_lapl =  new_lapl - old_lapl ,
           change_mathpl = new_mathpl - old_mathpl ,
           change_scipl = new_scipl - old_scipl ,
           change_sspl = new_sspl - old_sspl ) %>%
    select(student, starts_with("change")) %>%
    pivot_longer(cols = starts_with("change"), names_to = "pl", values_to = "change") %>% 
    filter(change >=1 | change <= -1) %>%
    left_join(teacher.match) %>%
    arrange(cougrade, teacher.last, desc(change), student)

bigchange.nest <- bigchange %>% 
    group_by(cougrade) %>%
    nest() %>% 
    mutate(dt = data %>%
               map(~ datatable(.x, options = list(pageLength = 20)))) %>%
    mutate(filename = paste0( cougrade, slash,  "1 Big Changes for " , cougrade, ".html"))




dir.create(here(loc))
# Makes the grade level folders
walk(bigchange.nest$cougrade, ~ dir.create(here(loc, .x)))
#  Saves the graphs in the current teachers folder
walk2(bigchange.nest$filename, bigchange.nest$dt , ~saveWidget(widget = .y, file = here(loc, .x), selfcontained = TRUE ) ) 



# teacher
bigchange.nest <- bigchange %>% 
    group_by(cougrade, teacher.last) %>%
    nest() %>% 
    mutate(dt = data %>%
               map(~ datatable(.x, options = list(pageLength = 20)))) %>%
    mutate(filename = paste0( cougrade, slash, teacher.last, slash,  "1 Big Changes for " , teacher.last, ".html"))


dir.create(here(loc))
# Makes the grade level folders
walk(bigchange.nest$cougrade, ~ dir.create(here(loc, .x)))
# Makes the teacher folders
walk2(bigchange.nest$cougrade, bigchange.nest$teacher.last, ~ dir.create(here(loc, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(bigchange.nest$filename, bigchange.nest$dt , ~saveWidget(widget = .y, file = here(loc, .x), selfcontained = TRUE ) ) 



# 
# dt <- datatable(bigchange, options = list(pageLength = 20))
# 
# saveWidget(widget = dt, file = "df.html")
# 


### Top students by grade ----


# by teacher
current.nest <- all.data %>%
    filter(trimester == max(trimester) ) %>%
    select(cougrade, teacher.last, student, ends_with("pl")) %>%
    group_by(cougrade, teacher.last) %>%
    nest() %>%
    mutate(dt = data %>%
               map(~ datatable(.x, options = list(pageLength = 30)))) %>%
    mutate(filename = paste0( cougrade, slash, teacher.last, slash,  "0 Latest Trimester for " , teacher.last, ".html"))



dir.create(here(loc))

# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
# Makes the teacher folders
walk2(current.nest$cougrade, current.nest$teacher.last, ~ dir.create(here(loc, .x, .y)))
#  Saves the tables in the current teachers folder
walk2(current.nest$filename, current.nest$dt,   ~saveWidget(widget = .y, file = here(loc, .x), selfcontained = TRUE ) )  


# by grade
current.nest <- all.data %>%
    filter(trimester == max(trimester) ) %>%
    select(cougrade, teacher.last, student, ends_with("pl")) %>%
    group_by(cougrade) %>%
    nest() %>%
    mutate(dt = data %>%
               map(~ datatable(.x, options = list(pageLength = 30)))) %>%
    mutate(filename = paste0( cougrade, slash,  "0 Latest Trimester for " , cougrade, ".html"))



dir.create(here(loc))

# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
#  Saves the tables in the current teachers folder
walk2(current.nest$filename, current.nest$dt,   ~saveWidget(widget = .y, file = here(loc, .x), selfcontained = TRUE ) )  





### Distibution by grade ----

#hoverover to see who in in that bar 
# 
# p <- all.data %>%
#     filter(trimester == max(trimester),
#            cougrade == "08") %>%
#     ggplot() +
#     geom_histogram(aes( x = mathpl, text = student)) +
#     scale_y_continuous(breaks= pretty_breaks()) +
#     theme_hc()
# 
# ggplotly(p, tooltip = "text" )
# 
# saveWidget(widget = ggplotly(p), file = "test.html")

# 

### Distibution by teacher ----

# #  For a single teacher
# 
# teach <- "Frazier"
# 
# all.data %>% 
#     filter(trimester == max(trimester),
#            str_detect(teacher, teach)) %>%
#     ggplot() + 
#     geom_histogram(aes( x = mathpl)) +
#     scale_y_continuous(breaks= pretty_breaks()) +
#     theme_hc()



#  For distribution for all teachers.  

current <- all.data %>%
    filter(trimester == max(trimester)) %>%
    select(stuid, student, cougrade, teacher.last, ends_with("pl")) 


current.nest <- current %>%
    mutate(student.id = stuid) %>%
    group_by(cougrade, teacher.last) %>%
    nest() %>%
    mutate(lapl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                       geom_histogram(aes( x = .x$lapl, text = student)) +
                       scale_y_continuous(breaks= pretty_breaks()) +
                       theme_hc() +
                       labs(x = "Language Arts PL Score",
                            y = "Number of students",
                            title = paste0("Students by Language Arts PL Score for ", .y )) ),    
           mathpl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$mathpl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Math PL Score",
                             y = "Number of students",
                             title = paste0("Students by Math PL Score for ", .y )) ),  
           scipl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$scipl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Science PL Score",
                             y = "Number of students",
                             title = paste0("Students by Science PL Score for ", .y )) ),  
           sspl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$sspl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Social Studies PL Score",
                             y = "Number of students",
                             title = paste0("Students by Social Studies PL Score for ", .y )) ),  
           ) %>%
    mutate(lafile = paste0( cougrade, slash,  teacher.last, slash, "1 LA Distribution for " , teacher.last, ".png"),
           mathfile = paste0( cougrade, slash,  teacher.last, slash, "2 Math Distribution for " , teacher.last, ".png"),
           scifile = paste0( cougrade, slash,  teacher.last, slash, "3 Science Distribution for " , teacher.last, ".png"),
           ssfile = paste0( cougrade, slash,  teacher.last, slash, "4 Social Studies Distribution for " , teacher.last, ".png")) %>%
   mutate(lafileh = paste0( cougrade, slash,  teacher.last, slash, "1 LA Distribution for " , teacher.last, ".html"),
          mathfileh = paste0( cougrade, slash,  teacher.last, slash, "2 Math Distribution for " , teacher.last, ".html"),
          scifileh = paste0( cougrade, slash,  teacher.last, slash, "3 Science Distribution for " , teacher.last, ".html"),
          ssfileh = paste0( cougrade, slash,  teacher.last, slash, "4 Social Studies Distribution for " , teacher.last, ".html"))  # to create path and filename



dir.create(here(loc))
# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
# Makes the teacher folders
walk2(current.nest$cougrade, current.nest$teacher.last, ~ dir.create(here(loc, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest$lafileh, current.nest$lapl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$mathfileh, current.nest$mathpl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$scifileh, current.nest$scipl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$ssfileh, current.nest$sspl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 

# walk2(current.nest$lafile, current.nest$lapl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 
# walk2(current.nest$mathfile, current.nest$mathpl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 
# walk2(current.nest$scifile, current.nest$scipl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 
# walk2(current.nest$ssfile, current.nest$sspl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 





# by grade
current.nest <- current %>%
    mutate(student.id = stuid) %>%
    group_by(cougrade) %>%
    nest() %>%
    mutate(lapl.graph = data %>%
               map2(cougrade ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$lapl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Language Arts PL Score",
                             y = "Number of students",
                             title = paste0("Students by Language Arts PL Score for ", .y )) ),    
           mathpl.graph = data %>%
               map2(cougrade ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$mathpl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Math PL Score",
                             y = "Number of students",
                             title = paste0("Students by Math PL Score for ", .y )) ),  
           scipl.graph = data %>%
               map2(cougrade ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$scipl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Science PL Score",
                             y = "Number of students",
                             title = paste0("Students by Science PL Score for ", .y )) ),  
           sspl.graph = data %>%
               map2(cougrade ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$sspl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Social Studies PL Score",
                             y = "Number of students",
                             title = paste0("Students by Social Studies PL Score for ", .y )) ),  
    ) %>%
    mutate(lafileh = paste0( cougrade, slash,   "1 LA Distribution for " , cougrade, ".html"),
           mathfileh = paste0( cougrade, slash,  "2 Math Distribution for " , cougrade, ".html"),
           scifileh = paste0( cougrade, slash,  "3 Science Distribution for " , cougrade, ".html"),
           ssfileh = paste0( cougrade, slash, "4 Social Studies Distribution for " , cougrade, ".html"))  # to create path and filename



dir.create(here(loc))
# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
#  Saves the graphs in the current teachers folder
walk2(current.nest$lafileh, current.nest$lapl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$mathfileh, current.nest$mathpl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$scifileh, current.nest$scipl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$ssfileh, current.nest$sspl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 




#### End -----
