# Lab 2 Assignment: Loading data and the grammar of graphics (ggplot2)
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [40 points] Open the R file "Lab2_Script.R" comment each line of code with its purpose (with exception of Part 3)
# 2. [60 points] Open the R file "Lab2_Assignment.R" and answer the questions

#---- Q1. write the code to load the dataset "tract_covariates.csv" located under the "datasets" folder in your repository. Create an object called `opportunities` ----

opportunities <- read.csv("02_lab/datasets/tract_covariates.csv")


#---- Q2. Read and become familiar with the dataset metadata. Next write the code for the following:
# Link to metadata: https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-9.pdf 

# what is the object class?

class(opportunities)

# how can I know the variable names?

names(opportunities)

# What is the unit of analysis? 

head(opportunities)

View(opportunities)

# The functions head() or View() allow us to do a visual inspection to find out what is the Unit of Analysis.
# As you can see, the `state`, `county`,`cz`,`cz`, `tract` variables are ID variables, this is they allow to identify an observation. 
# The rest of the variables are measures of those ID variables.
# It is easy to see that out of the ID variables, `tract` is the only variables that change in each row. Hence `tract` is the variable of interest.


# Create a new object called `sa_opportunities` that only contains the rows for the San Antonio area (hint: use the `czname` variable). Save the resulting plot as a pdf with the name 'githubusername_p1.pdf'

sa_opportunities<-opportunities[opportunities$czname=="San Antonio",]

pdf("02_lab/plots/estebanlp_p1.pdf",height = 8.5, width = 11)
plot(sa_opportunities)
dev.off()

# Create a plot that shows the ranking of Annualized job growth rate (ann_avg_job_growth_2004_2013 variable) by census tract (tract variable). Save the resulting plot as a pdf with the name 'githubusername_p2.pdf'

library(ggplot2)

ggplot(data = sa_opportunities)+
  geom_col(aes(y=reorder(tract,ann_avg_job_growth_2004_2013),x=ann_avg_job_growth_2004_2013))
ggsave(filename = "02_lab/plots/estebanlp_p2.pdf")

# Create a plot that shows the relation between the `frac_coll_plus` and the `hhinc_mean2000` variables, what can you hypothesize from this relation? what is the causality direction? Save the resulting plot as a pdf with the name 'githubusername_p3.pdf'

ggplot(data = sa_opportunities, aes(x=frac_coll_plus2000,y=hhinc_mean2000)) +
  geom_point()+
  geom_smooth(method = 'loess')
ggsave(filename = "02_lab/plots/estebanlp_p3.pdf")

# Answer: The causality is likely to go higher college educated -> higher income. However this is not always true as you can see there is high variation around the mean.

# [Bonus: 10 extra points]: Investigate (on the internet) how to add a title,a subtitle and a caption to your last plot. Create a new plot with that and save it as 'githubusername_p_extra.pdf'

ggplot(data = sa_opportunities, aes(x=frac_coll_plus2000,y=hhinc_mean2000)) +
  geom_point()+
  geom_smooth(method = 'loess')+
  labs(title = "Do census tracts with more education also exhibit to higher Income levels?",subtitle = "San Antonio Communting Zone area - year 2000", y="% College Educated", x="Median Household Income",caption = "Data source: Opportunity Insights Project - Harvard University \nhttps://opportunityinsights.org/data/ ")
ggsave(filename = "02_lab/plots/estebanlp_p_extra.pdf")





