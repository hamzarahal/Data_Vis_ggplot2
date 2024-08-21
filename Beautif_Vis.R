######################Beautifying Visualizations##################################
###################################################################################

### Modifying the Background


# Load the dataset 
library(tidyverse)
college <- read_csv('http://672258.youcanlearnit.net/college.csv')
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the bar graph from Video 2.3
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))

# Change the plot background color
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(plot.background=element_rect(fill='purple'))

# Change the panel background color
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_rect(fill='purple'))

# Let's be minimalist and make both backgrounds disappear
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank())

# Add grey gridlines
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  theme(panel.grid.major=element_line(color="grey"))

# Only show the y-axis gridlines
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  theme(panel.grid.major.y=element_line(color="grey"))

### Modifying Axes

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the bar graph from Video 3.1
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank())

# Rename the axes
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  xlab("Region") +
  ylab("Number of Schools")

# Resize the y-axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  xlab("Region") +
  ylab("Number of Schools") +
  ylim(0,500)

# Be careful - using the ylim function is like zooming
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  xlab("Region") +
  ylab("Number of Schools") +
  ylim(50,500) 

### Modifying Scales

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the bar graph 
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank())

# Change the name of x-axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region")

# Change the name and limits of the y-axis
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500))

# Change the fill colors
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"))

### Modifying Legends

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the bar graph 
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"))

# Change the legend title
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide_legend(title="Institution Type"))

# Adjust the legend formatting
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide=guide_legend(title="Institution Type", label.position="bottom", nrow=1, keywidth=2.5))

# Move the legend to the bottom of the plot
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide=guide_legend(title="Institution Type", label.position="bottom", nrow=1, keywidth=2.5)) +
  theme(legend.position="bottom")

# Move the legend to the top of the plot
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide=guide_legend(title="Institution Type", label.position="bottom", nrow=1, keywidth=2.5)) +
  theme(legend.position="top")

### Annotating Plots

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the scatterplot 
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2) 

# Add a text annotation
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2) +
  annotate("text", label="Elite Privates", x=45000,y=1500)

# Add a line for the mean SAT score
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2) +
  annotate("text", label="Elite Privates", x=45000,y=1500) +
  geom_hline(yintercept=mean(college$sat_avg))

# And label it
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2) +
  annotate("text", label="Elite Privates", x=45000,y=1500) +
  geom_hline(yintercept=mean(college$sat_avg)) +
  annotate("text", label="Mean SAT", x=47500, y=mean(college$sat_avg)-15)

# Add a line for mean tuition
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2) +
  annotate("text", label="Elite Privates", x=45000,y=1500) +
  geom_hline(yintercept=mean(college$sat_avg)) +
  annotate("text", label="Mean SAT", x=47500, y=mean(college$sat_avg)-15) +
  geom_vline(xintercept=mean(college$tuition)) +
  annotate("text", label="Mean Tuition", x=mean(college$tuition)+7500, y=700)

# And let's tidy this up a bit more
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg, color=control, size=undergrads), alpha=1/2) +
  annotate("text", label="Elite Privates", x=45000,y=1500) +
  geom_hline(yintercept=mean(college$sat_avg), color="dark grey") +
  annotate("text", label="Mean SAT", x=47500, y=mean(college$sat_avg)-15) +
  geom_vline(xintercept=mean(college$tuition), color="dark grey") +
  annotate("text", label="Mean Tuition", x=mean(college$tuition)+7500, y=700) +
  theme(panel.background = element_blank(), legend.key = element_blank()) +
  scale_color_discrete(name="Institution Type") +
  scale_size_continuous(name="Undergraduates") +
  scale_x_continuous(name="Tuition") +
  scale_y_continuous(name="SAT Scores")

# Titles

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the bar graph 
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide=guide_legend(title="Institution Type", label.position="bottom", nrow=1, keywidth=2.5)) +
  theme(legend.position="bottom")

# Add a title
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide=guide_legend(title="Institution Type", label.position="bottom", nrow=1, keywidth=2.5)) +
  theme(legend.position="bottom") +
  ggtitle("More colleges are in the southern U.S. than any other region.")

# Add a subtitle
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme(panel.background=element_blank()) +
  theme(plot.background=element_blank()) +
  scale_x_discrete(name="Region") +
  scale_y_continuous(name="Number of Schools", limits=c(0,500)) +
  scale_fill_manual(values=c("orange","blue"), guide=guide_legend(title="Institution Type", label.position="bottom", nrow=1, keywidth=2.5)) +
  theme(legend.position="bottom") +
  ggtitle("More colleges are in the southern U.S. than any other region.", subtitle="Source: U.S. Department of Education")

### Themes

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Create the bar graph from Video 2.3
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control))

# Try some different themes
ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_bw()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_minimal()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_void()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_dark()

# Check out ggthemes
install.packages("ggthemes")
library(ggthemes)

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_solarized()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_excel()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_wsj()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_economist()

ggplot(data=college) +
  geom_bar(mapping=aes(x=region, fill=control)) +
  theme_fivethirtyeight()