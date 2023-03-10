# my_directory <- file.path(your_file_path_here)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# First, we'll import ggplot2 for our visualizations.

library(ggplot2)

# Since we're interested in the hour of rental, let's first check how we're going to 
# approach using the Start.Time column values by verifying their datatype

print(paste("ny Start.Time datatype:", typeof(ny$Start.Time), sep=""))
print(paste("wash Start.Time datatype:", typeof(wash$Start.Time), sep=""))
print(paste("chi Start.Time datatype:", typeof(chi$Start.Time), sep=""))

# Since Start.Time is an integer for each dataframe, we can use the as.numeric, 
# format, and strptime functions to extract the hour values from the rental starting 
# times and create a new column for them in the respective dataframes.

ny$Start.Hour <- as.numeric(format(strptime(ny$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%H'))
wash$Start.Hour <- as.numeric(format(strptime(wash$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%H'))
chi$Start.Hour <- as.numeric(format(strptime(chi$Start.Time, format = '%Y-%m-%d %H:%M:%S'), '%H'))


# We want to facet wrap based on city when we examine rental starting hours, 
# so we'll create a new column to represent which city each row represents
# before using rbind to effectively merge the data from the three cities.

ny$City = 'ny'
wash$City = 'wash'
chi$City = 'chi'

# Now, we can use rbind to create a new dataframe that shows the start hour for each rental, 
# as well as which city that rental took place in. We'll create a variable column_names to
# hold these columns first to reduce redundancy.
column_names <- c('Start.Hour','City')
start_hour_df <- rbind(subset(ny, select = column_names),
                    subset(wash, select = column_names),
                    subset(chi, select = column_names))

# Using ggplot, we can create histograms of the rental starting hours, and facet-wrap
# based on city so we can make general comparisons on peak-hours between cities.  Since
# the 3 cities have vastly different amounts of data, we'll use ..density.. to get the 
# density/proportions of the rental counts

# First, we'll define our labels
plot_title <- "Density of Bike Rentals vs Time of Day"
xlabel <- "Time of Day"
ylabel <- "Density of Rentals"
plot_labels <- c(`chi` = "Chicago", `ny` = "New York City", `wash` = "Washington DC")

# Now, we can plot with ggplot
ggplot(aes(x = Start.Hour, y = ..density.. ), data = na.omit(start_hour_df)) +
    geom_histogram(binwidth = 6, boundary = 0, color = 'black', fill = '#00bfff') +
    scale_x_continuous(breaks = seq(3,21,6),labels=c("3"="Dawn", "9"="Morning","15"="Afternoon","21"="Evening")) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~City, labeller = as_labeller(plot_labels)) +
    theme(axis.text.x = element_text(size=8,angle=15), plot.title = element_text(hjust = .5)) +
    ggtitle(plot_title) +
    xlab(xlabel) +
    ylab(ylabel)

# We'll be forming a new dataframe to answer this question; this dataframe should
# initially contain the birth year and user type data from the cities of chi and ny,
# and then we can calculate a third column, age, based on the birth year before
# plotting the relationship between age and user type.


# merge ny and chi using rbind to make a new dataframe with columns Birth.Year, User.Type

column_names <- c("User.Type","Birth.Year")
age_and_user_df <- rbind(subset(ny, select = column_names),
                    subset(chi, select = column_names))


# Now, we can create a third column, "age", that is calculated by subtracting
# the birth year from 2017, the year the data is from. 

age_and_user_df$Age <- 2017 - age_and_user_df$Birth.Year


summary(age_and_user_df)

# Since the data also seemed to have empty entries for user type, 
# we'll set all of those to NA before plotting.

age_and_user_df[age_and_user_df == ""] <- NA

# We'll use qplot to create a simple boxplot that compares the distribution of 
# ages between the two user types.

# Once again, we'll make the labels first

chart_title <- "Distribution of User Ages from Rentals between different User Types"
xlabel <- "User Type"
# No need for ylabel or different plot labels, due to how the age column and the
# user type column's data are recorded.

qplot(x = User.Type, y = Age, data = na.omit(age_and_user_df),
      geom = 'boxplot') +
    ggtitle(chart_title) +
    xlab(xlabel) + 
    theme(plot.title = element_text(hjust = .5))
        

# We'll redo the boxplots, but now use a subset where the age is less than or equal
# to 75 years old. We'll also add increments on the y-axis so our analysis can be a
# little more clear.

qplot(x = User.Type, y = Age, data = subset(na.omit(age_and_user_df), Age <=85),
      geom = 'boxplot') +
    ggtitle(chart_title) +
    xlab(xlabel) + 
    theme(plot.title = element_text(hjust = .5)) +
    scale_y_continuous(breaks = seq(0, 90, 10))

# We'll be merging the 3 dataframes, and then creating a plot 
# based on travel time groupped by city.


# the 3 dataframes already have a City column, so we don't have 
# to worry about that before merging with rbind. We'll start with
# re-creating column_names to quickly grab the appriopriate data

column_names = c("Trip.Duration","City")
travel_time_df <- rbind(subset(ny, select = column_names),
                       subset(chi, select = column_names),
                       subset(wash, select = column_names))

# With our df, we can use qplot to create boxplots that show us the
# distribution of time spent travelling, in minutes.

# Once again, we create our titles/labels
chart_title <- "Bike Rental Durations by City"
ylabel <- "Trip Duration (Minutes)"
plot_labels <- c("Chicago","New York City","Washington DC")

qplot(x = City, y = Trip.Duration/60, data = na.omit(travel_time_df),
     geom = 'boxplot') +
    ggtitle(chart_title) +
    ylab(ylabel) + 
    theme(plot.title = element_text(hjust = .5)) +
    scale_x_discrete(labels = plot_labels)


# To estimate a good limit, we can use summary() to check one of the cities.
summary(chi$Trip.Duration/60)

# We'll take another look at the data, but limit the view so that
# analyses of median, Q1 and Q3 are easier.

qplot(x = City, y = Trip.Duration/60, data = na.omit(travel_time_df),
     geom = 'boxplot') +
    ggtitle(chart_title) +
    ylab(ylabel) + 
    theme(plot.title = element_text(hjust = .5)) +
    scale_x_discrete(labels = plot_labels) +
    coord_cartesian(ylim=c(0,30))
