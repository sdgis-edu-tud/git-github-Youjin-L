#used criteria for clustering: (1)distance to green (2)proximity from stream to points of interest (3)flood risk

#original value is normalised value for each criteria
  #(1)for distance to green, higher number means less distance to green from nearby stream unit
  #(2)for proximity from stream to points of interest, higher number means more points of interest nearby stream unit
  #(3)for flood risk, higher number means less flood risk nearby stream unit

#Step1
  #install and load packages
    #(remove # when install)
      #install.packages("sf")
      #install.packages("dplyr")
      library(sf) # for processing vector data
      library(dplyr) # for selecting and transforming data

  #load data
      getwd()
      units <- st_read("unit_normalised.gpkg", quiet = TRUE) #|unit name|<- st_read("|file name|", quiet = TRUE)

      head(units)  #view first few rows
      nrow(units)  #count number of units

      # Plot distance
        #plot(|unit name|["|column name|"], border = NA, main = "|map title|")
        plot(units["Udistance"], border = NA, main = "Connectivity to Green")

      # Plot POI
        plot(units["UPOI"], border = NA, main = "From stream to points of interests")

      # Plot Flood Risk
        plot(units["UFlood.Risk"],  border = NA, main = "Less Affected from Flood")

#Step2

  #select relevant features

        #/featurename/ <- /unitname/ |>select(/column1/,/column2/,/column3/)|>st_drop_geometry()
        features_y <- units |>
        select(Udistance, UPOI, UFlood.Risk) |>
        st_drop_geometry() # remove geometry column so we just keep a data table

  #standardize

        #/scaledfeaturename/ <- scale(/featurename/)
        X_scaled_y <- scale(features_y) # Standardize (mean=0, sd=1)

        head(X_scaled_y) #view first few rows

#Step3
      # Initialize an empty numeric vector to store inertia values
        #/setinertianame/ <- numeric()
        inertia_y <- numeric()

      # Try k values from 2 to 9
        #/setkvaluename/<-2:9
        k_values_y <- 2:9

      # Loop through each k value
        #for (k in /kvaluename/){/kmname/ <- kmeans (/scaledfeaturename/, centers=k, nstart=20)/inertianame/<-c(/inertianame/,/kmname/$tot.withinss)}
        for (k in k_values_y) {
          km_y <- kmeans(X_scaled_y, centers = k, nstart = 20)
          inertia_y <- c(inertia_y, km_y$tot.withinss)}

          # tot.withinss is Total within-cluster sum of squares
          # This measures how compact the clusters are: lower is better.

      # Combine the results into a data frame for plotting
        #/resultsname/ <- data.frame (k=/kvaluename/, inertia=/inertianame/)
        elbow_df_y <- data.frame(k = k_values_y, inertia = inertia_y)

        print(elbow_df_y)

      # Make the elbow plot
        plot(k_values_y, inertia_y,
             type = "b",                  # shown both points + lines
             col = "darkblue",
             main = "Elbow Method")

#Step4
      # `set.seed()` sets the random number generator to a fixed state
      # Set the seed so the clustering result is always the same when re-run
        set.seed(0)  # The number 0 is just a fixed choice. You can also use 10, 345, etc.

      # Choose the number of clusters based on the elbow plot
        k <- 4

      # Run K-means clustering on the standardized data
        kmeans_result_y <- kmeans(X_scaled_y, centers = k, nstart = 20)

      # Add the cluster labels to the spatial data
        units$cluster <- as.factor(kmeans_result_y$cluster) # The result kmeans_result$cluster is a list of cluster labels (1 to 4), in the same order as the original rows in X_scaled and grids

        head(units) #view first few rows
        print(table(units$cluster)) # Show how many grids fall into each cluster

        st_write(units, "units_normalised_cluster.gpkg") #save as file

      # Plot clusters with base R
        plot(units["cluster"],
             main = "Spatial Pattern of Urban Stream Clusters",
             border = NA)
#Step5
      # Get the cluster centers (in standardized form)
        scaled_centers_y <- kmeans_result_y$centers

        # Print them
        print("Cluster centers (standardized):")
        print(scaled_centers_y)

        # Convert the centers back to original scale: x * SD + mean
        original_centers_y <- t(apply(
          scaled_centers_y, 1,
          function(x) x * attr(X_scaled_y, "scaled:scale") + attr(X_scaled_y, "scaled:center")
        ))

        # Print the real-world values
        print("Cluster centers (original):")
        print(original_centers_y)

#Step 6

