devtools::install_github('delta1epsilon/BoxPacking')
library(BoxPacking)

source("D:/work/Github/BoxPacking-master/R/operations_on_chromosomes.R")
source("D:/work/Github/BoxPacking-master/R/PackBoxes.R")
source("D:/work/Github/BoxPacking-master/R/operations_on_boxes.R")
source("D:/work/Github/BoxPacking-master/R/EMS.R")
source("D:/work/Github/BoxPacking-master/R/objects_definition.R")
source("D:/work/Github/BoxPacking-master/R/PerformBoxPacking.R")
source("D:/work/Github/BoxPacking-master/R/plotcube.R")

BoxPacking::PerformBoxPacking
trace("PerformBoxPacking", edit = TRUE)
trace("CreateChromosome", edit = TRUE)

insertSource("D:/work/Github/BoxPacking-master/plotcube.R", package = "BoxPacking", functions = "PlotCube")



# create containers
containers <- list()
n_containers <- 4

for (i in 1:n_containers) {
    containers <- c(containers,
                    Container(length = 2, height = 2, width = 2)
    )
}

# create boxes
boxes <- list()
n_boxes <- 20

for (i in 1:n_boxes) {
    length <- sample(c(0.4, 0.5, 1), 1)
    height <- sample(c(0.4, 0.5, 1), 1)
    width <- sample(c(0.4, 0.5, 1), 1)

    boxes <- c(boxes,
               Box(length = length, height = height, width = width)
    )
}

# Box Packing
solution <-
    PerformBoxPacking(containers = containers,
                      boxes = boxes,
                      n_iter = 1,
                      population_size = 20,
                      elitism_size = 5,
                      crossover_prob = 0.5,
                      mutation_prob = 0.5,
                      verbose = TRUE,
                      plotSolution = TRUE
    )

