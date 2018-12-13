#  ==================================================================================
#   * Program Name       	: 코아스 차량 적재 시뮬레이션 시스템
#   * Source File Name   	: KOAS_BoxPacking_MAIN.R
#   * Author             	: 
#   * Version           	: 1.0.0
#   * Created Date       	: 2018-08-06
#   * Updated Date      	: 2018-08-08
#   * Last modifier      	: 
#   * Updated content    	: pop-up dialog box 디자인 수정
#   * Description       	: 
#  ==================================================================================

# -----------------------------------------------------------------------------------
# 1. 라이브러리 로딩 환경 Setting
# -----------------------------------------------------------------------------------
#'
# devtools::install_github('delta1epsilon/BoxPacking')
# install.packages("here")
# install.packages("writexl")
library(here)
library(methods)
library(BoxPacking)
library(rgl) # 3d Graphing package
library(readxl) # excel file importing package
library(writexl)

#' set current working directory
# getwd <- dirname(rstudioapi::getSourceEditorContext()$path)
getwd <- here()
setwd(paste0(getwd, "/KOAS_BoxPacking-master/."))

#' load everything of packages to use "Customized Function" in global environment
#' @include BoxPacking
#' 
try(devtools::load_all(pkg = "."), silent = TRUE)


# -----------------------------------------------------------------------------------
# 2. Class(S4-type) 재설정
# -----------------------------------------------------------------------------------
#'
#' An s4 class to represent a Container
#'
#' @slot origin A length-three vector 
#' @slot length A numeric
#' @slot height A numeric
#' @slot width A numeric
#' @slot truckType A character
#'
#' @examples 
#' # create a container with size 2 x 2 x 2
#' c1 <- Container(length = 2, height = 2, width = 2)
#'
#' @export Container
#' 
Container <- setClass('Container',
                      slots = c(origin = 'numeric',
                                length = 'numeric',
                                height = 'numeric',
                                width = 'numeric',
                                ems = 'list',  # list of instances of class EMS 
                                truckType = 'character'
                      ),
                      prototype = list(origin = c(0, 0, 0)),
                      validity = function (object) {  # make sure that all parameters are positive
                        if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                          return("A number <= 0 for one of the parameters was given.")
                        }
                        return(TRUE)
                      }
)
#' create method 'initialize' for Container class to create 
#' slot EMS (initial EMS is whole Container) when
#' an instance of class Container is being created 
#' @export 
#' 
setMethod('initialize', 
          'Container',
          function (.Object, ...) {
            .Object <- callNextMethod()
            .Object@ems <- 
              list(
                EMS(origin = .Object@origin,
                    length = .Object@length,
                    height = .Object@height,
                    width = .Object@width,
                    truckType = .Object@truckType
                )
              )
            return(.Object)
          }
)

#' An s4 class to represent a Box
#'
#' @slot origin A length-three vector 
#' @slot length A numeric
#' @slot height A numeric
#' @slot width A numeric
#' @slot weight A numeric
#' @slot modelName A character
#'
#' @examples 
#' # create a box with size 1 x 1 x 1
#' b1 <- Box(length = 1, height = 1, width = 1)
#'
#' @export Box
#' 
Box <- setClass('Box',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric',
                          weight = 'numeric',
                          modelName = 'character'
                ),
                validity = function (object) {  # make sure that all parameters are positive
                  if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                    return("A number <= 0 for one of the parameters was given.")
                  }
                  return(TRUE)
                }
) 

#' define class for Empty Maximal Spaces
#' @export EMS
#' 
EMS <- setClass('EMS',
                slots = c(origin = 'numeric',
                          length = 'numeric',
                          height = 'numeric',
                          width = 'numeric',
                          truckType = 'character'
                )
)


# -----------------------------------------------------------------------------------
# 3. 3D Plot Cube function 재설정
# -----------------------------------------------------------------------------------
#
#' A custom function to initialize RGL device
#'
#' @param new.device - A logical value. If TRUE, creates a new device
#' @param width the width of the device
#' @import rgl
#' 
RGLInit <- function(new.device = FALSE, width = 500) {
  if (new.device | rgl.cur() == 0) {  # rgl.cur(): returns active device ID
    rgl.open()
    par3d(windowRect = 50 + c(0, 0, width, width))
    rgl.bg(color = "white")
  }
  rgl.viewpoint(theta = 40, phi = 20)
}

#' Plot 3D cube
#'
#' @param object - An object of class 'Container', 'Box' or 'EMS'
#' @param plot_origin  - logical, whether to plot point at the origin
#' @examples
#'
#' RGLInit(new.device = T)  # create new device with specific settings
#' # plot a container
#' container <- Container(width = 2, length = 4, height = 2)
#' PlotCube(container)
#'
#' # plot a box
#' box <- Box(width = 1, length = 1, height = 1, origin = c(0, 0,0))
#' PlotCube(box)
#' 
PlotCube <- function (object,
                      plot_origin = TRUE, label_name, ...) {
  
  # stop if object origin is not specified
  if (length(object@origin) == 0) {
    stop('Specify origin for the object')
  }
  
  origin <- object@origin
  length <- object@length
  height <- object@height
  width  <- object@width
  truckType <- NULL
  modelName <- NULL
  
  # stored differently according to the class attribute of the object.
  if (slotNames(object)[length(slotNames(object))] == "truckType") {
    truckType <- object@truckType
  } else if (slotNames(object)[length(slotNames(object))] == "modelName") {
      modelName <- object@modelName
  }
  
  vertex1 <- origin
  vertex2 <- origin + c(0, height, width)
  vertex3 <- origin + c(length, height, 0)
  vertex4 <- origin + c(length, 0, width)
  
  # create data frame with coordinates of lines
  # to be joined to form a cube
  lines <- data.frame(vertex1, origin + c(0, height, 0))
  lines <- cbind(lines, data.frame(vertex1, origin + c(length, 0, 0)))
  lines <- cbind(lines, data.frame(vertex1, origin + c(0, 0, width)))
  
  lines <- cbind(lines, data.frame(vertex2, origin + c(0, 0, width)))
  lines <- cbind(lines, data.frame(vertex2, origin + c(0, height, 0)))
  lines <- cbind(lines, data.frame(vertex2, origin + c(length, height, width)))
  
  lines <- cbind(lines, data.frame(vertex3, origin + c(0, height, 0)))
  lines <- cbind(lines, data.frame(vertex3, origin + c(length, 0, 0)))
  lines <- cbind(lines, data.frame(vertex3, origin + c(length, height, width)))
  
  lines <- cbind(lines, data.frame(vertex4, origin + c(0, 0, width)))
  lines <- cbind(lines, data.frame(vertex4, origin + c(length, 0, 0)))
  lines <- cbind(lines, data.frame(vertex4, origin + c(length, height, width)))
  
  lines <- t(lines)
  
  if (class(object) == 'Container') {
    cube_color <- 'black'
  } else {
    # randomly select color for cube
    colors <- c('blue','orange','green','black')
    cube_color <- sample(colors, 1)
    #labels <- c('a','b')
    #cube_labels <- sample(labels, 1)
    #label <- label_name
  }
  
  # plot cube
  segments3d(lines, line_antialias = TRUE, color = cube_color, ...)
  if (plot_origin) {
    points3d(x = 0, y = 0, z = 0, color = 'blue', size = 7)
  }
  
  print(modelName)
  gv.modelName <<- c(gv.modelName, modelName)
}


#' Plot a Packing Solution
#'
#' @param packing_solution - A list
#' @return Returns as many plots of Containers with placed Boxes
#'         as many nonempty Containers are in the packing solution
#'         
PlotPackingSolution <- function (packing_solution) {
  
  for (i in 1:length(packing_solution)) {
    if (length(packing_solution[[i]]) == 1) {
      # the Container is empty
      next
    } else {
      # initialize device
      RGLInit(new.device = T)
      
      # plot a container
      PlotCube(packing_solution[[i]][[1]])
      
      # plot boxes
      for (j in 2:length(packing_solution[[i]])) {
        PlotCube(packing_solution[[i]][[j]])
      }
    }
  }
  
}


# -----------------------------------------------------------------------------------
# 4. 전역(global type) 변수 정의
# -----------------------------------------------------------------------------------
#
#' 적재된 제품명 저장 변수 정의
#' @inheritParams gv.modelName A character
#' @method - assign()
#' @details GlobalEnv variable
#' 
gv.modelName <- vector()
global.variable <- function () {
  assign("gv.modelName", NULL ,envir = .GlobalEnv)
}
global.variable()


# -----------------------------------------------------------------------------------
# 4. 데이터 준비
# -----------------------------------------------------------------------------------
#
#' 모델명/제품 사이즈 정보 전처리 및 loading
#' 
#' @docType .xlsx
#' @method - read_excel(), na.omit()
#' @concepts retrieving .xlsx file information with read_excel
#'           handling missing observation with na.omit
#' 
df.Goods_size <- na.omit(read_excel(paste0(getwd, "/KOAS_BoxPacking_INPUT.xlsm"), sheet = "Goods_size"))
df.Truck_Ton <- read_excel(paste0(getwd, "/KOAS_BoxPacking_INPUT.xlsm"), sheet = "INPUT_Truck_Ton", col_names = FALSE)
df.Truck_spec <- na.omit(read_excel(paste0(getwd, "/KOAS_BoxPacking_INPUT.xlsm"), sheet = "Truck_spec."))

#' 차량 톤수별 사이즈 정보 loading
trucks <- vector(mode="list")
n_trucks <- 1 # max number of trucks display
# names(truckType) <- df.Truck_spec$Truck_Type[1:nrow(df.Truck_spec)]


#' 차량 톤수 사용자 입력
#' 
#' @param scanTon - A numeric from keyboard input
#' @param ton - A numeric
#' @return Returns truck tonnage to refer specification(size) of the truck
#' @examples
#'
#' scanTon <- dlgInput("Enter the tonnage of the truck to be loaded", default = "", type = c("ok"))$res
#' # tap '5.5' into dialog box to storage numeric type variable
#'
#' # if you tapped something wrong, display ERROR message just like below
#' if(scanTon!="1"&&scanTon!="1.4"&&scanTon!="2.5"&&scanTon!="3.5"&&scanTon!="5"&&scanTon!="5.5"&&scanTon!="11"){
#'   dlg_message("ERROR! : enter exact tonnage of the truck.................................
#'                  (ex 1/1.4/2.5/3.5/5/5.5/11)", type = c("ok"))
#'
#'
#' caluate Total Product CBM(Cubic Meter)
Product_CBM <- c(NULL)

  for(i in 1:nrow(df.Goods_size)){
    temp.Product_CBM <- (df.Goods_size$Delivery_Quantity[i]*
                         df.Goods_size$Length[i]*
                         df.Goods_size$Width[i]*
                         df.Goods_size$Height[i])
  
    Product_CBM <- c(Product_CBM, temp.Product_CBM)
}

#' paste dialog box title with Total Product CBM
totalProduct_CBM <- round(sum(Product_CBM),2)
dialogBoxTitle_CBM <- paste("Total Product CBM :", as.character(totalProduct_CBM), "m3" ,sep = " ")

scanTon <- df.Truck_Ton[2,1]

# require(svDialogs)
# while(TRUE){
#   
#   # command pop-up dialog box
#   scanTon <- dlgInput(c(paste("              ", dialogBoxTitle_CBM),                  
#                         "       Enter the tonnage of the truck to be loaded                    "), 
#                       default = "", type = c("ok"))$res  
#   
#   # scanTon <- dlgInput(c("\n",
#   #                       paste("                         ", dialogBoxTitle_CBM),
#   #                       "\n",
#   #                       
#   #                       "                               [ CBM Standards ]                              ",
#   #                       "                   1.0t  : max (7.17)   /   optimum (5.02)                    ",
#   #                       "                   1.4t  : max (9.18)   /   optimum (6.43)                    ",
#   #                       "                   2.5t  : max (13.93)  /   optimum (9.75)                    ",
#   #                       "                   3.5t  : max (18.86)  /   optimum (13.20)                   ",
#   #                       "                   5.0t  : max (28.52)  /   optimum (19.96)                   ",
#   #                       "                   5.5t  : max (36.80)  /   optimum (25.76)                   ",
#   #                       "                  11.0t : max (46.53)  /   optimum (32.57)                    ",
#   #                       
#   #                       "\n",
#   #                       "\n",
#   #                       "               Enter the tonnage of the truck to be loaded                    "), 
#   #                     default = "", type = c("ok"))$res
# 
#   if(scanTon!="1"&&
#       scanTon!="1.4"&&
#        scanTon!="2.5"&&
#         scanTon!="3.5"&&
#          scanTon!="5"&&
#           scanTon!="5.5"&&
#            scanTon!="11"){
#     
#       # if user tapped wrong strings, display ERROR message just like below
#       dlg_message("ERROR! : enter exact tonnage of the truck", type = c("ok"))
#   } else break
# }
  # assign row index to 'ton' object
  if(scanTon=="1"){ton<-1
    } else if(scanTon=="1.4"){ton<-2
      } else if(scanTon=="2.5"){ton<-3
        } else if(scanTon=="3.5"){ton<-4
          } else if(scanTon=="5"){ton<-5
            } else if(scanTon=="5.5"){ton<-6
              } else ton<-7
  
  # 모델 사용변수에(trucks) 차량 정보 저장
  for(i in 1:n_trucks){
    trucks <- c(trucks, Container(length = df.Truck_spec$Length[ton], 
                                  height = df.Truck_spec$Height[ton], 
                                  width = df.Truck_spec$Width[ton], 
                                  truckType = df.Truck_spec$Truck_Type[ton]))
}

#' 적재 대상 제품 사이즈 정보 loading
goods <- vector(mode="list")
n_goods <- df.Goods_size$Delivery_Quantity[1:nrow(df.Goods_size)]
# names(goods) <- df.Goods_size$모델[1:nrow(df.Goods_size)]

  # 모델 사용변수에(goods) 적재 대상제품 정보 저장
  for(i in 1:nrow(df.Goods_size)){
      for (j in 1:n_goods[i]) {
                                length <- df.Goods_size$Length[i]
                                height <- df.Goods_size$Height[i]
                                width  <- df.Goods_size$Width[i]
                                modelName <- df.Goods_size$Model[i]
    
                                goods  <- c(goods, Box(length = length, 
                                                       height = height, 
                                                       width = width, 
                                                       modelName = modelName))
  }
}


# -----------------------------------------------------------------------------------
# 5. 차량 적재 시뮬레이션 실행
# -----------------------------------------------------------------------------------
#'
#' 사용자 지정 트럭 종류에 따라 실행
solution <-
    PerformBoxPacking(containers = trucks,
                      boxes = goods,
                      n_iter = 1,
                      population_size = 1,
                      elitism_size = 5,
                      crossover_prob = 0.5,
                      mutation_prob = 0.5,
                      verbose = TRUE,
                      plotSolution = TRUE
    )

#' 차량 적재 시뮬레이션 결과
loaded.table <- table(gv.modelName)
result.table <- cbind(df.Goods_size[,c("Num","Model","Length","Width","Height","Delivery_Quantity")], 
                      Loaded_Quantity = c(0))

for(i in 1:nrow(result.table)){
  for(j in 1:length(gv.modelName)){
    
    # if same model name / less than or equal to delievery quantity / not zero value
    if(result.table$Model[i] == gv.modelName[j] && 
       result.table$Delivery_Quantity[i] > result.table$Loaded_Quantity[i] && 
       loaded.table[result.table$Model[i]] > 0){
      
        result.table$Loaded_Quantity[i] <- result.table$Loaded_Quantity[i] + 1
        loaded.table[result.table$Model[i]] <- loaded.table[result.table$Model[i]] - 1
    }
    
  }
}

#' calculate Unloaded Quantity (Delivery Quantity - Loaded Quantity)
result.table <- cbind(result.table, 
                      Unloaded_Quantity = (result.table$Delivery_Quantity - result.table$Loaded_Quantity))

#' calculate loading rate (Loaded Product CBM / Truck Nominal spec.)
Loaded_Product_CBM <- c(NULL)

  for(i in 1:nrow(result.table)){
    temp.Product_CBM <- (result.table$Loaded_Quantity[i]*
                         result.table$Length[i]*
                         result.table$Width[i]*
                         result.table$Height[i])
  
    Loaded_Product_CBM <- c(Loaded_Product_CBM, temp.Product_CBM)
}

loadingRate <- round((sum(Loaded_Product_CBM)/(df.Truck_spec$Length[ton]*
                                               df.Truck_spec$Width[ton]*
                                               df.Truck_spec$Height[ton]))*100,0)


# -----------------------------------------------------------------------------------
# 6. 결과 Display
# -----------------------------------------------------------------------------------
#'
#' display loading result table & add text in 3d plot (loading rate)
#' 
text3d(x=0, y=3.5, z=0, 
       text = paste("Loading Rate :", loadingRate, "%", sep = " "), 
       color = "blue", specular="black", 
       adj = c(0.5,0.5), 
       font = 1)

utils::View(result.table,
            title = paste("Loading Rate :", loadingRate, "%", sep = " "))

writexl::write_xlsx(result.table, path = paste0(getwd, "/output/KOAS_output.xlsx"), col_names = TRUE)

Sys.sleep(10000)
  

