# Part 2: Longitudinal Data Class and Methods
#
# The purpose of this part is to create a new class for representing 
# longitudinal data, which is data that is collected over time on a given 
# subject/person. This data may be collected at multiple visits, in multiple 
# locations. You will need to write a series of generics and methods for 
# interacting with this kind of data.
#
# The data for this part come from a small study on indoor air pollution on 10 
# subjects. Each subject was visited 3 times for data collection. Indoor air 
# pollution was measured using a high-resolution monitor which records pollutant 
# levels every 5 minutes and the monitor was placed in the home for about 1 
# week. In addition to measuring pollutant levels in the bedroom, a separate 
# monitor was usually placed in another room in the house at roughly the same 
# time.
#
# Before doing this part you may want to review the section on object oriented 
# programming (you can also read that section here).
#
# The variables in the dataset are
#
# id: the subject identification number
# visit: the visit number which can be 0, 1, or 2
# room: the room in which the monitor was placed
# value: the level of pollution in micrograms per cubic meter
# timepoint: the time point of the monitor value for a given visit/room
#
# You will need to design a class called “LongitudinalData” that characterizes 
# the structure of this longitudinal dataset. You will also need to design 
# classes to represent the concept of a “subject”, a “visit”, and a “room”.
#
# In addition you will need to implement the following functions
#
# make_LD: a function that converts a data frame into a “LongitudinalData” 
#          object
# subject: a generic function for extracting subject-specific information
# visit: a generic function for extracting visit-specific information
# room: a generic function for extracting room-specific information
#
# For each generic/class combination you will need to implement a method, 
# although not all combinations are necessary (see below). You will also need 
# to write print and summary methods for some classes (again, see below).

setGeneric("print")
setGeneric("summary")
setGeneric("subject", function(x,...){
  standardGeneric("subject")
})
setGeneric("visit", function(x,...){
  standardGeneric("visit")
})
setGeneric("room", function(x,...){
  standardGeneric("room")
})

# LongitudinalData Class and Methods

setClass("LongitudinalData",
         representation(id = "numeric", 
                      visit = "numeric",
                      room = "character",
                      value = "numeric",
                      timepoint = "numeric"))

setMethod("print",
          c(x = "LongitudinalData"),
          function(x){
            paste("Longitudinal dataset with", length(unique(x@id)), "subjects")
          })

setMethod("subject",
          c(x = "LongitudinalData"),
          function(x,n){
            new("subject_class", id = x@id[x@id == n], visit = x@visit[x@id == n],
                room = x@room[x@id == n], value = x@value[x@id == n],
                timepoint = x@timepoint[x@id == n])
          })

# subject_class Class and Methods

setClass("subject_class",
         representation(id = "numeric", 
                      visit = "numeric",
                      room = "character",
                      value = "numeric",
                      timepoint = "numeric"))

setMethod("print",
          c(x = "subject_class"),
          function(x){
            if (length(unique(x@id)) > 0) {
              cat(paste("Subject ID:",unique(x@id)))
            } else {
              NULL
            }
          })

setMethod("summary",
          c(object = "subject_class"),
          function(object){
            new("subject_summary", id = object@id, visit = object@visit, 
                room = object@room, value = object@value)
          })

setMethod("visit",
          c(x = "subject_class"),
          function(x,n){
            new("visit_class", id = x@id[x@visit == n], visit = x@visit[x@visit == n],
                room = x@room[x@visit == n], value = x@value[x@visit == n],
                timepoint = x@timepoint[x@visit == n])
          })

# subject_summary Class and Methods

setClass("subject_summary",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric"))

setMethod("print",
          c(x = "subject_summary"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            as.data.frame(cbind(visit=x@visit,room=x@room,value=x@value),stringsAsFactors = FALSE) %>%
              mutate(value = as.numeric(value)) %>%
              group_by(visit,room) %>%
              summarise(avg = mean(value)) %>%
              spread(room,avg)
          })

# visit_class Class and Methods

setClass("visit_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

setMethod("room",
          c(x = "visit_class"),
          function(x,n){
            new("room_class", id = x@id[x@room == n], visit = x@visit[x@room == n],
                room = x@room[x@room == n], value = x@value[x@room == n],
                timepoint = x@timepoint[x@room == n])
          })

# room_class Class and Methods

setClass("room_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

setMethod("print",
          c(x = "room_class"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            cat(paste("Visit:",unique(x@visit)),"\n")
            cat(paste("Room:",unique(x@room)))
          })

setMethod("summary",
          c(object = "room_class"),
          function(object){
            new("room_summary", id = object@id, value = object@value)
          })

# room_summary Class and Methods

setClass("room_summary",
         representation(id = "numeric", 
                        value = "numeric"))

setMethod("print",
          c(x = "room_summary"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            summary(x@value)
          })

# other functions

make_LD <- function(x) {
  new("LongitudinalData", id = x$id, visit = x$visit,
      room = x$room, value = x$value, timepoint = x$timepoint)
}
