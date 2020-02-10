//
// F# program to analyze Divvy daily ride data.
//
// << Het Banker >>
// U. of Illinois, Chicago
// CS 341, Fall 2019
// Project #04
//

#light
module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [15,22,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

 
//count the occurance of the bikeID
let rec getBikeID L uniqID =
    match L with
    | [ ] -> 0
    | head::tail when head = uniqID -> 1 + getBikeID tail uniqID
    | _ ::tail -> 0 + (getBikeID tail uniqID)

// to filter out the duplicates values from the ;ist
let filterDuplicates list1 =
    let rec FilterDuplicates list1 newEmptyList=
        match list1 with 
        | [ ] -> newEmptyList
        | head::tail when  (List.contains head newEmptyList) -> FilterDuplicates tail newEmptyList
        | head::tail -> FilterDuplicates tail (head :: newEmptyList)
    FilterDuplicates list1 [ ]

//count how many rides were taken per day
let rec dayCount day L =
    match L with
    | [ ] -> 0
    | head::tail when head = day -> 1 + (dayCount day tail)
    | _::tail -> 0 + (dayCount day tail)            


//this function to recurse thru every list in the list, used it almost every function
let rec getListOfList L index =
    match L with
    | [ ] -> 0
    | head::tail when index = 0 -> head
    | _::tail -> 0 + (getListOfList tail (index - 1))

//function to merge two lists
let rec concatList list index =
   match list with
   | [ ] -> [ ]
   | head::tail ->  0 + (getListOfList head index) :: (concatList tail index)

//creat a new list of bikeID, tostation and total seconds
let rec uniqueBikeAndStationTotalTIme L firstList secondList= 
    match L with
    | [ ] -> [ ]
    | head::tail -> [(getListOfList head firstList) ; (getListOfList head secondList)]::(uniqueBikeAndStationTotalTIme tail firstList secondList )

//
let rec getTimeAndBikeID L uniqueBikeID bike time = 
    match L with
    | [ ] -> [ ]
    | head::tail when (getListOfList head bike) = uniqueBikeID -> (getListOfList head time) :: (getTimeAndBikeID tail uniqueBikeID bike time)
    | _::tail ->  getTimeAndBikeID tail uniqueBikeID bike time

//prints the stars for the histogram
let rec starsPrint L =
    match L with 
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           starsPrint(L-1)

//count the number of stars for histogram
let rec histogram day L =
    match day with
    | 7 -> ()
    | day -> printf "%A: " day
             starsPrint((dayCount day L)/10)
             printfn " %A" (dayCount day L)
             histogram (day + 1) L

//make a new list of the 
let rec howManyTImmes L L1 =
    match L with
    | [ ] -> [ ]
    | head::tail -> [head; (dayCount head L1)] :: (howManyTImmes tail L1)

//2 seperate lists for the sorted toStation and occurance of the list
let rec sortedStationAndOccurance L x = 
    match L with
    | [ ] -> [ ]
    | head::tail -> (getListOfList head x) :: (sortedStationAndOccurance tail x)


//main
[<EntryPoint>]  
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()      //userinput for file name
  //let filename = "divvy01.csv"
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of rides: %A" N
  printfn ""


  let countBikes = concatList ridedata 2
  let bikeCounter = filterDuplicates countBikes
  let uniqueBikes = List.length bikeCounter
  printfn "# of bikes: %A" uniqueBikes
  printfn ""
  
  //ask for bikeID input
  printf "BikeID> "
  let input = System.Console.ReadLine() 
  let inputBike  = input |> int     //convert string to int
  let bikeID = getBikeID countBikes inputBike
  printfn ""
  printfn "# of rides for BikeID %A: %A" inputBike bikeID



  let time = concatList ridedata 5
  let listOfBikeIDAndTime = uniqueBikeAndStationTotalTIme ridedata 2 5
  let timeForInputBikeID = getTimeAndBikeID listOfBikeIDAndTime inputBike 0 1
  let totalTime = timeForInputBikeID  |> List.sum 
  let inFloatTotalTime = totalTime |> float
  let inFloat2BikeID = bikeID |> float
  let mins = totalTime / 60
  let secHelper = mins * 60
  let sec = totalTime - secHelper
  let averageSecs =  (inFloatTotalTime  / inFloat2BikeID) |> float 
  
  printfn ""
  printfn "Total time spent riding BikeID %A: %A minutes %A seconds" inputBike mins sec
  printfn ""
  printfn "Average time spent riding BikeID %A: %.2f seconds" inputBike averageSecs
  printfn ""


  let stationID = concatList ridedata 1
  printf "StationID> "
  let input1 = System.Console.ReadLine() 
  let inputStation = input1 |> int
  let ID = getBikeID stationID inputStation
  let stationAndTIme = uniqueBikeAndStationTotalTIme ridedata 1 5
  let timeForInputStation = getTimeAndBikeID stationAndTIme inputStation 0 1
  let totalTime2 = timeForInputStation |> List.sum
  let inFloatTotalTime2 = totalTime2 |> float
  let inFloat2BikeID2 = ID |> float
  let secs2 =  (inFloatTotalTime2  / inFloat2BikeID2) |> float 
  printfn ""
  printfn "# of rides to StationID %A: %A" inputStation ID
  printfn ""
  printfn "Average time spent on trips leading to StationID %A: %.2f seconds" inputStation secs2
  printfn ""


  //day and rides
  printfn "Number of Trips on Sunday: %A"    (dayCount 0 (concatList ridedata 4))
  printfn "Number of Trips on Monday: %A"    (dayCount 1 (concatList ridedata 4))
  printfn "Number of Trips on Tuesday: %A"   (dayCount 2 (concatList ridedata 4))
  printfn "Number of Trips on Wednesday: %A" (dayCount 3 (concatList ridedata 4))
  printfn "Number of Trips on Thursday: %A"  (dayCount 4 (concatList ridedata 4))
  printfn "Number of Trips on Friday: %A"    (dayCount 5 (concatList ridedata 4))
  printfn "Number of Trips on Saturday: %A"  (dayCount 6 (concatList ridedata 4))
  printfn ""

  histogram 0 (concatList ridedata 4) //prints the histogram
  printfn ""

  //z sorts takes 2 sub lists and sorts
 //let z = List.take 10 (filterDuplicates (howManyTImmes (sortedStationAndOccurance ridedata 1) (concatList ridedata 1)) |> List.sortBy (fun [stationBy;occur] -> [stationBy])|> List.sortBy(fun [_;_] -> [ ] )|> List.sortBy (fun [stationBy;occur] -> [-1*occur]))  |> List.rev |> List.rev
  let z = List.take 10 (filterDuplicates (howManyTImmes (sortedStationAndOccurance ridedata 1) (concatList ridedata 1)) |> List.sortBy (fun x -> -(sortedStationAndOccurance 0 x ), (sortedStationAndOccurance 1 x) )|> List.rev |> List.rev )
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 0) (getListOfList (sortedStationAndOccurance z 1) 0) 
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 1) (getListOfList (sortedStationAndOccurance z 1) 1)  
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 2) (getListOfList (sortedStationAndOccurance z 1) 2)  
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 3) (getListOfList (sortedStationAndOccurance z 1) 3) 
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 4) (getListOfList (sortedStationAndOccurance z 1) 4)  
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 5) (getListOfList (sortedStationAndOccurance z 1) 5)  
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 6) (getListOfList (sortedStationAndOccurance z 1) 6)  
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 7) (getListOfList (sortedStationAndOccurance z 1) 7) 
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 8) (getListOfList (sortedStationAndOccurance z 1) 8)  
  printfn "# of rides to station %A: %A" (getListOfList (sortedStationAndOccurance z 0) 9) (getListOfList (sortedStationAndOccurance z 1) 9) 
  printfn ""
  0 

 