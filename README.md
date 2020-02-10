# Project 04 -- Divvy Analysis F#

## Get started

You will see `Program.fs` and the file `divvy01.csv`, `divvy02.csv` under the `program` folder.

Your task is to implement functionalities in the `Program.fs`. 
For the detailed requirements, please check the project description in Box folder

`divvy01.csv` contains a snapshot of trip histories from Divvy. Each line contains exactly 6 values,
* from_station_id: the station id where the bike comes from
* to_station_id: the station id where the bike goes
* bikeid: bike id
* start_hour: the start hour of the trip, from 0 to 23
* start_dayofweek: day of week when the trip starts
* tripduration: trip duration in seconds

## Building the project

* Open a terminal, `cd program` and type
```bash
$ dotnet build && dotnet run
```

* input the filename in the console
```bash
filename> divvy01.csv
```

* test your implementation accumulatively. 

## Requirements
The detailed requirements are written in the project description, please use the exact output format when implementing your function.

## Autograde test
We provide two more file to check the correct output format, `_expected_output` and `autograde.sh`.
It will compare the output with the test case `divvy01.csv`.

* run autograde.sh
```bash
$ ./autograde.sh
```
* it will generate a new file `_user_output`. 
And output the difference in console between your output and the expected output.

## Submission
Please submit your work `Program.fs` to Gradescope.