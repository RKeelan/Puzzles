module Time

open System
open Numbers

// Types ------------------------------------------------------------------------------------------

type Day =
    | Mon
    | Tues
    | Wed
    | Thurs
    | Fri
    | Sat
    | Sund

type Month =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

// Year Functions ---------------------------------------------------------------------------------

let isLeapYear (year:int) : bool =
    if not(isDivisible year 4) then false
    else
        if isDivisible year 400 then true
        elif isDivisible year 100 then false
        else true

let daysInYear year = if isLeapYear year then 366 else 365

// Month Functions --------------------------------------------------------------------------------

let Months = [Jan;Feb;Mar;Apr;May;Jun;Jul;Aug;Sep;Oct;Nov;Dec]

let daysInMonth month year =
    match month with 
    | Jan -> 31
    | Feb -> if isLeapYear year then 29 else 28
    | Mar -> 31
    | Apr -> 30
    | May -> 31
    | Jun -> 30
    | Jul -> 31
    | Aug -> 31
    | Sep -> 30
    | Oct -> 31
    | Nov -> 30
    | Dec -> 31

let precedingMonth month =
    match month with
        | Jan -> raise (ArgumentException("No month precedes January"))
        | Feb -> Jan
        | Mar -> Feb
        | Apr -> Mar
        | May -> Apr
        | Jun -> May
        | Jul -> Jun
        | Aug -> Jul
        | Sep -> Aug
        | Oct -> Sep
        | Nov -> Oct
        | Dec -> Nov

let rec firstOfMonth month year : int =
    match month with
    | Jan -> 1
    | _ -> (firstOfMonth (precedingMonth month) year) + (daysInMonth (precedingMonth month) year)