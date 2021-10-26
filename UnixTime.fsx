open System

let now = DateTimeOffset.Now.ToUnixTimeMilliseconds()
let ago (time:int64) = now - time
let minutes m = 1000L * 60L * m
let hours h = 60L * h |> minutes 
let days d = 24L * d |> hours
let epochDateTime = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
let toDateTime (timestamp:int64) =
    epochDateTime.AddMilliseconds(float timestamp).ToLocalTime()
let toEpochTime year month day =
    int64 ((DateTime(year, month, day,0,0,0,DateTimeKind.Utc)) - epochDateTime).TotalMilliseconds