module Main

open System.Collections.Generic

[<EntryPoint>]
let main argv = 
    let env = System.Environment.GetEnvironmentVariables()
    let config =
      Config.defaults 
      |> Config.withEnvironment env
      |> Config.withCommandLine argv
      |> Config.withConfiguredFiles
      |> Config.withUserChoices 
    printfn "%A" config
    0 // return an integer exit code
