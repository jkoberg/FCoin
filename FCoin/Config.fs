module Config

open System.Collections.Generic


module Dict =
  let merge newD oldD = 
    let oldThenNew = seq { yield! oldD; yield! newD }
    dict (seq {
      for kvp : KeyValuePair<_,_> in oldThenNew
        -> (kvp.Key, kvp.Value) 
      })

  let keysMatching (keylist:string seq) (d:KeyValuePair<_,_> seq) =
      let keys = set keylist
      seq {
        for kvp in d do
          if keys.Contains(kvp.Key)
          then yield kvp 
        }
          
    

module File =
  let readUtf8OrEmpty filename =
    try 
        System.IO.File.ReadAllText(filename, System.Text.Encoding.UTF8)
    with
      | :? System.IO.FileNotFoundException -> ""

let defaults =
  dict [
    "configFileName", ".fcoinrc"
  ] 

let fileKeysAllowed = [
    "nodeName"
    ]

let envKeysAllowed =
  dict [
    "FCoin_nodeName", "nodeName" 
    ]

let parse (filedata:string) = 
    let lines = filedata.Split('\n')
    seq {
      for line in lines do
        let s = line.Split([|'='|], 2)
        if s.Length > 1 
        then yield new KeyValuePair<_,_>(s.[0].Trim(), s.[1].Trim())
      }

let withConfiguredFiles (configSoFar:IDictionary<_,_>) =
    let filedata = File.readUtf8OrEmpty configSoFar.["configFileName"]
    let newdata = parse filedata |> Dict.keysMatching fileKeysAllowed
    Dict.merge configSoFar newdata

let withCommandLine argv (configSoFar:IDictionary<_,_>) =
    configSoFar

let withEnvironment (env:System.Collections.IDictionary) (configSoFar:IDictionary<string,string>) =
    Dict.merge configSoFar (dict (seq { 
        for kvp in envKeysAllowed do
          if env.Contains(kvp.Key)
          then yield kvp.Value, downcast env.[kvp.Key]
          }))

let withUserChoices (configSoFar:IDictionary<string,string>) =
    configSoFar