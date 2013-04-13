module p2p


// [snippet: Async socket server using F# async computations.]
open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading
 



//type Socket with
//  member socket.AsyncAccept() = Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)


 
type Server() =
  static member Start(hostname:string, ?port) =
    let ipAddress = Dns.GetHostEntry(hostname).AddressList.[0]
    Server.Start(ipAddress, ?port = port)
 
  static member Start(?ipAddress, ?port) =
    let ipAddress = defaultArg ipAddress IPAddress.Any
    let port = defaultArg port 80
    let endpoint = IPEndPoint(ipAddress, port)
    let cts = new CancellationTokenSource()
    let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    listener.Bind(endpoint)
    listener.Listen(int SocketOptionName.MaxConnections)
    printfn "Started listening on port %d" port
    
    let rec loop() = async {
      printfn "Waiting for request ..."
      let socket = listener.Accept()
      // Setting ownsSocket to false allows us to later re-use a socket.
      let stream = new NetworkStream(socket, false) 
      printfn "Received request"
      let response = [|
        "HTTP/1.1 200 OK\r\n"B
        "Content-Type: text/plain\r\n"B
        "\r\n"B
        "Hello World!"B |] |> Array.concat
      try
        try
          let! bytesSent = stream.AsyncWrite(response)
          printfn "Sent response"
        with e -> printfn "An error occurred: %s" e.Message
      finally
        stream.Close()
        socket.Shutdown(SocketShutdown.Both)
        socket.Close()
      return! loop() }
 
    Async.Start(loop(), cancellationToken = cts.Token)
    { new IDisposable with member x.Dispose() = cts.Cancel(); listener.Close() }


// [/snippet]



 
// [snippet: Demo server]
let disposable = Server.Start(port = 8090)
Thread.Sleep(60 * 1000)
printfn "bye!"
disposable.Dispose()


// [/snippet]