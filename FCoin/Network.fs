module Network

open System.Net.Sockets

type ConnectionStatus =
   | Started
   | Accepting
   | Accepted
   | Connecting
   | Connected
   | GreetingSent
   | Established
   | Closing
   | Closed
   | Failing
   | Failed

let finished incoming = 
  true

let sendGreeting tcpConn = async {
   do! Async.Sleep(100)
   }

let waitForGreeting tcpConn = async {
    do! Async.Sleep(100)
    }

let rec processConnection tcpConn state = async {
  let loop = processConnection tcpConn

  match state with

    | Started ->
       do! sendGreeting tcpConn
       return! loop GreetingSent

    | GreetingSent ->
       let! rcvd = waitForGreeting tcpConn
       return! loop Established

    | Established ->
       do! Async.Sleep 100
       return! loop Established

    | Closing ->
       do! Async.Sleep 100
       return! loop Closed

    | Closed ->
       return Closed

    | Failing ->
       do! Async.Sleep 100
       return! loop Failed

    | Failed ->
       return Failed
  }


//let openSocket =
//  use socket = new System.Net.Sockets.Socket(SocketType.Stream, ProtocolType.Tcp)
//  //let acceptAsync = Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)
//  //let receiveAsync = Async.FromBeginEnd(socket.BeginReceive, socket.EndReceive)
//  let processConnect = async {
//    let! connected = acceptAsync
//    let! data = receiveAsync
//    do printf "%A" data
//    }
//  do Async.RunSynchronously processConnect


