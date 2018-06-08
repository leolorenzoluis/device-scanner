// Copyright (c) 2018 Intel Corporation. All rights reserved.
// Use of this source code is governed by a MIT-style
// license that can be found in the LICENSE file.

module IML.DeviceAggregatorDaemon.Handlers

open Fable.Core.JsInterop
open Fable.Import.Node
open Fable.Import.Node.PowerPack
open IML.CommonLibrary
open IML.Types.MessageTypes
open IML.Types.ScannerStateTypes
open Thoth.Json
open Query
open Fable
open Fable.Import

open Elmish
open Elmish
//open Elmish.React
//open Fable.Helpers.React

//module Heartbeat =
type Model =
    { heartbeats : Map<string, int> } // JS.SetTimeoutToken> }

type Msg =
    | AddHeartbeat of string
 ///   * (AggregatorCommand -> Heartbeats)
    | RemoveHeartbeat of string
    | GetTree of Http.ServerResponse
    | UpdateTree of string * string

let init() =
    { heartbeats = Map.empty }, Cmd.ofMsg (AddHeartbeat "test")

//type Model =
//    { x : int }

// type Msg =
    // | Increment
    // | Decrement

// let init () =
    // { x = 0 }, Cmd.ofMsg Increment

let update msg model =
    match msg with
    | AddHeartbeat x -> //when model.x < 3 ->
        { model with heartbeats = Map.add x 1 model.heartbeats }, Cmd.ofMsg (RemoveHeartbeat "test")
    | RemoveHeartbeat x -> //when model.x < 3 ->
        { model with heartbeats = Map.remove x model.heartbeats }, Cmd.ofMsg (AddHeartbeat "test")
    | _ ->
        model, Cmd.none
    // | Increment ->
        // { model with x = model.x + 1 }, Cmd.ofMsg Decrement
//
    // | Decrement when model.x > 0 ->
        // { model with x = model.x - 1 }, Cmd.ofMsg Decrement
//
    // | Decrement ->
        // { model with x = model.x - 1 }, Cmd.ofMsg Increment

Program.mkProgram init update (fun model _ -> printf "%A\n" model)
|> Program.run

//type Heartbeats = Map<string, JS.SetTimeoutToken>
//
//// type AggregatorState = {
//    // tree: DevTree;
//    // heartbeats: Heartbeats;
//// }
//
//let init() = Map.empty
//
//let heartbeatTimeout = 30000
//
//let clearTimeout heartbeats host =
//    Map.tryFind host heartbeats
//    |> Option.map JS.clearTimeout
//    |> ignore
//
//let rec handleHeartbeat
//  (state : Heartbeats) (command : AggregatorCommand) : Heartbeats =
//    match command with
//    | AddHeartbeat ((host), (handler)) ->
//          clearTimeout state host
//          let onTimeout() =
//              handler (RemoveHeartbeat host)
//                  |> ignore
//              //heartbeats <- Map.remove host heartbeats
//              //(handler, host)
//          let token = JS.setTimeout onTimeout heartbeatTimeout
//          Map.add host token state
//    | RemoveHeartbeat host ->
//          clearTimeout state host
//          Map.remove host state
//    | _ ->
//          state
//
//let rec handleTree
//  (state : DevTree) (command : AggregatorCommand) : DevTree =
//    match command with
//    | GetTree response ->
//          runQuery response state Legacy
//          state
//    | UpdateTree ((host), (data)) ->
//          data
//          |> Decode.decodeString State.decoder
//          |> Result.unwrap
//          |> (fun x -> Map.add host x state)
//    | _ ->
//          state
//
//let heartbeatReducer = IML.CommonLibrary.scan init handleHeartbeat
//let treeReducer = IML.CommonLibrary.scan init handleTree
//
//let serverHandler (request : Http.IncomingMessage) (response : Http.ServerResponse) =
//    match request.method with
//    | Some "GET" ->
//        treeReducer (GetTree response) |> ignore
//    | Some "POST" ->
//        request
//        |> Stream.reduce "" (fun acc x -> Ok(acc + x.toString ("utf-8")))
//        |> Stream.iter (fun x ->
//               match !!request.headers?("x-ssl-client-name") with
//               | Some "" ->
//                     eprintfn "Aggregator received message but hostname was empty"
//               | Some host ->
//                     match Message.decoder x with
//                     | Ok Message.Heartbeat ->
//                           heartbeatReducer (AddHeartbeat (host, heartbeatReducer)) |> ignore
//                     | Ok(Message.Data y) ->
//                           treeReducer (UpdateTree (host, y)) |> ignore
//                     | Error x ->
//                           eprintfn
//                               "Aggregator received message but message decoding failed (%A)"
//                               x
//               | None ->
//                     eprintfn
//                        "Aggregator received message but x-ssl-client-name header was missing from request"
//        )
//        |> ignore
//    | x ->
//        eprintfn "Aggregator handler got a bad match %A" x
//    response.``end``()
