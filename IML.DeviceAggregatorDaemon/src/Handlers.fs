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

type AggregatorCommand =
    | AddHeartbeat of string
    | RemoveHeartbeat of string
    | GetTree of Http.ServerResponse
    | UpdateTree of string * string

type DevTree = Map<string, State>

type Heartbeats = Map<string, JS.SetTimeoutToken>

type AggregatorState = {
    tree: DevTree;
    heartbeats: Heartbeats;
}

let init() = Map.empty

let heartbeatTimeout = 30000

let clearTimeout heartbeats host =
    Map.tryFind host heartbeats
    |> Option.map JS.clearTimeout
    |> ignore

let rec handleHeartbeat
  (state : Heartbeats) (command : AggregatorCommand) : Heartbeats =
    match command with
    | AddHeartbeat host ->
          clearTimeout state host
          let onTimeout() =
              clearTimeout state host
              handleHeartbeat state (RemoveHeartbeat host)
                  |> ignore
              //heartbeats <- Map.remove host heartbeats
              //(handler, host)
          let token = JS.setTimeout onTimeout heartbeatTimeout
          Map.add host token state
    | RemoveHeartbeat host ->
          clearTimeout state host
          Map.remove host state
    | _ ->
          state

let rec handleTree
  (state : DevTree) (command : AggregatorCommand) : DevTree =
    match command with
    | GetTree response ->
          runQuery response state Legacy
          state
    | UpdateTree ((host), (data)) ->
          data
          |> Decode.decodeString State.decoder
          |> Result.unwrap
          |> (fun x -> Map.add host x state)
    | _ ->
          state

let heartbeatReducer = IML.CommonLibrary.scan init handleHeartbeat
let treeReducer = IML.CommonLibrary.scan init handleTree

let aggregatorUpdate (command : AggregatorCommand) =
    {
        heartbeats = heartbeatReducer command
        tree = treeReducer command
    }


let update (state : DevTree) (request : Http.IncomingMessage)
  (response : Http.ServerResponse) : DevTree =
    match request.method with
    | Some "GET" ->
        aggregatorUpdate (GetTree response)
            |> ignore
    | Some "POST" ->
        request
        |> Stream.reduce "" (fun acc x -> Ok(acc + x.toString ("utf-8")))
        |> Stream.iter (fun x ->
               match !!request.headers?("x-ssl-client-name") with
               | Some "" ->
                     eprintfn "Aggregator received message but hostname was empty"
               | Some host ->
                     match Message.decoder x with
                     | Ok Message.Heartbeat ->
                           aggregatorUpdate (AddHeartbeat host)
                               |> ignore
                     | Ok(Message.Data y) ->
                           printfn
                               "Aggregator received update with devices from host %s"
                               host
                           aggregatorUpdate (UpdateTree (host, y))
                               |> ignore
                     | Error x ->
                           eprintfn
                               "Aggregator received message but message decoding failed (%A)"
                               x
               | None ->
                     eprintfn
                        "Aggregator received message but x-ssl-client-name header was missing from request"
        )
        |> ignore
        response.``end``()
    | x ->
        response.``end``()
        eprintfn "Aggregator handler got a bad match %A" x
    state
