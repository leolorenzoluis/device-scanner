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

let heartbeatTimeout = 30000

let clearTimeout heartbeats host =
    Map.tryFind host heartbeats
    |> Option.map JS.clearTimeout
    |> ignore

let addHeartbeat heartbeats handler host =
    clearTimeout heartbeats host
    let onTimeout() =
        clearTimeout heartbeats host
        reducer (RemoveHeartbeat host)
        //heartbeats <- Map.remove host heartbeats
        (handler host)

    let token = JS.setTimeout onTimeout heartbeatTimeout
    heartbeats <- Map.add host token heartbeats

type AggregatorCommand =
    | GetTree
    | UpdateTree of string * string
    | AddHeartbeat of string
    | RemoveHeartbeat of string

type DevTree = Map<string, State>
let mutable devTree : DevTree = Map.empty
type Heartbeats = Map<string, JS.SetTimeoutToken>

type AggregatorState = {
    tree: DevTree;
    heartbeats: Heartbeats;
}

let init() = {
    tree = Map.empty
    heartbeats = Map.empty
}

let rec update
  (state : AggregatorState) (command : AggregatorCommand) : AggregatorState =
    match command with
    | GetTree ->
          state
    | UpdateTree ((host), (data)) ->
          let newTree = data
                        |> Decode.decodeString State.decoder
                        |> Result.unwrap
                        |> (fun x -> Map.add host x state.tree)
          { state with tree = newTree }
    | AddHeartbeat host ->
          clearTimeout state.heartbeats host
          let onTimeout() =
              clearTimeout state.heartbeats host
              update state (RemoveHeartbeat host)
              //heartbeats <- Map.remove host heartbeats
              //(handler, host)
          let token = JS.setTimeout onTimeout heartbeatTimeout
          { state with heartbeats = Map.add host token state.heartbeats }
    | RemoveHeartbeat host ->
          clearTimeout state.heartbeats host
          { state with heartbeats = Map.remove host state.heartbeats }

let reducer = IML.CommonLibrary.scan init update


let update (state : DevTree) (request : Http.IncomingMessage)
  (response : Http.ServerResponse) : DevTree =
    match request.method with
    | Some "GET" ->
        reducer (GetTree)
            |> (fun x -> runQuery response x.tree Legacy)
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
                       printfn "Aggregator received no heartbeat from host %A" host
                       reducer (AddHeartbeat host)
                           |> ignore
                   | Ok(Message.Data y) ->
                       printfn
                           "Aggregator received update with devices from host %s"
                           host
                       reducer (UpdateTree host y)
                           |> ignore
                   | Error x ->
                       eprintfn
                           "Aggregator received message but message decoding failed (%A)"
                           x
               | None ->
                   eprintfn
                       "Aggregator received message but x-ssl-client-name header was missing from request"
                response.``end``())
    | x ->
        response.``end``()
        eprintfn "Aggregator handler got a bad match %A" x
    state
