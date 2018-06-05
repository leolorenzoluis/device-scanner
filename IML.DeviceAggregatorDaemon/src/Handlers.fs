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
open Heartbeats
open Thoth.Json
open Query
open Fable

type DevTree = Map<string, State>
let mutable devTree : DevTree = Map.empty

let init() =
    Ok devTree

let timeoutHandler host =
    printfn "Aggregator received no heartbeat from host %A" host
    devTree <- Map.remove host devTree

let updateTree host x devTree =
    let state = Decode.decodeString State.decoder x |> Result.unwrap
    Map.add host state devTree

let update (state : Result<DevTree, exn>) (request : Http.IncomingMessage)
  (response : Http.ServerResponse) : Result<DevTree, exn> =
    match request.method with
    | Some "GET" ->
        runQuery response state Legacy
        state
    | Some "POST" ->
        request
        |> Stream.reduce "" (fun acc x -> Ok(acc + x.toString ("utf-8")))
        |> Stream.iter (fun x ->
               match !!request.headers?("x-ssl-client-name") with
               | Some "" ->
                   eprintfn "Aggregator received message but hostname was empty"
               | Some host ->
                   match Message.decoder x with
                   | Ok Message.Heartbeat -> addHeartbeat timeoutHandler host
                   | Ok(Message.Data y) ->
                       printfn
                           "Aggregator received update with devices from host %s"
                           host
                       updateTree host y state
                   | Error x ->
                       eprintfn
                           "Aggregator received message but message decoding failed (%A)"
                           x
               | None ->
                   eprintfn
                       "Aggregator received message but x-ssl-client-name header was missing from request"
               response.``end``())
        |> ignore
    | x ->
        response.``end``()
        eprintfn "Aggregator handler got a bad match %A" x

let update (state : Result<State, exn>) (command : Command) : Result<State, exn> =
    match state with
    | Ok state ->
        match command with
        | ZedCommand x ->
            Zed.update state.zed x
            |> Result.map (fun zed -> { state with zed = zed })
        | UdevCommand x ->
            Udev.update state.blockDevices x
            |> Result.map
                   (fun blockDevices ->
                   { state with blockDevices = blockDevices })
        | MountCommand x ->
            Mount.update state.localMounts x
            |> Result.map
                   (fun localMounts -> { state with localMounts = localMounts })
        | Command.Stream -> Ok state
    | x -> x

let scan init update =
    let mutable state = init()
    fun x ->
        state <- update state x
        state

let handler = IML.CommonLibrary.scan init update
