module FileTransferExample

open Elmish
open Browser.Types
open Browser
open Fable.Core
open Fable.Core.JsInterop

module AblyMapping =
    type IMessage =
        abstract name: string
        abstract data: string

    type IChannel =
        abstract publish: string * string -> unit
        abstract subscribe: string * (IMessage -> unit) -> unit

    type IChannels =
        abstract ``get``: string -> IChannel

    type IRealTime =
        abstract channels: IChannels

    type IAbly =
        abstract Realtime: (string) -> IRealTime

    let Ably: IAbly = importAll "Ably"


[<AutoOpen>]
module Messaging =
    type File = {
        Name: string
        Data: Blob
    }

    type MessageType =
        | File of string

    type FileBlob = Blob

    type ChannelMessage =
        | File of FileBlob

type LifecycleModel = {
    Peer: RTCPeerConnection
    Channel: RTCDataChannel option
}

type Role =
    | Sender
    | Receiver

type PeerModel = {
    Lifecycle: LifecycleModel
    Channel: AblyMapping.IChannel
}

type InitializeMsg =
    | Sender
    | Receiver

module Signaling =
    open AblyMapping

    type WebRTCOffer = Offer of string
    type WebRTCAnswer = Answer of string
    type WebRTCCandidate = Candidate of string

    type Notification =
        | Offer of WebRTCOffer
        | Answer of WebRTCAnswer
        | SenderCandidate of WebRTCCandidate
        | ReceiverCandidate of WebRTCCandidate

    let onSignal (channel: IChannel) msg =
        match msg with
        | Notification.Answer (WebRTCAnswer.Answer ans) ->
            channel.publish ("answer", ans)
        | Notification.SenderCandidate (WebRTCCandidate.Candidate candidate) ->
            channel.publish ("sender-candidate", candidate)
        | Notification.ReceiverCandidate (WebRTCCandidate.Candidate candidate) ->
            channel.publish ("receiver-candidate", candidate)
        | Notification.Offer (WebRTCOffer.Offer offer) ->
            channel.publish ("offer", offer)

    let init (channel: IChannel) (subscribers: Map<string, IMessage -> unit>) =
        subscribers
        |> Map.iter (fun subscriberKey subscriberFunc ->
            channel.subscribe (subscriberKey, subscriberFunc)
        )
        channel

module Base64 =
    let ``to`` s =
        s
        |> Fable.Core.JS.JSON.stringify
        |> window.btoa

    let from s =
        s
        |> window.atob
        |> Fable.Core.JS.JSON.parse

let log moduleName msg =
    console.log (sprintf "[%s] %s" moduleName msg)

let mutable peer: LifecycleModel = Unchecked.defaultof<_>

module WebRTC =
    let private adapter: obj = importAll "webrtc-adapter"

    type ConnectionState =
        | NotConnected
        | Connecting
        | Connected

    [<RequireQualifiedAccess>]
    type Msg =
        | Offer of string
        | Answer of string
        | Candidate of string
        | State of ConnectionState

    let private internalLog = log "WebRTC"
    [<Fable.Core.Emit("new RTCSessionDescription(JSON.parse(atob($0)))")>]
    let private rtcSessionDescription (x: string): RTCSessionDescriptionInit =
        jsNative

    [<Fable.Core.Emit("new RTCIceCandidate(JSON.parse(atob($0)))")>]
    let private rtcIceCandidateInit (x: string): RTCIceCandidateInit = jsNative

    let create () =
        let conf =
            [| RTCIceServer.Create( [| "turn:serverAddress" |], "account", "pass", RTCIceCredentialType.Password ) |]
            |> RTCConfiguration.Create
            |> fun x ->
                x.iceTransportPolicy <- Some RTCIceTransportPolicy.Relay
                x

        let pc = RTCPeerConnection.Create(conf)

        { Peer = pc; Channel = None }

    let createChannel pc name =
        let channel = pc.Peer.createDataChannel name
        { pc with Channel = Some channel}

    let private subscribeChannel (channel: RTCDataChannel) msgHandler dispatch =
        channel.onopen <-
          fun _ ->
            channel.onmessage <-
                fun e ->
                    let blob = e.data :?> Blob
                    msgHandler blob
            internalLog (sprintf "Channel %s open" channel.label)
            Connected
            |> Msg.State
            |> dispatch

        channel.onclose <-
          fun _ ->
            internalLog (sprintf "channel %s close" channel.label)
        channel

    let private initReceiverChannel (pc: RTCPeerConnection) msgHandler dispatch =
        let mutable updatedChannel: RTCDataChannel = Unchecked.defaultof<_>
        let callback (ev: RTCDataChannelEvent) =
            let receiveChannel = subscribeChannel ev.channel msgHandler dispatch
            internalLog (sprintf "updating channel: %A" ev.channel.id)
            peer <- { peer with Channel = Some receiveChannel }

        pc.ondatachannel <- callback
        updatedChannel

    let subscribe connection onSignal msgHandler dispatch role =
        let pc, channel = (connection.Peer, connection.Channel)
        let updatedChannel =
            if role = Role.Sender then
                match channel with
                | Some c ->
                    subscribeChannel c msgHandler dispatch
                | None -> failwith "Channel is not initilized for sender"
            else
                initReceiverChannel pc msgHandler dispatch

        pc.oniceconnectionstatechange <-
            fun _ ->
                internalLog (sprintf "Connection state changed to: %A" pc.iceConnectionState)

        pc.onicecandidate <-
          fun e ->
            match e.candidate with
            | None -> internalLog "Trickle ICE Completed"
            | Some cand ->
              cand.toJSON()
              |> Base64.``to``
              |> Signaling.WebRTCCandidate.Candidate
              |>
                if role = Role.Sender then
                    Signaling.Notification.SenderCandidate
                else Signaling.Notification.ReceiverCandidate
              |> onSignal

        { Peer = pc
          Channel = Some updatedChannel }

    let init connection onSignal =
        let pc, channel = connection.Peer, connection.Channel
        pc.createOffer().``then``(fun desc ->
              pc.setLocalDescription (desc) |> Promise.start
              if isNull desc.sdp then
                internalLog "Local description is empty"
              else
                desc
                |> Base64.``to``
                |> Signaling.WebRTCOffer.Offer
                |> Signaling.Notification.Offer
                |> onSignal)

            |> Promise.catch (sprintf "On negotation needed return error: %A" >> internalLog)
            |> Promise.start

        { Peer = pc
          Channel = channel }

    let setAnswer (lifecycle: LifecycleModel) remote =
        try
            let desc = rtcSessionDescription remote
            lifecycle.Peer.setRemoteDescription desc
            |> Promise.catch (sprintf "Failed to set remote description: %A" >> internalLog)
            |> Promise.start
        with e ->
            internalLog (sprintf "Error occured while adding remote description: %A" e)

    let setOffer (lifecycle: LifecycleModel) onSignal remote =
        try
            let desc = rtcSessionDescription remote
            lifecycle.Peer.setRemoteDescription desc
            |> Promise.catch (sprintf "Failed to set remote description: %A" >> internalLog)
            |> Promise.start
            lifecycle.Peer.createAnswer().``then``(fun desc ->
                lifecycle.Peer.setLocalDescription (desc) |> Promise.start
                if isNull desc.sdp then
                    internalLog "Local description is empty"
                else
                    desc
                    |> Base64.``to``
                    |> Signaling.WebRTCAnswer.Answer
                    |> Signaling.Notification.Answer
                    |> onSignal)

            |> Promise.catch (sprintf "On negotation needed errored with: %A" >> internalLog)
            |> Promise.start
        with e ->
            internalLog (sprintf "Error occured while adding remote description: %A" e)

    let setCandidate (lifecycle: LifecycleModel) candidate =
        try
            let cand = rtcIceCandidateInit candidate
            lifecycle.Peer.addIceCandidate cand
            |> Promise.catch (sprintf "Failed to set add candidate: %A" >> internalLog)
            |> Promise.start
        with e ->
            internalLog (sprintf "Error occured while adding candidate: %A" e)

type Msg =
    | SendFile of File
    | Initialize of InitializeMsg
    | WebRTC of WebRTC.Msg
    | Signaling of AblyMapping.IChannel
    | Connect

type Model = {
    Role: Role option
    ConnectionState: WebRTC.ConnectionState
    Channel: AblyMapping.IChannel
    WaitingCandidates: string list }

peer <- WebRTC.create ()

module MessageTransfer =
    let send (lifecycle: LifecycleModel) (msg: ChannelMessage) =
        match msg, lifecycle.Channel with
        | File blob, Some channel ->
            blob
            |> U4.Case2
            |> channel.send
        | _, _ -> log "MessageTransfer" (sprintf "Unable to process: %A channel is: %A" msg lifecycle.Channel)

    let download (data: Blob) =
        let url = URL.createObjectURL data
        let mutable anchor = document.createElement "a"
        anchor.hidden <- true
        anchor.setAttribute ("href", url)
        anchor.setAttribute ("download", "image.png")
        document.body.appendChild anchor |> ignore
        anchor.click ()
        document.body.removeChild anchor |> ignore


let webRTCInit subscribeAs (lifecycle: LifecycleModel) (channel: AblyMapping.IChannel) role =
    subscribeAs lifecycle (Signaling.onSignal channel) MessageTransfer.download role

[<Literal>]
let ChannelName = "fileTransfer"

[<Literal>]
let AblyChannelName = "fileShare"

let init (): Model * Cmd<Msg> =
    let channel =
        AblyMapping.Ably.Realtime "apikey"
        |> fun x -> x.channels.get "fileShare"
    let model = {
        Role = None
        Channel = channel
        ConnectionState = WebRTC.ConnectionState.NotConnected
        WaitingCandidates = [] }
    model, Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Initialize Sender ->
        let lifecycle = WebRTC.createChannel peer ChannelName
        peer <- lifecycle
        let subscribe dispatch =
            let subs =
                [
                    "answer", fun (msg: AblyMapping.IMessage) -> dispatch (WebRTC (WebRTC.Msg.Answer msg.data))
                    "receiver-candidate", fun (msg: AblyMapping.IMessage) -> dispatch (WebRTC (WebRTC.Msg.Candidate msg.data))
                ] |> Map.ofList
            Signaling.init model.Channel subs
            |> Msg.Signaling
            |> dispatch

        { model with
                    Role = Some Role.Sender }, Cmd.ofSub subscribe
    | Initialize Receiver ->
        let subscribe dispatch =
            let subs =
                [
                    "offer", fun (msg: AblyMapping.IMessage) -> dispatch (WebRTC (WebRTC.Msg.Offer msg.data))
                    "sender-candidate", fun (msg: AblyMapping.IMessage) -> dispatch (WebRTC (WebRTC.Msg.Candidate msg.data))
                ] |> Map.ofList
            Signaling.init model.Channel subs
            |> Msg.Signaling
            |> dispatch

        { model with Role = Some Role.Receiver }, Cmd.ofSub subscribe

    | Connect ->
        let lifecycle' = WebRTC.init peer (Signaling.onSignal model.Channel)
        peer <- lifecycle'
        model, Cmd.none

    | Signaling channel ->
        match model.Role with
        | Some r ->
            let init dispatch =
                let lifeCycle = webRTCInit WebRTC.subscribe peer channel (WebRTC >> dispatch) r
                peer <- lifeCycle

            { model with
                Channel = channel }, Cmd.ofSub init
        | None ->
            log "Update" "Role is not initialized properly"
            model, Cmd.none

    | WebRTC (WebRTC.Msg.Offer offer) ->
        WebRTC.setOffer peer (Signaling.onSignal model.Channel) offer
        if model.WaitingCandidates.Length > 0 then
            model.WaitingCandidates
            |> List.iter (WebRTC.setCandidate peer )
            { model with
                        ConnectionState = WebRTC.ConnectionState.Connecting
                        WaitingCandidates = [] }, Cmd.none
        else
            { model with ConnectionState = WebRTC.ConnectionState.Connecting }, Cmd.none
    | WebRTC (WebRTC.Msg.Answer answer) ->
        WebRTC.setAnswer peer answer
        { model with ConnectionState = WebRTC.ConnectionState.Connecting }, Cmd.none
    | WebRTC (WebRTC.Msg.Candidate candidate) ->
        if model.ConnectionState <> WebRTC.ConnectionState.NotConnected then
            WebRTC.setCandidate peer candidate
            model, Cmd.none
        else
            { model with WaitingCandidates = candidate::model.WaitingCandidates }, Cmd.none
    | WebRTC (WebRTC.Msg.State state) ->
        { model with ConnectionState = state}, Cmd.none

    | SendFile file ->
        MessageTransfer.send peer (ChannelMessage.File file.Data)
        model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Field.div [ Field.IsGrouped ] [
            if model.Role = None then
                Control.p [ Control.IsExpanded ] [
                    Button.a [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> dispatch (Initialize Sender))
                    ] [
                        str "Initialize as Sender"
                    ]
                ]
                Control.p [ ] [
                    Button.a [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> dispatch (Initialize Receiver))
                    ] [
                        str "Initialize as Receiver"
                    ]
                ]
            match model.ConnectionState, model.Role with
            | WebRTC.ConnectionState.NotConnected, Some Role.Sender
            | WebRTC.ConnectionState.Connecting, Some Role.Sender ->
                Control.p [ ] [
                    Button.a [
                        Button.Color IsPrimary
                        Button.OnClick (fun _ -> dispatch Connect)
                    ] [
                        str "Connect"
                    ]
                ]
            | _, _ -> ()
        ]
        Field.div [
            Field.IsGrouped ] [
            Control.p [ Control.IsExpanded ] [
                div [   Class "card border-primary"
                        Draggable true
                        Style [ Height "100px" ]
                        OnDragOver ( fun e ->
                            e.preventDefault()
                        )
                        OnDrop ( fun e ->
                            e.preventDefault()
                            if e.dataTransfer.files.length > 0 then
                                let file = e.dataTransfer.files.[0]
                                { Name = file.name; Data = file.slice () }
                                |> SendFile
                                |> dispatch
                        )
                ] []
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                BackgroundColor "white"
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "File Transfer" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]
