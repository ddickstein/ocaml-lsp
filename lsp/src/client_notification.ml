open Import
open Types

type t =
  | TextDocumentDidOpen of DidOpenTextDocumentParams.t
  | TextDocumentDidClose of DidCloseTextDocumentParams.t
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFoldersParams.t
  | ChangeConfiguration of DidChangeConfigurationParams.t
  | Initialized
  | Exit
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgressCancel of WorkDoneProgressCancelParams.t
  | SetTrace of SetTraceParams.t
  | UnknownNotification of Jsonrpc.Notification.t

let method_ = function
  | TextDocumentDidOpen _ -> "textDocument/didOpen"
  | TextDocumentDidChange _ -> "textDocument/didChange"
  | TextDocumentDidClose _ -> "textDocument/didClose"
  | Exit -> "exit"
  | Initialized -> "initialized"
  | ChangeWorkspaceFolders _ -> "workspace/didChangeWorkspaceFolders"
  | ChangeConfiguration _ -> "workspace/didChangeConfiguration"
  | WillSaveTextDocument _ -> "textDocument/willSave"
  | DidSaveTextDocument _ -> "textDocument/didSave"
  | SetTrace _ -> "$/setTrace"
  | CancelRequest _ -> Cancel_request.meth_
  | WorkDoneProgressCancel _ -> "window/workDoneProgress/cancel"
  | UnknownNotification n -> n.method_

let yojson_of_t = function
  | TextDocumentDidOpen params ->
    Some (DidOpenTextDocumentParams.yojson_of_t params)
  | TextDocumentDidChange params ->
    Some (DidChangeTextDocumentParams.yojson_of_t params)
  | TextDocumentDidClose params ->
    Some (DidCloseTextDocumentParams.yojson_of_t params)
  | Exit -> None
  | Initialized -> None
  | ChangeWorkspaceFolders params ->
    Some (DidChangeWorkspaceFoldersParams.yojson_of_t params)
  | ChangeConfiguration params ->
    Some (DidChangeConfigurationParams.yojson_of_t params)
  | WillSaveTextDocument params ->
    Some (WillSaveTextDocumentParams.yojson_of_t params)
  | DidSaveTextDocument params ->
    Some (DidSaveTextDocumentParams.yojson_of_t params)
  | CancelRequest params -> Some (Cancel_request.yojson_of_t params)
  | WorkDoneProgressCancel params ->
    Some (WorkDoneProgressCancelParams.yojson_of_t params)
  | SetTrace params -> Some (SetTraceParams.yojson_of_t params)
  | UnknownNotification n -> (n.params :> Json.t option)

let of_jsonrpc (r : Jsonrpc.Notification.t) =
  let open Result.O in
  let params = r.params in
  match r.method_ with
  | "textDocument/didOpen" ->
    let+ params =
      Json.message_params params DidOpenTextDocumentParams.t_of_yojson
    in
    TextDocumentDidOpen params
  | "textDocument/didChange" ->
    let+ params =
      Json.message_params params DidChangeTextDocumentParams.t_of_yojson
    in
    TextDocumentDidChange params
  | "textDocument/didClose" ->
    let+ params =
      Json.message_params params DidCloseTextDocumentParams.t_of_yojson
    in
    TextDocumentDidClose params
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    let+ params =
      Json.message_params params DidChangeWorkspaceFoldersParams.t_of_yojson
    in
    ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    let+ params =
      Json.message_params params DidChangeConfigurationParams.t_of_yojson
    in
    ChangeConfiguration params
  | "textDocument/willSave" ->
    let+ params =
      Json.message_params params WillSaveTextDocumentParams.t_of_yojson
    in
    WillSaveTextDocument params
  | "textDocument/didSave" ->
    let+ params =
      Json.message_params params DidSaveTextDocumentParams.t_of_yojson
    in
    DidSaveTextDocument params
  | m when m = Cancel_request.meth_ ->
    let+ params = Json.message_params params Cancel_request.t_of_yojson in
    CancelRequest params
  | "window/workDoneProgress/cancel" ->
    let+ params =
      Json.message_params params WorkDoneProgressCancelParams.t_of_yojson
    in
    WorkDoneProgressCancel params
  | "$/setTrace" ->
    let+ params = Json.message_params params SetTraceParams.t_of_yojson in
    SetTrace params
  | _ -> Ok (UnknownNotification r)

let to_jsonrpc t =
  let method_ = method_ t in
  let params = yojson_of_t t |> Option.map Jsonrpc.Structured.t_of_yojson in
  { Jsonrpc.Notification.params; method_ }
