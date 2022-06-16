open Stdune
open Fiber.O

let _PATH =
  Bin.parse_path (Option.value ~default:"" @@ Env.get Env.initial "PATH")

let bin = Bin.which "ocamllsp" ~path:_PATH |> Option.value_exn |> Path.to_string

let env = Spawn.Env.of_list [ "OCAMLLSP_TEST=true" ]

module Client = Lsp_fiber.Client
open Lsp.Types

let%expect_test "start and stop server" =
  let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
  let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
  let pid =
    Spawn.spawn ~env ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o ()
  in
  Unix.close stdin_i;
  Unix.close stdout_o;
  let handler = Client.Handler.make () in
  let init =
    let* io =
      let blockity =
        if Sys.win32 then `Blocking
        else (
          Unix.set_nonblock stdout_i;
          Unix.set_nonblock stdin_o;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* in_ = make stdout_i Input in
      let+ out = make stdin_o Output in
      Lsp_fiber.Fiber_io.make in_ out
    in
    let client = Client.make handler io () in
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let print_init =
      let+ resp = Client.initialized client in
      InitializeResult.yojson_of_t resp
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline
    in
    Fiber.fork_and_join_unit run_client (fun () ->
        print_init
        >>> Fiber.all_concurrently_unit
              [ Lsp_fiber.Fiber_io.close io `Read
              ; Lsp_fiber.Fiber_io.close io `Write
              ; Client.stop client
              ; Fiber.of_thunk (fun () ->
                    Fiber.return @@ Unix.kill pid Sys.sigint)
              ])
  in
  let waiting = ref true in
  let waitpid =
    let+ (_ : Unix.process_status) = Lev_fiber.waitpid ~pid in
    waiting := false;
    ()
  in
  let timeout =
    let+ () = Lev_fiber.Timer.sleepf 2.0 in
    if !waiting then (
      print_endline "timeout";
      Unix.kill pid Sys.sigint)
  in
  Lev_fiber.run
    ~f:(fun () -> Fiber.all_concurrently_unit [ init; waitpid; timeout ])
    (Lev.Loop.default ());
  [%expect
    {|
    {
      "capabilities": {
        "textDocumentSync": {
          "openClose": true,
          "change": 2,
          "willSave": false,
          "willSaveWaitUntil": false,
          "save": true
        },
        "completionProvider": {
          "triggerCharacters": [ ".", "#" ],
          "resolveProvider": true
        },
        "hoverProvider": true,
        "signatureHelpProvider": {
          "triggerCharacters": [ " ", "~", "?", ":", "(" ]
        },
        "declarationProvider": true,
        "definitionProvider": true,
        "typeDefinitionProvider": true,
        "referencesProvider": true,
        "documentHighlightProvider": true,
        "documentSymbolProvider": true,
        "codeActionProvider": {
          "codeActionKinds": [
            "quickfix", "construct", "destruct", "inferred_intf",
            "put module name in identifiers",
            "remove module name from identifiers", "type-annotate"
          ]
        },
        "codeLensProvider": { "resolveProvider": false },
        "documentFormattingProvider": true,
        "renameProvider": { "prepareProvider": true },
        "foldingRangeProvider": true,
        "executeCommandProvider": { "commands": [ "dune/promote" ] },
        "selectionRangeProvider": true,
        "workspaceSymbolProvider": true,
        "workspace": {
          "workspaceFolders": { "supported": true, "changeNotifications": true }
        },
        "experimental": {
          "ocamllsp": {
            "interfaceSpecificLangId": true,
            "handleSwitchImplIntf": true,
            "handleInferIntf": true,
            "handleTypedHoles": true,
            "handleWrappingAstNode": true,
            "diagnostic_promotions": true
          }
        }
      },
      "serverInfo": { "name": "ocamllsp", "version": "dev" }
    }
    timeout |}]
