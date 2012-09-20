open Common
open Gdefs
open Printf

let dump_regs regs = 
    let l = List.map (fun r->dump_reg r) regs in
    let f acc s = acc^";"^s in
    let s = List.fold_left f "" l in
    sprintf "[%s]" s

let dump_gmeta gm = 
    match gm with
    | GMeta(gadget, fm, modified_regs, stack_fix) ->
        let gs = dump_gadget gadget in
        let fs = dump_filemeta fm in
        let modified = sprintf "modified: %s" (dump_regs modified_regs) in
        let sf = sprintf "stack_fix: %d" stack_fix in
        let s = sprintf "%s, %s, %s, %s" gs fs modified sf in
        s
let dump_container c = 
    let GContainer(fn, (data_s, data_e), gm_list) = c in
    let fs = 
        sprintf "### fn: %s, data_s: 0x%08x, data_e: 0x%08x" fn data_s data_e in
    let l = List.map dump_gmeta gm_list in
    let l = fs::l in
    (* string concat would be O(n^2) *)
    let glue acc s = Buffer.add_string acc s; Buffer.add_string acc "\n"; acc in
    let b = List.fold_left glue (Buffer.create 32) l in
    b

let main () =
    let argc = Array.length Sys.argv in
    if argc > 1 then
        let fn = Sys.argv.(1) in
        let container = Common.unmarshal_from_file fn in
        let b = dump_container container in
        Buffer.output_buffer stdout b
    else 
        let err = Printf.sprintf "Usage:\n%s <bin file>\n" Sys.argv.(0) in
        print_string err

let _ = main()
