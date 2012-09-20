(** SSA simplifications

This uses all supported simplifications to try to optimize as much as possible.
*)

let simp_cfg ?(liveout=[]) ?(usedc=true) ?(usesccvn=true) ?(usemisc=true) cfg =
  let cfgref = ref cfg in
  let changed = ref true in
  while !changed do
    let (cfg,c1) = if usesccvn then Sccvn.replacer ~opt:true !cfgref else (!cfgref,false) in
    let (cfg,c2) = if usedc then Deadcode.do_dce ~globals:liveout cfg else (cfg,false) in
    let (cfg,c3) = if usemisc then Ssa_simp_misc.cfg_jumpelim cfg else (cfg,false) in
    cfgref := cfg;
    changed := c1 || c2 || c3
  done;
  !cfgref
