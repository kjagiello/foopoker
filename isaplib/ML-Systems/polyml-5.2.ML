(*  Title:      Pure/ML-Systems/polyml.ML

Compatibility wrapper for Poly/ML 5.2 and 5.2.1.
*)

open Thread;

structure ML_Name_Space =
struct
  open PolyML.NameSpace;
  type T = PolyML.NameSpace.nameSpace;
  val global = PolyML.globalNameSpace;
end;

use "ML-Systems/polyml_common.ML";

use "ML-Systems/multithreading_polyml.ML";

val pointer_eq = PolyML.pointerEq;

fun share_common_data () = PolyML.shareCommonData PolyML.rootFunction;

use "ML-Systems/compiler_polyml-5.2.ML";
use "ML-Systems/pp_polyml.ML";
