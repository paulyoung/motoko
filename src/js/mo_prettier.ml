(*
  Provides a parser which produces an AST in a format that is convenient to use
  with the Prettier code formatter.
*)
module Js = Js_of_ocaml.Js
module Syntax = Mo_def.Syntax
module Trivia = Mo_def.Trivia

open Source
open Syntax
open Trivia

type position = <
  line : int Js.readonly_prop; (* >= 1 *)
  column : int Js.readonly_prop; (* >= 0 *)
> Js.t

type source_location = <
  start : position Js.readonly_prop;
  _end : position Js.readonly_prop;
> Js.t

type comment = <
  text : (Js.js_string Js.t) Js.readonly_prop;
  leading : (bool Js.t) Js.readonly_prop;
  trailing : (bool Js.t) Js.readonly_prop;
  printed : (bool Js.t) Js.readonly_prop;
> Js.t

(* TODO: can all node type declarations inherit these fields? *)
type node = <
  _type : (Js.js_string Js.t) Js.readonly_prop;
  loc : source_location Js.readonly_prop;
  comments : ((comment Js.js_array) Js.t) Js.readonly_prop;
> Js.t

type id_node = <
  _type : (Js.js_string Js.t) Js.readonly_prop;
  loc : source_location Js.readonly_prop;
  comments : ((comment Js.js_array) Js.t) Js.readonly_prop;
  identifier : (Js.js_string Js.t) Js.readonly_prop;
> Js.t

type lit_node = <
  _type : (Js.js_string Js.t) Js.readonly_prop;
  loc : source_location Js.readonly_prop;
  comments : ((comment Js.js_array) Js.t) Js.readonly_prop;
  literal : (Js.js_string Js.t) Js.readonly_prop;
> Js.t

type prog_node = <
  _type : (Js.js_string Js.t) Js.readonly_prop;
  loc : source_location Js.readonly_prop;
  comments : ((comment Js.js_array) Js.t) Js.readonly_prop;
  declarations : ((node Js.js_array) Js.t) Js.readonly_prop;
> Js.t

type top_level_node = <
  _type : (Js.js_string Js.t) Js.readonly_prop;
  loc : source_location Js.readonly_prop;
  comments : ((comment Js.js_array) Js.t) Js.readonly_prop;
  program : prog_node Js.readonly_prop;
> Js.t

let position_of_pos (pos : Source.pos) : position =
  object%js
    val line = pos.line
    val column = if pos.column > 0 then pos.column - 1 else 0
  end

let source_location_of_region (region : Source.region) : source_location =
  object%js
    val start = position_of_pos region.left
    val _end = position_of_pos region.right
  end

let leading_comment (text : string) : comment =
  object%js
    val text = Js.string text
    val leading = Js._true
    val printed = Js._false
    val trailing = Js._false
  end

let trailing_comment (text : string) : comment =
  object%js
    val text = Js.string text
    val leading = Js._false
    val trailing = Js._true
    val printed = Js._false
  end

let comments_of_triv_table (triv_table : Trivia.triv_table) (region : Source.region) : ((comment Js.js_array) Js.t) =
  let trivia_info = Trivia.find_trivia triv_table region in

  let leading_comments =
    List.filter_map
      (function Comment text -> Some (leading_comment text) | _ -> None)
      trivia_info.leading_trivia
  in

  let trailing_comments =
    List.filter_map
      (function Comment text -> Some (trailing_comment text) | _ -> None)
      trivia_info.trailing_trivia
  in

  let comments = List.append leading_comments trailing_comments in

  Js.array (Array.of_list comments)

let node_of_id (triv_table : Trivia.triv_table) (id : Syntax.id) : id_node =
  object%js
    val _type = Js.string "Identifier"
    val loc = source_location_of_region id.at
    val comments = comments_of_triv_table triv_table id.at
    val identifier = Js.string id.it
  end

let rec node_of_exp (triv_table : Trivia.triv_table) (exp : Syntax.exp) : node =
  let loc = source_location_of_region exp.at in
  let comments = comments_of_triv_table triv_table exp.at in
  match exp.it with
  | PrimE prim ->
      (object%js
        val _type = Js.string "PrimitiveExpression"
        val loc = loc
        val comments = comments
        val primitive = prim
      end :> node)
  | VarE id ->
      (object%js
        val _type = Js.string "VariableExpression"
        val loc = loc
        val comments = comments
        val identifier = node_of_id triv_table id
      end :> node)
  | LitE { contents = lit } ->
      (object%js
        val _type = Js.string "LiteralExpression"
        val loc = loc
        val comments = comments
        val literal = Js.string (Syntax.string_of_lit lit)
      end :> node)
  | ActorUrlE exp ->
      (object%js
        val _type = Js.string "ActorReferenceExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | UnE (_op_typ, _unop, _exp) ->
      (object%js
        val _type = Js.string "UnaryOperatorExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | BinE (_op_typ, _exp1, _binop, _exp2) ->
      (object%js
        val _type = Js.string "BinaryOperatorExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val left = node_of_exp triv_table exp1 *)
        (* val right = node_of_exp triv_table exp2 *)
      end :> node)
  | RelE (_op_typ, _exp1, _relop, _exp2) ->
      (object%js
        val _type = Js.string "RelationalOperatorExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val left = node_of_exp triv_table exp1 *)
        (* val right = node_of_exp triv_table exp2 *)
      end :> node)
  | ShowE (_op_typ, _exp) ->
      (object%js
        val _type = Js.string "DebugShowOperatorExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | TupE exp_list ->
      let exps = List.map (node_of_exp triv_table) exp_list in
      (object%js
        val _type = Js.string "TupleExpression"
        val loc = loc
        val comments = comments
        val expressions = Js.array (Array.of_list exps)
      end :> node)
  | ProjE (_exp, _int) ->
      (object%js
        val _type = Js.string "TupleProjectionExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | OptE exp ->
      (object%js
        val _type = Js.string "OptionInjectionExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | DoOptE exp ->
      (object%js
        val _type = Js.string "OptionMonadExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | BangE exp ->
      (object%js
        val _type = Js.string "ScopedOptionProjectionExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | ObjBlockE (_obj_sort, _dec_field_list) ->
      (object%js
        val _type = Js.string "ObjectBlockExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | ObjE _exp_field_list ->
      (object%js
        val _type = Js.string "RecordLiteralExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | TagE (id, exp) ->
      (object%js
        val _type = Js.string "VariantExpression"
        val loc = loc
        val comments = comments
        val identifier = node_of_id triv_table id
        val expression = node_of_exp triv_table exp
      end :> node)
  | DotE (exp, id) ->
      (object%js
        val _type = Js.string "ObjectProjectionExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
        val identifier = node_of_id triv_table id
      end :> node)
  | AssignE (exp1, exp2) ->
      (object%js
        val _type = Js.string "AssignmentExpression"
        val loc = loc
        val comments = comments
        val left = node_of_exp triv_table exp1
        val right = node_of_exp triv_table exp2
      end :> node)
  | ArrayE (_mut, _exp_list) ->
      (object%js
        val _type = Js.string "ArrayExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | IdxE (exp1, exp2) ->
      (object%js
        val _type = Js.string "ArrayIndexingExpression"
        val loc = loc
        val comments = comments
        val left = node_of_exp triv_table exp1
        val right = node_of_exp triv_table exp2
      end :> node)
  | FuncE (_string, _sort_pat, _typ_bind_list, _pat, _typ, _sugar, _exp) ->
      (object%js
        val _type = Js.string "FunctionExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | CallE (exp1, inst, exp2) ->
      (object%js
        val _type = Js.string "FunctionCallExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | BlockE dec_list ->
      let decs = List.map (node_of_dec triv_table) dec_list in
      (object%js
        val _type = Js.string "BlockExpression"
        val loc = loc
        val comments = comments
        val declarations = Js.array (Array.of_list decs)
      end :> node)
  | NotE exp ->
      (object%js
        val _type = Js.string "NotExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | AndE (exp1, exp2) ->
      (object%js
        val _type = Js.string "AndExpression"
        val loc = loc
        val comments = comments
        val left = node_of_exp triv_table exp1
        val right = node_of_exp triv_table exp2
      end :> node)
  | OrE (exp1, exp2) ->
      (object%js
        val _type = Js.string "OrExpression"
        val loc = loc
        val comments = comments
        val left = node_of_exp triv_table exp1
        val right = node_of_exp triv_table exp2
      end :> node)
  | IfE (exp1, exp2, exp3) ->
      (object%js
        val _type = Js.string "IfExpression"
        val loc = loc
        val comments = comments
        val condition = node_of_exp triv_table exp1
        val consequent = node_of_exp triv_table exp2
        val alternative = node_of_exp triv_table exp3
      end :> node)
  | SwitchE (_exp, case_list) ->
      (object%js
        val _type = Js.string "SwitchExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | WhileE (_exp1, _exp2) ->
      (object%js
        val _type = Js.string "WhileDoLoopExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | LoopE (_exp, _exp_option) ->
      (object%js
        val _type = Js.string "DoWhileLoopExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | ForE (_pat, _exp1, _exp2) ->
      (object%js
        val _type = Js.string "IterationExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
      end :> node)
  | LabelE (_id, _typ, _exp) ->
      (object%js
        val _type = Js.string "LabelExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val identifier = node_of_id triv_table id *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | BreakE (id, exp) ->
      (object%js
        val _type = Js.string "BreakExpression"
        val loc = loc
        val comments = comments
        val identifier = node_of_id triv_table id
        val expression = node_of_exp triv_table exp
      end :> node)
  | RetE exp ->
      (object%js
        val _type = Js.string "ReturnExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | DebugE exp ->
      (object%js
        val _type = Js.string "DebuggingExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | AsyncE (_typ_bind, _exp) ->
      (object%js
        val _type = Js.string "AsyncExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | AwaitE exp ->
      (object%js
        val _type = Js.string "AwaitExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | AssertE exp ->
      (object%js
        val _type = Js.string "AssertionExpression"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | AnnotE (_exp, _typ) ->
      (object%js
        val _type = Js.string "TypeAnnotationExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        val expression = node_of_exp triv_table exp
      end :> node)
  | ImportE (_string, { contents = _resolved_import }) ->
      (object%js
        val _type = Js.string "ImportStatementExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        val expression = node_of_exp triv_table exp
      end :> node)
  | ThrowE exp ->
      (object%js
        val _type = Js.string "ThrowExceptionExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        val expression = node_of_exp triv_table exp
      end :> node)
  | TryE (_exp, _case_list) ->
      (object%js
        val _type = Js.string "CatchExceptionExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        val expression = node_of_exp triv_table exp
      end :> node)
  | IgnoreE exp ->
      (object%js
        val _type = Js.string "IgnoreExpression"
        val loc = loc
        val comments = comments
        (* FIXME: missing fields *)
        val expression = node_of_exp triv_table exp
      end :> node)

and node_of_dec (triv_table : Trivia.triv_table) (dec : Syntax.dec) : node =
  let loc = source_location_of_region dec.at in
  let comments = comments_of_triv_table triv_table dec.at in
  match dec.it with
  | ExpD exp ->
      (object%js
        val _type = Js.string "ExpressionDeclaration"
        val loc = loc
        val comments = comments
        val expression = node_of_exp triv_table exp
      end :> node)
  | LetD (_pat, _exp) ->
      (object%js
        val _type = Js.string "LetDeclaration"
        val loc = loc
        val comments = comments
        (* val pattern = _ *)
        (* val expression = node_of_exp triv_table exp *)
      end :> node)
  | VarD (id, exp) ->
      (object%js
        val _type = Js.string "VarableDeclaration"
        val loc = loc
        val comments = comments
        val identifier = node_of_id triv_table id
        val expression = node_of_exp triv_table exp
      end :> node)
  | TypD (_typ_id, _typ_binds, _typ) ->
      (object%js
        val _type = Js.string "TypeDeclaration"
        val loc = loc
        val comments = comments
        (* val type_identifier = _ *)
        (* val type_bindings = _ *)
        (* val _type = _ *)
      end :> node)
  | ClassD (_sort_pat, _typ_id, _typ_binds, _pat, _typ, _obj_sort, _id, _dec_fields) ->
      (object%js
        val _type = Js.string "ClassDeclaration"
        val loc = loc
        val comments = comments
        (* val sort_pattern = _*)
        (* val type_identifier = _*)
        (* val type_bindings = _*)
        (* val pattern = _*)
        (* val _type = _*)
        (* val object_sort = _*)
        (* val identifier = node_of_id triv_table id *)
        (* val declaration_fields = _*)
      end :> node)

let node_of_prog (prog : Syntax.prog) : prog_node =
  let decs = List.map (node_of_dec prog.note.trivia) prog.it in
  object%js
    val _type = Js.string "Program"
    val loc = source_location_of_region prog.at
    val comments = comments_of_triv_table prog.note.trivia prog.at
    val declarations = Js.array (Array.of_list decs)
  end

let parse (filepath : Js.js_string Js.t) (source : Js.js_string Js.t) : top_level_node Js.opt =
  let parse_result =
    Pipeline.parse_string (Js.to_string filepath) (Js.to_string source) in
  match parse_result with
  | Ok ((prog, _name), msgs) ->
      let prog_node = node_of_prog prog in
      (*
        If the top-level node contains a `comments` property Prettier assumes
        that it should process the contents of that field. Since we are
        attaching comments manually we introduce a top-level wrapper node to
        prevent this unwanted behavior.
      *)
      Js.some (object%js
        val _type = Js.string "TopLevel"
        val loc = prog_node##.loc
        val comments = Js.array (Array.of_list [])
        val program = prog_node
      end)

  | Error _msgs ->
      (* FIXME: report errors *)
      Js.null

let _ = Js.export "parse" parse
