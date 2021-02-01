type 'a parser

(* Operators *)
val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
val ( <* ) : 'a parser -> 'b parser -> 'a parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser

(* Name aliases for operators *)
val map : ('a -> 'b) -> 'a parser -> 'b parser
val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
val lift : ('a -> 'b) parser -> 'a parser -> 'b parser

(* Elementary abstract parsers *)
val optional : 'a parser -> unit parser
val some : 'a parser -> ('a list) parser
val many : 'a parser -> ('a list) parser
val any : char parser
val choice : ('a parser) list -> 'a parser
val none_of : ('a parser) list -> unit parser
val count : int -> 'a parser -> ('a list) parser
val satisfy : (char -> bool) -> char parser

(* Elementary value parsers *)
val char : char -> char parser
val ws : unit parser
val space : unit parser
val digit : char parser
val word : string parser
val eof : unit parser
val string : string -> string parser

(* Utility parsers *)
val between : 'o parser -> 'c parser -> 'a parser -> 'a parser
val parens : 'a parser -> 'a parser
val braces : 'a parser -> 'a parser
val angles : 'a parser -> 'a parser
val brackets : 'a parser -> 'a parser
val semi : unit parser
val comma : unit parser
val colon : unit parser
val dot : unit parser
val double_quotes : 'a parser -> 'a parser
val single_quotes : 'a parser -> 'a parser

(* Common parsers for mostly any language *)
val int_lit : int parser
val float_lit : float parser
val str_lit : string parser
val char_lit : char parser
val bool_lit : bool parser
