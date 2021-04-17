type 'a t

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module Ops : sig
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
end

val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val lift : ('a -> 'b) t -> 'a t -> 'b t

val optional : 'a t -> ('a option) t
val some : 'a t -> ('a list) t
val many : 'a t -> ('a list) t
val any : char t
val choice : ('a t) list -> 'a t
val none_of : ('a t) list -> unit t
val count : int -> 'a t -> ('a list) t
val satisfy : (char -> bool) -> char t

val char : char -> char t
val ws : unit t
val space : unit t
val digit : char t
val word : string t
val eof : unit t
val string : string -> string t

val between : 'o t -> 'c t -> 'a t -> 'a t
val parens : 'a t -> 'a t
val braces : 'a t -> 'a t
val angles : 'a t -> 'a t
val brackets : 'a t -> 'a t
val semi : unit t
val comma : unit t
val colon : unit t
val dot : unit t
val quote : unit t
val double_quotes : 'a t -> 'a t
val single_quotes : 'a t -> 'a t

val int_lit : int t
val float_lit : float t
val str_lit : string t
val char_lit : char t
val bool_lit : bool t
