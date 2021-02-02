let string_of_charlist cs = List.to_seq cs |> String.of_seq

type 'a parser = { run : string -> ('a * string) option }

let ( >> ) f g x = g (f x)

let ( <$> ) f p = { run = fun s ->
    match p.run s with
    | Some (a, s') -> Some (f a, s')
    | None -> None
  }

let ( <|> ) p1 p2 = { run = fun s ->
    match p1.run s with
    | Some res -> Some res
    | None -> p2.run s
  }

let ( >>= ) ap abp = { run = fun s ->
    Option.bind (ap.run s) (fun (a, s') -> (abp a).run s')
  }

let ( <*> ) abp ap = { run = fun s ->
    Option.bind (abp.run s) (fun (f, s') ->
        Option.map (fun (a, s'') -> (f a, s'')) (ap.run s'))
  }

let ( <* ) ap bp = { run = fun s ->
    match ap.run s with
    | Some (a, s') -> Option.map (fun (_, s'') -> (a, s'')) (bp.run s')
    | None -> None
  }

let ( *> ) ap bp = { run = fun s ->
    match ap.run s with
    | Some (_, s') -> Option.map (fun (b, s'') -> (b, s'')) (bp.run s')
    | None -> None
  }

let map = ( <$> )

let bind = ( >>= )

let lift = ( <*> )

let pure a = { run = fun s -> Some (a, s) }

let void p = p *> pure ()

let empty = { run = fun _ -> None }

let optional p = { run = fun s ->
    match p.run s with
    | Some (a, s') -> Some (Some(a), s')
    | None -> Some (None, s)
  }

let choice ps = List.fold_left ( <|> ) empty ps

let many p =
  let rec f acc s = match p.run s with
    | Some (a, s') -> f (a :: acc) s'
    | None -> Some (acc, s) in
  { run = fun s -> f [] s }

let some p = p >>= fun a -> (List.cons a) <$> many p

let any = { run = fun s ->
    match s with
    | "" -> None
    | _ -> Some (s.[0], String.sub s 1 (String.length s - 1))
  }

let none_of ps = { run = fun s ->
    match (choice ps).run s with
    | None -> Some ((), s)
    | _ -> None
  }

let count n p =
  let rec f n acc = match n with
    | 0 -> pure acc
    | _ -> p >>= (fun x -> f (n - 1) (acc @ [x])) in
  f n []

let satisfy f = any >>= (fun c -> if f c then pure c else empty)

let char c = satisfy ((==) c)

let ws = void (char ' ' <|> char '\n' <|> char '\t')

let space = void (char ' ')

let digit = satisfy (function
    | '0'..'9' -> true
    | _ -> false)

let word =
  let alpha = satisfy (function 'a'..'z' | 'A'..'Z' -> true | _ -> false) in
  let alphanum = alpha <|> digit in
  alpha
  >>= fun c -> some alphanum <|> pure []
  >>= fun cs -> pure (string_of_charlist (c :: cs))

let eof = { run = function
    | "" -> Some ((), "")
    | _ -> None
  }

let string str = { run = fun s ->
    let len_str = String.length str in
    let len_input = String.length s in
    if len_str > len_input
    then None
    else if String.equal str (String.sub s 0 len_str)
    then Some(str, String.sub s len_str (len_input - len_str))
    else None
  }

let between o c p = o *> p <* c

let parens p = between (char '(') (char ')') p

let braces p = between (char '{') (char '}') p

let angles p = between (char '<') (char '>') p

let brackets p = between (char '[') (char ']') p

let semi = void (char ';')

let comma = void (char ',')

let colon = void (char ':')

let dot = void (char '.')

let double_quotes p = between (char '"') (char '"') p

let single_quotes p = between (char '\'') (char '\'') p

let int_lit = (string_of_charlist >> int_of_string) <$> (some digit)

let float_lit =
  let make_float i dec = float_of_string (string_of_int i ^ "." ^ string_of_int dec) in
  int_lit >>= fun i -> make_float i <$> (dot *> int_lit)

let str_lit = double_quotes (string_of_charlist <$> many (satisfy ((!=) '"')))

let char_lit = single_quotes (satisfy ((!=) '\''))

let bool_lit = (string "True" *> pure true) <|> (string "False" *> pure false)
