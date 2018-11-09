type expr =
  | Number(float)
  | Add(expr, expr)
  | Minus(expr, expr)
  | Multiply(expr, expr)
  | Divid(expr, expr)
  | Paren(expr)


let rec eval = fun 
  | Number(r) => r
  | Add(r1, r2) => eval(r1) +. eval(r2)
  | Minus(r1, r2) => eval(r1) -. eval(r2)
  | Multiply(r1, r2) => eval(r1) *. eval(r2)
  | Divid(r1, r2) => eval(r1) /. eval(r2)
  | Paren(r) => eval(r)

let getBlanks = (indent: int) => {
  let indentWidth = 2;
  " " |> Js.String.repeat(indent * indentWidth)
} 

let getLine = ( indent: int, input: string) => getBlanks(indent) |> Js.String.concat(input)

let rec print_ = (indent: int, input: expr) => {
  let getLineWithIndent = getLine(indent)
  switch(input) {
    | Number(v) => "Number(" ++ (v |> string_of_float) ++ ")" |> getLineWithIndent |> Js.log
    | Add(exp1, exp2) => 
      "Add(" |> getLineWithIndent |> Js.log;
      exp1 |> print_(indent + 1);
      "," |> getLineWithIndent |> Js.log;
      exp2 |> print_(indent + 1);
      ")" |> getLineWithIndent |> Js.log;
    | Minus(exp1, exp2) =>
      "Minus(" |> getLineWithIndent |> Js.log;
      exp1 |> print_(indent + 1);
      "," |> getLineWithIndent |> Js.log;
      exp2 |> print_(indent + 1);
      ")" |> getLineWithIndent |> Js.log; 
    | Multiply(exp1, exp2) =>
     "Multiply(" |> getLineWithIndent |> Js.log;
      exp1 |> print_(indent + 1);
      "," |> getLineWithIndent |> Js.log;
      exp2 |> print_(indent + 1);
      ")" |> getLineWithIndent |> Js.log; 
    | Divid(exp1, exp2) =>
     "Divid(" |> getLineWithIndent |> Js.log;
      exp1 |> print_(indent + 1);
      "," |> getLineWithIndent |> Js.log;
      exp2 |> print_(indent + 1);
      ")" |> getLineWithIndent |> Js.log;
    | Paren(exp) => 
      "(" |> getLineWithIndent |> Js.log;
      exp |> print_(indent + 1);
      ")" |> getLineWithIndent |> Js.log;
  }
}  
let print = expr => expr |> print_(0)


