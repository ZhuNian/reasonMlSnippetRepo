type expr =
  | Number(float)
  | Add(expr, expr)
  | Minus(expr, expr)
  | Paren(expr)


let rec eval = fun 
  | Number(r) => r
  | Add(r1, r2) => eval(r1) +. eval(r2)
  | Minus(r1, r2) => eval(r1) -. eval(r2)
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
    | Paren(exp) => 
      "(" |> getLineWithIndent |> Js.log;
      exp |> print_(indent + 1);
      ")" |> getLineWithIndent |> Js.log;
  }
}  
let print = expr => expr |> print_(0)

/* let rec eval : type a. expr(a) => a = (input) =>
  switch (input) {
  | Value(Int(n)) => n
  | Value(Bool(b)) => b
  | Compare(expr1, expr2) =>
    let (n1, n2) = (eval(expr1), eval(expr2));
    n1 >= n2
  | If(expr1, expr2, expr3) =>
    if(eval(expr1)) {
      eval(expr2)
    }else{
      eval(expr3)
    }
  }; */
/* let expr1 =  Compare(Value(Int(3)), Value(Int(5)));
let expr2 =  If(Value(Bool(false)), Value(Int(0)), Value(Int(1)));
eval(Value(Int(3))) |> Js.log;
eval(expr1) |> Js.log;
eval(expr2) |> Js.log */

/* Add(Number(3.), Number(4.)) |> print */