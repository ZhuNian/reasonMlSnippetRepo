open ExpressionEvaluator;

type result('success, 'error) =
  | Ok('success)
  | Error('error);
type parser('result) = string => result(('result, string), string);
exception ParseError(string);

let run = (p: parser('a), input) => p(input);
let pChar = (match: string => bool): parser(string) =>
  (input: string) => {
    let char = Js.String.charAt(0, input);
    if (char |> match) {
      let remaining =
        Js.String.slice(~from=1, ~to_=Js.String.length(input), input);
      let result = (char, remaining);
      Ok(result);
    } else {
      Error("pchar error");
    };
  };

let (|@$) = (r: result('a, 'b), fn: 'a => result('c, 'd)) =>
  switch (r) {
  | Ok(r) => fn(r)
  | Error(r) => Error(r)
  };

let (|@^) = (r: result('a, 'b), fn: 'b => result('c, 'd)) =>
  switch (r) {
  | Ok(r) => Ok(r)
  | Error(r) => fn(r)
  };

let (&>) = (p1, p2, input) =>
  input
  |> run(p1)
  |@$ (
    ((v1, remaining)) =>
      run(p2, remaining)
      |@$ (
        ((v2, remaining2)) => {
          let res = ((v1, v2), remaining2);
          Ok(res);
        }
      )
  );

let (&|) = (p1, p2, input) =>
  input
  |> run(p1)
  |@^ (
    _ => run(p2, input) |@$ (((v2, remaining2)) => Ok((v2, remaining2)))
  );

let (>>=) = (p, func: 'a => 'b, input) =>
  input
  |> run(p)
  |@$ (
    ((v, remaining)) =>
      try (Ok((func(v), remaining))) {
      | ParseError(message) => Error(message)
      }
  );

let isChar = (char, input) => char == input;
let isNumberChar = input => input->(Js.Re.test([%re "/\d/"]));
let rec many = (p: parser('a), input) =>
  input
  |> run(p)
  |@$ (
    ((v1, remaining)) => {
      let p2 = many(p);
      switch (run(p2, remaining)) {
      | Ok((v2, remaining2)) =>
        let _ = Js.Array.unshift(v1, v2);
        let res = (v2, remaining2);
        Ok(res);
      | Error(_) => Ok(([|v1|], remaining))
      };
    }
  );
let empty =
  fun
  | s when s |> Js.String.length == 0 => Ok((s, ""))
  | _ as e => Error(e);

let numberParser =
  isNumberChar
  |> pChar
  |> many
  >>= (res => (res |> Js.Array.joinWith("") |> float_of_string)->Number);

let parenParser = p =>
  pChar("(" |> isChar)
  &> p
  &> pChar(")" |> isChar)
  >>= ((((_, v), _)) => Paren(v));

let valueParser = p => numberParser &| parenParser(p);
type opType =
  | AddMinus
  | MulDiv;
let opParser = opType =>
  pChar(c =>
    switch (opType) {
    | AddMinus => c == "+" || c == "-"
    | MulDiv => c == "*" || c == "/"
    }
  )
  >>= (
    (c, n1, n2) =>
      switch (c) {
      | "+" => Add(n1, n2)
      | "-" => Minus(n1, n2)
      | "*" => Multiply(n1, n2)
      | "/" => Divid(n1, n2)
      | _ => raise(ParseError("op parser error"))
      }
  );

let computeParser = (opType, expressionParser) =>
  expressionParser
  &> (opParser(opType) &> expressionParser |> many)
  >>= (
    ((expr, opValPairs)) =>
      opValPairs
      |> Js.Array.reduce((res, (op, expr)) => op(res, expr), expr)
  );
let genAddminusParser = AddMinus->computeParser;
let genMulDivParser = MulDiv->computeParser;

let makeParser = (genParser, valueParser, parser) => {
  let expressionParser = valueParser(parser);
  genParser(expressionParser) &| expressionParser;
};

let rec parser_ = input => {
  let mulDivParser = makeParser(genMulDivParser, valueParser, parser_);
  input |> makeParser(genAddminusParser, p => mulDivParser &| valueParser(p), parser_)
};
let parser = parser_ &> empty >>= (((exp, _)) => exp);

let exp = "5*(1+1)";

switch (run(parser, exp)) {
| Ok((v1, _)) => eval(v1) |> Js.log
| Error(e) => e |> Js.log
};