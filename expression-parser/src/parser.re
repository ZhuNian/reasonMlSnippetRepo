open ExpressionEvaluator;

type result('success, 'error) =
  | Ok('success)
  | Error('error);
type parser('result) = string => result(('result, string), string);

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
  }

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
    (_) =>
      run(p2, input)
      |@$ (
        ((v2, remaining2)) => {
          Ok((v2, remaining2));
        }
      )
  );

let isChar = char => input => char == input
let isNumberChar = input => input |. Js.Re.test([%re "/\d/"])
let rec many =(p: parser('a), input) =>
  input
  |> run(p)
  |@$ (
    ((v1, remaining)) => {
      let p2 = many(p);
      switch(run(p2, remaining)) {
        | Ok((v2, remaining2)) => 
            let _ = Js.Array.unshift(v1, v2);
            let res = (v2, remaining2);
            Ok(res); 
        | Error(_) => Ok(([|v1|], remaining))
      }
    }
  )

  let (>>=) = (p, func: ('a => 'b), input) => {
    input
    |> run(p)
    |@$ (
      ((v, remaining)) => Ok((func(v), remaining))
    )
  }

 let empty = fun
  | s when s |> Js.String.length == 0 => Ok((s, ""))
  | _ => Error("parse empty error")

  let rec parser_ = (input: string) => {
    let numberParser = isNumberChar |> pChar |> many >>= res =>res |> Js.Array.join |> float_of_string |. Number
    let parenParser = pChar("(" |> isChar) &> parser_ &> pChar(")" |> isChar) >>= (((_, v), _)) => Paren(v)

    let valueParser = numberParser &| parenParser
    let opParser = pChar(c=>c == "+" || c == "-") >>= (c) => (n1, n2)=> c == "+" ? Add(n1, n2):Minus(n1, n2)
    let addParser = valueParser &> (opParser &> valueParser |> many) >>= ((expr, opValPairs)) => opValPairs |> Js.Array.reduce(
      (res, (op, expr))=>{
        op(res, expr)
      },
      expr
    )
    /* and minusParser = parser_ &> pChar("-" |> isChar) &> parser_ >>= (((v1, _), v3)) => Minus(v1, v3) */
    run(addParser &| valueParser, input)
  }

let parser = parser_ &> empty >>= ((exp, _)) => exp
  

/* run(pChar("A" |> isChar) &> pChar("B" |> isChar) &> pChar("D" |> isChar), "ABDC") |> Js.log; */
/* run(pChar("A" |> isChar) &| pChar("B" |> isChar) &| pChar("D" |> isChar) |> many, "ABEDC") |> Js.log; */
/* run(many(pChar(isNumberChar)), "1gg2aa3") |> Js.log; */
/* run(pChar("(" |> isChar) &> many(pChar(isNumberChar)) &> pChar(")" |> isChar), "(12)aa3") |> Js.log; */
let exp = "1+2+(5-4)"
/* run(parser_, exp) |> Js.log */
/* run(empty, "111") |> Js.log */
switch(run(parser, exp)) {
  |Ok((v1, _)) => eval(v1) |> Js.log
  |Error(e) => e |> Js.log
}
