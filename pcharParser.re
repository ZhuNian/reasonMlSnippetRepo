type result('success, 'error) =
  | Ok('success)
  | Error('error);
type parser('result) = string => result(('result, string), string);

let run = (p: parser('a), input) => p(input);
let pChar = (char: string): parser(string) =>
  (input: string) =>
    if (Js.String.charAt(0, input) == char) {
      let remaining =
        Js.String.slice(~from=1, ~to_=Js.String.length(input), input);
      let result = (char, remaining);
      Ok(result);
    } else {
      Error("pchar error");
    };

let (|@$) = (r: result('a, 'b), fn: 'a => result('c, 'd)) =>
  switch (r) {
  | Ok(r) => fn(r)
  | Error(r) => Error(r)
  };
let (&>) = (p1, p2, input) =>
  input
  |> run(p1)
  |@$ (
    ((v1, remaining)) =>
      run(p2, remaining)
      |@$ (
        ((v2, remaining2)) => {
          let res = (v1 ++ v2, remaining2);
          Ok(res);
        }
      )
  );
run(pChar("A") &> pChar("B") &> pChar("D"), "ABDC") |> Js.log;

