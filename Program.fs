// A probability is a float between 0.0 and 1.0.
type Probability = Probability of float

// A probabilistic regular expression can be characters, concatenations,
// Choice (|) operations and Kleene-star (*) operations.
type ProbRegEx =
  | Char of char
  | Concat of ProbRegEx * ProbRegEx
  | Choice of ProbRegEx * ProbRegEx * Probability
  | Kleene of ProbRegEx * Probability
  | Option of ProbRegEx * Probability
  | Plus of ProbRegEx * Probability

// Constructor for the Probability type.
let CreateProbability (f : float) =
  if f > 0.0 && f < 1.0
    then Probability(f)
    else failwith "Invalid value for probability."

let rnd = System.Random() // RNG.

// Takes a probabilistic regular expression and returns a string made from the given regular expression, while respecting probabilities.
let rec generateString (s : ProbRegEx) =
  match s with
  | Char(c) -> string c
  | Concat(a, b) -> (generateString a) + (generateString b)
  | Choice(a, b, Probability(p)) -> if rnd.NextDouble() < p then generateString a else generateString b
  | Kleene(a, Probability(p)) -> if rnd.NextDouble() < p then "" else (generateString a) + (generateString s)
  | Option(a, Probability(p)) -> if rnd.NextDouble() < p then generateString a else ""
  | Plus(a, Probability(p)) -> if rnd.NextDouble() < p then generateString a else (generateString a) + (generateString s)

// Takes a probabilistic regular expression, an integer (n), and a string list.
// Generates n strings from the regular expression and returns them as a string list.
let rec generateStrings (s: ProbRegEx) (n : int) (ss : string list) =
  if n = 0 then ss
  else
    generateStrings s (n-1) ((generateString s) :: ss)

// Helper function for the concat case of calculateProbability
let rec concatProb (s : ProbRegEx) (t : ProbRegEx) (w : string) (i : int) (res : float) =
  if (i+1) >= w.Length then res + calculateProbability s "" * calculateProbability t w + calculateProbability s w * calculateProbability t ""
  else
    concatProb s t w (i+1) (res + calculateProbability s w.[0..i] * calculateProbability t w.[(i+1)..(w.Length-1)])
// Helper function for the Kleene case of calculateProbability
and kleeneProb (s : ProbRegEx) (t : ProbRegEx) (w : string) (i : int) (res : float) =
  if i > w.Length then res
  else
    match s with
    | Kleene(a, Probability(p)) -> kleeneProb s (Concat(a, t)) w (i+1) (res + (1.0-p)**(float i) * p * calculateProbability t w)
    | Plus(a, Probability(p)) -> kleeneProb s (Concat(a, t)) w (i+1) (res + (1.0-p)**(float i) * p * calculateProbability t w)
    | _ -> failwith "Invalid use of KleeneProb"
// Takes a probabilistic regular expression and a string. Returns the probability of the given expression deriving the given string.  
and calculateProbability (s : ProbRegEx) (w : string) =
  match (s, w) with
  | (Char(c), v) -> if string c = v then 1.0 else 0.0
  | (Choice(a, b, Probability(p)), v) -> p * (calculateProbability a v) + (1.0-p) * (calculateProbability b v)
  | (Concat(a, b), v) -> concatProb a b v 0 0.0
  | (Kleene(_, Probability(p)), "") -> p
  | (Kleene(a, Probability(p)), v) -> kleeneProb s a v 1 0.0
  | (Option(a, Probability(p)), "") -> 1.0-p
  | (Option(a, Probability(p)), v) -> p * calculateProbability a v
  | (Plus(a, Probability(p)), "") -> 0.0
  | (Plus(a, Probability(p)), v) -> kleeneProb s a v 0 0.0

// Checks the validity of a probabilistic regular expression.
let checkProbRegEx (s : ProbRegEx) =
  match s with
  | Kleene(a, p) -> if calculateProbability a "" = 0.0
                    then s
                    else failwith "Invalid expression."
  | _ -> s

// Tests.
let tests =
  // Tests of the Probability constructor.
  printfn "--CreateProbability--"
  printfn "%A" (CreateProbability(0.5))
  //printfn "%A" (CreateProbability(1.0)) // Fails with "Invalid value for probability".
  //printfn "%A" (CreateProbability(0.0)) // Fails with "Invalid value for probability".
  //printfn "%A" (CreateProbability(-0.5)) // Fails with "Invalid value for probability".

  // Tests of the validity checker (checkProbRegEx)
  printfn "--checkProbRegEx--"
  printfn "%A" (checkProbRegEx (Kleene(Char('a'), CreateProbability(0.5))))
  //printfn "%A" (checkProbRegEx (Kleene(Kleene(Char('a'), CreateProbability(0.5)), CreateProbability(0.5)))) // Fails with "Invalid expression".

  // Tests of generateStrings.
  printfn "--generateStrings--"
  printfn "Char test: %A" (generateStrings (Char('a')) 1 [])
  printfn "Concat test: %A" (generateStrings (Concat(Char('a'), Char('b'))) 2 [])
  printfn "Choice test: %A" (generateStrings (Choice(Char('a'), Char('b'), CreateProbability(0.4))) 5 [])
  printfn "Kleene test: %A" (generateStrings (Concat(Kleene(Char('a'), CreateProbability(0.3)), Char('b'))) 10 [])
  printfn "Option test: %A" (generateStrings (Concat(Option(Char('a'), CreateProbability(0.3)), Char('b'))) 15 [])
  printfn "Plus test: %A" (generateStrings (Concat(Plus(Char('a'), CreateProbability(0.3)), Char('b'))) 20 [])
  printfn "Mixed test: %A" (generateStrings (Kleene(Choice(Char('a'), Choice(Char('b'), Choice(Char('c'), Choice(Char('d'), Char('r'), Probability(0.5)), Probability(0.4)), Probability(0.3)), Probability(0.2)), Probability(0.2))) 10 [])

  // Tests of calculateProbability.
  printfn "--calculateProbability--"
  printfn "Char test0: %A" (calculateProbability (Char('c')) "c")
  printfn "Char test1: %A" (calculateProbability (Char('c')) "d")
  printfn "Concat test: %A" (calculateProbability (Concat(Char('a'), Concat(Char('b'), Char('c')))) "abc")
  printfn "Choice test0: %A" (calculateProbability (Choice(Char('a'), Char('b'), CreateProbability(0.4))) "a")
  printfn "Choice test1: %A" (calculateProbability (Choice(Char('a'), Choice(Char('b'), Char('c'), CreateProbability(0.5)), CreateProbability(0.5))) "c")
  printfn "Kleene test: %A" (calculateProbability (Kleene(Char('a'), CreateProbability(0.3))) "aaa")
  printfn "Option test: %A" (calculateProbability (Concat(Option(Char('a'), CreateProbability(0.3)), Char('b'))) "ab")
  printfn "Plus test: %A" (calculateProbability (Plus(Char('a'), CreateProbability(0.3))) "aaa")
  // abracadabra!
  printfn "Abracadabra test: %A" (calculateProbability (Kleene(Choice(Char('a'), Choice(Char('b'), Choice(Char('c'), Choice(Char('d'), Char('r'), Probability(0.5)), Probability(0.4)), Probability(0.3)), Probability(0.2)), Probability(0.2))) "abracadabra")

[<EntryPoint>]
let main argv = 
    tests
    //System.Console.ReadKey() |> ignore // For Windows users.
    0 // return an integer exit code