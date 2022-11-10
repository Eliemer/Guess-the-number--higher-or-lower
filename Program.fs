open System

let MAX_ATTEMPTS = 5

[<Literal>]
let WELCOME_MESSAGE =
    "Hello!
I'm gonna think of a number between 1 and 100 (inclusive).
You will write the number you think it is.
I will tell you if:
\t- The number you guessed is TOO HIGH, TOO LOW, or exactly my secret number
\t- If you guess it right, I'll send you CONGRATULATIONS!"

type GameState =
    | Failure
    | Success
    | Ongoing

let rec round (secret: int) (state: GameState) (attempts: int) : GameState =
    match state with
    | Ongoing ->
        printf "Write your guess:"
        let guessStr = Console.ReadLine()

        match Int32.TryParse guessStr with
        | false, _ ->
            printfn "Please submit a number"
            round secret Ongoing attempts

        | true, guess when guess > 0 && guess < 101 ->
            if attempts < MAX_ATTEMPTS then

                if guess < secret then
                    printfn "TOO LOW"
                    round secret Ongoing (attempts + 1)
                else if guess > secret then
                    printfn "TOO HIGH"
                    round secret Ongoing (attempts + 1)
                else if guess = secret then
                    round secret Success attempts
                else
                    Failure

            else
                round secret Failure attempts

        | true, _ ->
            printfn "Please submite a number between 1 and 100"
            round secret Ongoing attempts

    | _ -> state

[<EntryPoint>]
let main _ =

    let mutable newRound = true
    let rand = Random()

    while newRound do
        Console.Clear()
        printfn "%s" WELCOME_MESSAGE

        match round (rand.Next(1, 101)) Ongoing 0 with
        | Success -> printfn "CONGRATULATIONS you win!"
        | Failure -> printfn "Womp womp, you ran out of attempts"
        | Ongoing -> failwith "Incorrect game state: Ongoing!"

        let mutable validNewRoundResponse = false

        while not validNewRoundResponse do
            printfn "\n\nWould you like to play another round? (1: yes, 0: no)"
            let newRoundStr = Console.ReadLine()

            match Int32.TryParse newRoundStr with
            | false, _ -> printfn "Not a valid response"
            | true, n when n = 0 || n = 1 ->
                validNewRoundResponse <- true
                newRound <- n % 2 = 1
            | true, n -> printfn "Submit a number that is either 0 or 1"

    0
