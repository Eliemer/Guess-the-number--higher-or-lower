module Main.Elmish

open System
open Elmish

[<Literal>]
let WELCOME_MESSAGE =
    "Hello!
I'm gonna think of a number between 1 and 100 (inclusive).
You will write the number you think it is.
I will tell you if:
\t- The number you guessed is TOO HIGH, TOO LOW, or exactly my secret number
\t- If you guess it right, I'll send you CONGRATULATIONS!"

let rand = System.Random()

type PositiveInt =
    | PositiveInt of int

    static member create x =
        if x >= 0 then Some <| PositiveInt x else None

    static member value(PositiveInt n) = n

    static member tryParse(s : string) =
        match Int32.TryParse s with
        | true, n -> PositiveInt.create n
        | false, _ -> None

let tryParseRetry s =
    if s = "1" then Ok true
    else if s = "0" then Ok false
    else Error "Not a valid response"

type GameState =
    | Welcome
    | ReadingGuess
    | Guessing
    | ReadingRetry
    | Winning
    | Losing
    | Goodbye

type Model =
    {
        State : GameState
        HiddenNumber : PositiveInt
        GuessedNumber : PositiveInt option
        MaxAttempts : PositiveInt
        Attempts : int
    }

type Msg =
    | ReadGuess
    | Guess of PositiveInt option
    | Retry of bool option
    | Win

let init (maxAttempts : PositiveInt) =
    {
        State = Welcome
        HiddenNumber = PositiveInt.create (rand.Next(0, 101)) |> Option.get
        GuessedNumber = None
        MaxAttempts = maxAttempts
        Attempts = 0
    }

let update msg model =
    match msg with
    | ReadGuess -> { model with State = ReadingGuess }
    | Guess n ->
        match n with
        | None ->
            printfn "Please submit a number"
            { model with State = ReadingGuess }
        | Some posN ->
            if (PositiveInt.value posN) <= 100 then
                if model.Attempts < PositiveInt.value model.MaxAttempts then
                    { model with
                        GuessedNumber = Some posN
                        State = Guessing
                        Attempts = model.Attempts + 1
                    }
                else
                    { model with State = Losing }
            else
                printfn "Please submit a number between 1 and 100"
                { model with State = ReadingGuess }
    | Win -> { model with State = Winning }
    | Retry b ->
        match b with
        | None ->
            printfn "Would you like to play another round? (1: yes, 0: no)"
            { model with State = ReadingRetry }
        | Some retry ->
            if retry then
                init model.MaxAttempts
            else
                { model with State = Goodbye }

let view model (dispatch : Dispatch<Msg>) =
    match model.State with
    | Welcome ->
        Console.Clear()
        Console.WriteLine WELCOME_MESSAGE
        dispatch <| Guess None
    | ReadingGuess ->
        Console.WriteLine "Write your guess:"
        Console.ReadLine() |> PositiveInt.tryParse |> Guess |> dispatch
    | Guessing ->
        match model.GuessedNumber with
        | Some posN ->
            if posN > model.HiddenNumber then
                Console.WriteLine "TOO HIGH"
                dispatch <| ReadGuess
            else if posN < model.HiddenNumber then
                Console.WriteLine "TOO LOW"
                dispatch <| ReadGuess
            else
                dispatch Win
        | None -> ()
    | ReadingRetry ->
        Console.ReadLine()
        |> tryParseRetry
        |> function
            | Ok b -> dispatch <| Retry(Some b)
            | Error s ->
                printfn "%s" s
                dispatch <| Retry None
    | Winning ->
        printfn "CONGRATULATIONS! YOU WIN!!"
        dispatch (Retry None)
    | Losing ->
        printfn "Womp womp, you ran out of attempts"
        dispatch (Retry None)
    | Goodbye -> printfn "Goodbye!"

let MaxAttempts = PositiveInt.create 5 |> Option.get

[<EntryPoint>]
let main args =

    Program.mkSimple init update view |> Program.runWith (MaxAttempts)

    0
