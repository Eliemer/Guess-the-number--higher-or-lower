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

let guessingLoop (secret: int) : GameState =
    let rec loop attempts =
        function
        | Ongoing ->
            printf "Write your guess:"
            let guessStr = Console.ReadLine()

            match Int32.TryParse guessStr with
            | false, _ ->
                printfn "Please submit a number"
                loop attempts Ongoing

            | true, guess when guess > 0 && guess < 101 ->
                if attempts < MAX_ATTEMPTS then

                    if guess < secret then
                        printfn "TOO LOW"
                        loop (attempts + 1) Ongoing
                    else if guess > secret then
                        printfn "TOO HIGH"
                        loop (attempts + 1) Ongoing
                    else if guess = secret then
                        loop attempts Success
                    else
                        Ongoing

                else
                    loop attempts Failure

            | true, _ ->
                printfn "Please submite a number between 1 and 100"
                loop attempts Ongoing

        | state -> state

    loop 0 Ongoing

let newRoundLoop =

    let rec loop res valid : bool =
        if not valid then
            printfn "\n\nWould you like to play another round? (1: yes, 0: no)"
            let newRoundStr = Console.ReadLine()

            match Int32.TryParse newRoundStr with
            | false, _ ->
                printfn "Not a valid response"
                loop false false
            | true, n when n = 0 || n = 1 -> loop (n % 2 = 1) true
            | true, n ->
                printfn "Submit a number that is either 0 or 1"
                loop false false
        else
            res

    loop false

let mainLoop (rand: System.Random) =
    let rec loop newRound =
        if newRound then
            Console.Clear()
            printfn "%s" WELCOME_MESSAGE

            match guessingLoop (rand.Next(1, 101)) with
            | Success -> printfn "CONGRATULATIONS you win!"
            | Failure -> printfn "Womp womp, you ran out of attempts"
            | Ongoing -> failwith "Incorrect game state: Ongoing!"

            loop (newRoundLoop false)
        else
            printfn "Thank you for playing!"

    loop true

[<EntryPoint>]
let main _ =
    let rand = Random()
    mainLoop rand
    0
