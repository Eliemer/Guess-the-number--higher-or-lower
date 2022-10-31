open System

let MAX_ATTEMPTS = 5

[<Literal>]
let WELCOME_MESSAGE =
    "Hello!
I'm gonna think of a number between 1 and 100 (inclusive).
You will write the number you think it is.
I will tell you if:
\t- The number you guessed is HIGHER, LOWER, than my secret number
\t- If you guess it right, I'll send you CONGRATULATIONS!"


[<EntryPoint>]
let main _ =

    let mutable newRound = true
    let rand = Random()

    while newRound do
        Console.Clear()
        printfn "%s" WELCOME_MESSAGE

        let mutable failed = false
        let mutable numOfAttempts = 0
        let mutable guess = -1
        let secretNumber = rand.Next(1, 101) // [1, 101)

        while guess <> secretNumber && not failed do
            printf "Write your guess:"
            let guessStr = Console.ReadLine()

            match Int32.TryParse guessStr with
            | false, _ -> printfn "Please submit a number"
            | true, n when n > 0 && n < 101 ->
                guess <- n
                numOfAttempts <- numOfAttempts + 1

                if numOfAttempts < MAX_ATTEMPTS then
                    if guess < secretNumber then
                        printfn "HIGHER"
                    else if guess > secretNumber then
                        printfn "LOWER"
                    else if guess = secretNumber then
                        printfn "CONGRATULATIONS you win!"

                else
                    printfn "Womp womp, you ran out of attempts"
                    failed <- true

            | true, _ -> printfn "Please submite a number between 1 and 100"

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
