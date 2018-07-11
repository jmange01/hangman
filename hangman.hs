main = do  
    putStrLn "Let's play!"
    putStrLn "Enter your word:"
    word <- getLine
    playGame word []

playGame word guesses = if gameEnd word guesses
                        then do
                            putStrLn "Game Over "
                            putStrLn (sprites !! 6)
                        else do
                            putStrLn "Enter guess "
                            c <- getChar
                            putStrLn (hideLetters word (c:guesses))
                            putStrLn ("guesses: " ++ (c:guesses))
                            putStrLn (sprites !! (countErrors word guesses))
                            playGame word (filter' (c:guesses))
                        where 
                          sprites = [['\n'],[' ',' ',' ',' ',' ','_','\n',' ',' ',' ',' ','(',' ',')','\n'],[' ',' ',' ',' ',' ','_','\n',' ',' ',' ',' ','(',' ',')','\n',' ',' ',' ',' ',' ','|',' ','\n',' ',' ',' ',' ',' ','|',' ','\n'],[' ',' ',' ',' ',' ','_','\n',' ',' ',' ',' ','(',' ',')','\n',' ',' ',' ',' ','\\','|',' ','\n',' ',' ',' ',' ',' ','|',' ','\n'],[' ',' ',' ',' ',' ','_','\n',' ',' ',' ',' ','(',' ',')','\n',' ',' ',' ',' ','\\','|','/','\n',' ',' ',' ',' ',' ','|',' ','\n'],[' ',' ',' ',' ',' ','_','\n',' ',' ',' ',' ','(',' ',')','\n',' ',' ',' ',' ','\\','|','/','\n',' ',' ',' ',' ',' ','|',' ','\n',' ',' ',' ',' ','/','\n'],[' ',' ',' ',' ',' ','_','\n',' ',' ',' ',' ','(',' ',')','\n',' ',' ',' ',' ','\\','|','/','\n',' ',' ',' ',' ',' ','|',' ','\n',' ',' ',' ',' ','/',' ','\\','\n']]

gameEnd word guesses = (isWin word guesses) || (isLoss word guesses)

isWin word guesses = (countCorrect word guesses) == (length word)

isLoss word guesses = (countErrors word guesses) >= (length sprites) - 1

hideLetters word except = map (repl except) word
    where
        repl r c = if c `elem` r then c else '_'

filter' xs = [ x | x <- xs, not (x `elem` "\n_")]

countCorrect word guesses = foldl (\acc x -> if x `elem` guesses then acc + 1 else acc) 0 word

countErrors word guesses = foldl (\acc x -> if x `elem` word then acc else acc + 1) 0 guesses
