module BlockGame where

import Data.List
import System.IO
import Data.Maybe

level1 = ["==#==",
          "=---=",
          "=-O-=",
          "=-!-=",
          "====="]
level2 = ["===#===",
          "=---!-=",
          "=-@O@-=",
          "=-!---=",
          "======="]
level3 = ["==#==",
          "=O@-=",
          "=-@!=",
          "=-@-=",
          "====="]
level4 = ["===#===",
          "=-----=",
          "=-O---=",
          "=@-@-@=",
          "=-@-@-=",
          "=@-!-@=",
          "======="]
level5 = ["===#===",
          "=-@-@-=",
          "=@--O@=",
          "=!@--@=",
          "=-@@--=",
          "=-@-@-=",
          "======="]
level6 = ["====#====",
          "=!!!!!!!=",
          "=!-----!=",
          "=!-@@@-!=",
          "=!-@O@-!=",
          "=!-@@@-!=",
          "=!-----!=",
          "=!!!!!!!=",
          "========="]
victory = ["  Level", 
           "Complete!", 
           "   ___", 
           "  /n n\\",
          "  \\ u /", 
           "   ---"]

main = help
info = help
help = putStr "start levelN: Play the Nth level (currently there are six levels)\n\
              \startBig levelN: Play the Nth level with each game element represented by a 2x2 block of characters\n\
              \manual: Display the manual for the game\n\n"

man = manual
manual = putStr "\
\Welcome to Block Game!\n\
\Move with WASD.\n\
\Quit with CTRL+C.\n\
\O: You\n\
\#: Door (the goal)\n\
\!: Keys (collect all keys to unlock the door)\n\
\@: Blocks (you can push them around)\n\
\-: Open space\n\
\=: Walls\n\n" 

start state = do
 display state
 continue state

continue state = do
 c <- getChar
 display (move c state)
 continue (move c state)
 display (move c state)

startBig state = do
 displayBig state
 continueBig state

continueBig state = do
 c <- getChar
 displayBig (move c state)
 continueBig (move c state)
 display (move c state)

move m state =
 if (m == 'a')
  then (moveLeft state)
  else if (m == 'd')
        then (moveRight state)
        else if (m=='w') 
              then (moveUp state)
              else if(m=='s')
                    then (moveDown state)
                    else if(m=='q')
                          then state
                          else state

moveLeftRow row = 
 if ("-O" `isInfixOf` row  || "!O" `isInfixOf` row) 
  then init(takeWhile(/= 'O') (row)) ++ "O-" ++ tail(dropWhile(/= 'O') (row));
  else if ("-@O" `isInfixOf` row)
        then init(init(takeWhile(/= 'O') row)) ++ "@O-" ++ tail(dropWhile(/= 'O') (row));
        else row

moveLeft state = map moveLeftRow state

moveRightRow row = 
 if ("O-" `isInfixOf` row || "O!" `isInfixOf` row) 
  then (takeWhile(/= 'O') (row)) ++ "-O" ++ tail(tail(dropWhile(/= 'O') (row)));
  else if ("O@-" `isInfixOf` row)
        then (takeWhile(/= 'O') (row)) ++ "-O@" ++ tail(tail(tail(dropWhile(/= 'O') (row))));
        else row

moveRight state = map moveRightRow state

moveUp state = 
 if (above 1 state == '-' || above 1 state == '!')
  then init(takeWhile (notElem 'O') state) ++ [enterRow state (aboveRow 1 state)] ++ [clearRow(head(dropWhile (notElem 'O') state))] ++ tail(dropWhile (notElem 'O') state)
  else if (above 1 state == '@' && above 2 state == '-')
        then init(init(takeWhile (notElem 'O') state)) ++ [pushRow state (aboveRow 2 state)] ++ [enterRow state (aboveRow 1 state)] ++ [clearRow(head(dropWhile (notElem 'O') state))] ++ tail(dropWhile (notElem 'O') state)
        else if (above 1 state == '#' && not (keyCheck(state)))
              then victory
              else state

moveDown state = 
 if (below 1 state == '-' || below 1 state == '!')
  then takeWhile (notElem 'O') state ++ [clearRow(head(dropWhile (notElem 'O') state))] ++ [enterRow state (belowRow 1 state)] ++ tail(tail(dropWhile (notElem 'O') state))
  else if (below 1 state == '@' && below 2 state == '-')
        then takeWhile (notElem 'O') state ++ [clearRow(head(dropWhile (notElem 'O') state))] ++ [enterRow state (belowRow 1 state)] ++ [pushRow state (belowRow 2 state)] ++ tail(tail(tail(dropWhile (notElem 'O') state)))
        else state 

enterRow state row = take (findX state) row ++ "O" ++ drop (findX state + 1) row 

clearRow row =
 if ("O" `isInfixOf` row) 
  then (takeWhile(/= 'O') (row)) ++ "-" ++ (tail(dropWhile(/= 'O') (row)));
  else row

pushRow state row = take (findX state) row ++ "@" ++ drop (findX state + 1) row 

above n state = last(take (findX state + 1) (aboveRow n state))
aboveRow n state = last (take (findY state - n + 1) state)

below n state = last(take (findX state + 1) (belowRow n state))
belowRow n state = last (take (findY state + n + 1) state)

findX :: [[Char]] -> Int
findX state = minimum (map length (map (takeWhile(/= 'O')) state))

findY :: [[Char]] -> Int
findY state = fromJust (elemIndex (findX state) (map length (map (takeWhile(/= 'O')) state)))

keyCheck :: [[Char]] -> Bool
keyCheck state = any (==True) (map (elem '!') state)

completeCheck :: [[Char]] -> Bool
completeCheck state = elem "Complete!" state

display :: [[Char]] -> IO()
display state = putStr("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" ++ concat (map (++"\n") state))

duplicateStr :: [Char] -> [Char]
duplicateStr s = s ++ "\n" ++ s

duplicateChar :: Char -> [Char]
duplicateChar c = [c, c]

duplicateChars :: [Char] -> [Char]
duplicateChars cs = concat (map (duplicateChar) cs)

displayBig :: [[Char]] -> IO()
displayBig state = if (completeCheck state)
                    then display state
                    else display (map duplicateStr (map duplicateChars state))
