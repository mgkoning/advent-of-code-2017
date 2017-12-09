data Mode = Group | Garbage | Cancel deriving Show

isCancel Cancel = True
isCancel _ = False

score :: String -> (Int, Int, Int, [Mode])
score input = foldl readForScore (1, 1, 0, [Group]) (tail input)
  where
    readForScore (score, level, garbageCount, modes) next =
      case head modes of
        Cancel -> (score, level, garbageCount, tail modes)
        Garbage -> case next of
          '!' -> (score, level, garbageCount, Cancel:modes)
          '>' -> (score, level, garbageCount, tail modes)
          _ -> (score, level, garbageCount+1, modes)
        Group -> case next of
          '<' -> (score, level, garbageCount, Garbage:modes)
          '}' -> (score, level - 1, garbageCount, tail modes)
          '{' -> let nextLevel = level + 1 in (score + nextLevel, nextLevel, garbageCount, Group:modes)
          _ -> (score, level, garbageCount, modes)

solve = do
  input <- readFile "input.txt"
  putStrLn $ show $ score input

testGroups1 = "{}"
testGroups2 = "{{{}}}"
testGroups3 = "{{},{}}"
testGroups4 = "{{{},{},{{}}}}"
testGroups5 = "{<a>,<a>,<a>,<a>}"
testGroups6 = "{{<ab>},{<ab>},{<ab>},{<ab>}}"
testGroups7 = "{{<!!>},{<!!>},{<!!>},{<!!>}}"
testGroups8 = "{{<a!>},{<a!>},{<a!>},{<ab>}}"
garbage1 = wrapInGroup "<>"
garbage2 = wrapInGroup "<random characters>"
garbage3 = wrapInGroup "<<<<>"
garbage4 = wrapInGroup "<{!>}>"
garbage5 = wrapInGroup "<!!>"
garbage6 = wrapInGroup "<!!!>>"
garbage7 = wrapInGroup "<{o\"i!a,<{i<a>"

wrapInGroup s = '{':(s ++ ['}'])