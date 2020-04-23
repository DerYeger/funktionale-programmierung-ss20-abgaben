main = interact parseFile

parseFile input =
    let
        fileLines = lines input
        charCount = show $ length input
        wordCount = show . length . words $ input
        secondLine = if (length fileLines >= 2) then fileLines!!1 else ""
        secondLineLength = show . length $ secondLine
    in charCount ++ " characters\n" ++ wordCount ++ " words\nSecond line: " ++ secondLine ++ "\n" ++ secondLineLength ++ " characters on the second line\n"
